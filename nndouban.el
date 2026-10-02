;;; nndouban.el --- Douban timelines and reply inbox for Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gnus-thread-reader "0.1.0") (firefox-cookies "0.1.0") (plz "0.9.1"))
;; Keywords: news, comm
;; Pure HTML/status parsers adapted from Dzming Li's elfeed-adapters-douban.el.
;; No Elfeed runtime dependency.

;;; Commentary:

;; Subscription types: timeline.USER, replies.ACCOUNT, group.ID and topic.ID.  The normal
;; Gnus overview contains one article per discussion.  Replies are cached as
;; real messages with stable numbers and References, exposed by request-thread.
;; Fetching is asynchronous; Gnus article reads use the local snapshot only.

;;; Code:
(require 'thread-reader-douban)
(require 'seq)
(require 'url)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-start)
(require 'nnoo)
(require 'nnheader)
(require 'rfc2047)
(require 'mail-parse)
(require 'message)
(require 'gnus-thread-reader)

(defgroup nndouban nil "Douban in Gnus." :group 'gnus)

(defcustom nndouban-timeline-excluded-title-fragments nil
  "Alist of (USER-ID . FRAGMENTS) excluded from that user's timeline.
Each fragment is matched literally within the discussion title."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'nndouban)

(defun nndouban--timeline-include-p (user discussion)
  "Whether USER's DISCUSSION passes its configured title exclusions."
  (let ((title (or (thread-reader-discussion-title discussion) "")))
    (not (cl-some (lambda (fragment)
                    (and (stringp fragment)
                         (not (string-empty-p fragment))
                         (string-match-p (regexp-quote fragment) title)))
                  (alist-get user nndouban-timeline-excluded-title-fragments
                             nil nil #'equal)))))

(defcustom nndouban-source-page-size 20
  "Items requested per Douban page."
  :type 'natnum :group 'nndouban)

(defun nndouban-source--group-id (id)
  "Validate numeric Douban group ID."
  (unless (and (stringp id) (string-match-p "\\`[1-9][0-9]*\\'" id))
    (error "Expected a numeric Douban group ID"))
  id)

(defun nndouban-source--group-page (url callback)
  "Read group or group topic URL and call CALLBACK with (HTML ERROR).
The desktop group page can reject curl even when Emacs's URL transport works.
Do not follow redirects with Cookie headers."
  (unless (or (thread-reader-douban--group-topic-p url)
              (string-match-p
               "\\`https://www\\.douban\\.com/group/[1-9][0-9]*/\\'" url))
    (error "Unsupported Douban group URL"))
  (condition-case nil
      (let* ((cookies (when (thread-reader-douban--group-topic-p url)
                        (funcall thread-reader-douban-cookie-function url)))
             (url-request-method "GET")
             (url-max-redirections 0)
             (url-request-extra-headers
              `(("User-Agent" . ,thread-reader-douban--user-agent)
                ("Accept" . "text/html")
                ("Referer" . ,url)
                ,@(when cookies
                    `(("Cookie" . ,(thread-reader-douban--cookie-header cookies)))))))
        (with-current-buffer
            (or (url-retrieve-synchronously url t t thread-reader-douban-timeout)
                (error "No Douban group topic response"))
          (unwind-protect
              (progn
                (goto-char (point-min))
                (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
                  (error "Douban group topic access denied"))
                (unless (re-search-forward "\r?\n\r?\n" nil t)
                  (error "Douban group topic response has no body"))
                (let ((body (buffer-substring-no-properties (point) (point-max))))
                  (funcall callback
                           (if (multibyte-string-p body) body
                             (decode-coding-string body 'utf-8)) nil)))
            (kill-buffer (current-buffer)))))
    (error (funcall callback nil "Douban group page access denied; check Firefox login"))))

(defun nndouban-source--group-time (value)
  "Normalize a group listing's last-reply VALUE to local date and time."
  (let ((value (string-trim value)))
    (cond
     ((string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" value)
      (concat value " 00:00:00"))
     ((string-match-p
       "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\'"
       value)
      (concat value ":00"))
     ((string-match-p "\\`[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\'" value)
      (let* ((year (format-time-string "%Y" (current-time) 28800))
             (candidate (concat year "-" value ":00")))
        (when (time-less-p (time-add (current-time) (days-to-time 1))
                           (date-to-time (concat candidate " +0800")))
          (setq candidate (concat (number-to-string (1- (string-to-number year)))
                                  "-" value ":00")))
        candidate))
     (t (error "Unknown Douban group listing time: %s" value)))))

(defun nndouban-source--group-discussions (html)
  "Parse HTML group listing into lightweight topic discussions."
  (let* ((document (with-temp-buffer
                     (insert html)
                     (libxml-parse-html-region (point-min) (point-max))))
         (table (car (dom-by-class document "\\bolt\\b")))
         discussions)
    (unless table (error "Douban group listing has no topic table"))
    (dolist (row (dom-by-tag table 'tr))
      (when-let* ((title-cell (car (dom-by-class row "\\btitle\\b")))
                  (anchor (car (dom-by-tag title-cell 'a)))
                  (href (dom-attr anchor 'href))
                  ((string-match
                    "\\`https://www\\.douban\\.com/group/topic/\\([1-9][0-9]*\\)/" href)))
        (let* ((id (match-string 1 href))
               (url (format "https://www.douban.com/group/topic/%s/" id))
               (title (string-trim (or (dom-attr anchor 'title)
                                       (dom-texts anchor))))
               (author-link (car (dom-by-tag (nth 1 (dom-by-tag row 'td)) 'a)))
               (author (if author-link (string-trim (dom-texts author-link)) "豆瓣用户"))
               (author-url (and author-link (dom-attr author-link 'href)))
               (author-id (and author-url
                               (string-match "/people/\\([0-9]+\\)/" author-url)
                               (match-string 1 author-url)))
               (time-cell (car (dom-by-class row "\\btime\\b")))
               (time (and time-cell (nndouban-source--group-time
                                     (dom-texts time-cell)))))
          (when (and (not (string-empty-p title)) time)
            (push (make-thread-reader-discussion
                   :id id :url url :title title :cursor '(:kind comments :start 0)
                   :entries (list (make-nndouban-source-entry
                                   :id (concat "topic:" id) :author author
                                   :author-id author-id :time time :url url
                                   :body-format 'plain :body "打开帖子以加载正文和回复。"
                                   :placeholder-p t)))
                  discussions)))))
    (unless discussions (error "Douban group listing has no topics"))
    (nreverse discussions)))

(defun nndouban-source-group (id callback)
  "Fetch the latest topics in Douban group ID; call CALLBACK with (ITEMS ERROR)."
  (let ((url (format "https://www.douban.com/group/%s/"
                     (nndouban-source--group-id id))))
    (nndouban-source--group-page
     url
     (lambda (html error)
       (if error (funcall callback nil error)
         (condition-case problem
             (funcall callback (nndouban-source--group-discussions html) nil)
           (error (funcall callback nil (error-message-string problem)))))))))

(defun nndouban-source--group-csrf (group-id)
  "Return a current CSRF cookie for GROUP-ID without submitting a post."
  (let* ((page (format "https://www.douban.com/group/%s/new_topic"
                       (nndouban-source--group-id group-id)))
         (cookies (funcall thread-reader-douban-cookie-function page))
         (stored (cdr (assoc "ck" cookies))))
    (or stored
        (let ((url-request-method "GET")
              (url-request-extra-headers
               `(("Cookie" . ,(thread-reader-douban--cookie-header cookies))
                 ("User-Agent" . ,thread-reader-douban--user-agent))))
          (with-current-buffer
              (or (url-retrieve-synchronously page t t thread-reader-douban-timeout)
                  (error "Could not open the Douban group editor"))
            (unwind-protect
                (save-excursion
                  (goto-char (point-min))
                  (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
                    (error "Douban group editor is unavailable"))
                  (let ((case-fold-search t))
                    (when (re-search-forward
                           "^Set-Cookie: ck=\\([^;\r\n]+\\)" nil t)
                      (match-string 1))))
              (kill-buffer (current-buffer))))))))

(defun nndouban-source--draft-content (body)
  "Encode plain BODY as minimal Draft.js block data."
  (json-serialize
   (list :blocks
         (vconcat
          (cl-loop for line in (split-string body "\n" nil)
                   for index from 1
                   collect (list :key (format "b%04d" index)
                                 :text line :type "unstyled" :depth 0
                                 :inlineStyleRanges [] :entityRanges []
                                 :data (make-hash-table :test #'equal))))
         :entityMap (make-hash-table :test #'equal))))

(defun nndouban-source-publish-group-topic (group-id title body callback)
  "Publish TITLE and BODY to GROUP-ID; call CALLBACK with (URL ERROR).
Use Douban's JSON topic endpoint.  Accept success only when it returns a
canonical group topic URL."
  (nndouban-source--group-id group-id)
  (unless (and (stringp title) (not (string-empty-p (string-trim title)))
               (stringp body) (not (string-empty-p (string-trim body))))
    (error "Douban group title and body must be nonempty"))
  (let* ((page (format "https://www.douban.com/group/%s/new_topic" group-id))
         (endpoint "https://m.douban.com/rexxar/api/v2/topic/post")
         (cookies (funcall thread-reader-douban-cookie-function endpoint))
         (ck (or (cdr (assoc "ck" cookies))
                 (nndouban-source--group-csrf group-id)))
         (payload (json-serialize
                   (list :group_id group-id :title title
                         :content (nndouban-source--draft-content body)))))
    (unless (and (assoc "dbcl2" cookies) ck)
      (error "Log in to Douban in Firefox and open the group editor"))
    (unless (assoc "ck" cookies) (push (cons "ck" ck) cookies))
    (let ((plz-curl-default-args
           '("--disable" "--silent" "--show-error" "--compressed")))
      (plz 'post endpoint
        :headers `(("User-Agent" . ,thread-reader-douban--user-agent)
                   ("Referer" . ,page)
                   ("Origin" . "https://www.douban.com")
                   ("Content-Type" . "application/json; charset=utf-8")
                   ("X-CSRF-TOKEN" . ,ck)
                   ("Cookie" . ,(thread-reader-douban--cookie-header cookies)))
        :body (encode-coding-string payload 'utf-8) :body-type 'binary
        :as 'response :connect-timeout 10 :timeout thread-reader-douban-timeout
        :then (lambda (response)
                (let (url failure)
                  (condition-case nil
                      (let* ((json (json-parse-string
                                    (plz-response-body response)
                                    :object-type 'plist :array-type 'list))
                             (candidate (plist-get json :url)))
                        (when (thread-reader-douban--group-topic-p candidate)
                          (setq url candidate)))
                    (error nil))
                  (unless url
                    (setq failure (make-thread-reader-send-error
                                   :message "Douban did not confirm a group topic URL; check the website"
                                   :uncertain t)))
                  (funcall callback url failure)))
        :else (lambda (failure)
                (funcall callback nil
                         (thread-reader-douban--http-error failure 'post)))))))

(defun nndouban-source--account ()
  "Return the explicitly selected Firefox session's Douban account ID."
  (let* ((cookies (funcall thread-reader-douban-cookie-function
                          "https://www.douban.com/reply_notify/"))
         (login (cdr (assoc "dbcl2" cookies))))
    (unless (and login
                 (string-match "\\`\"?\\([0-9]+\\):" login))
      (user-error "Log in to Douban in the configured Firefox profile"))
    (match-string 1 login)))

(defun nndouban-source--status-id (url)
  "Return the numeric broadcast ID in an allowed Douban URL."
  (when (and (stringp url)
             (string-match
              "\\`https://\\(?:www\\.douban\\.com/people/[[:alnum:]_-]+/status/\\|m\\.douban\\.com/status/\\)\\([0-9]+\\)/?\\(?:[?#][^\r\n]*\\)?\\'" url))
    (match-string 1 url)))

(defun nndouban-source--allowed (url method)
  "Check URL against the exact endpoints allowed for METHOD."
  (and (stringp url)
       (not (string-match-p "[\r\n]" url))
       (if (eq method 'post)
           (string-match-p "\\`https://m\\.douban\\.com/rexxar/api/v2/status/[0-9]+/create_comment\\'" url)
         (or (nndouban-source--status-id url)
             (thread-reader-douban--topic-id url)
             (string-match-p
              (concat "\\`https://\\(?:www\\.douban\\.com/\\(?:reply_notify/\\(?:[?]start=[0-9]+\\)?"
                      "\\|notification/reply_notify[?]id=[0-9]+\\)"
                      "\\|m\\.douban\\.com/rexxar/api/v2/status/"
                      "\\(?:user_timeline/[0-9]+\\|[0-9]+\\(?:/comments\\)?"
                      "\\|comment/[0-9]+/replies\\)\\)\\'") url)))))

(defun nndouban-source--request (method url source params callback &optional redirects)
  "Request an allowed URL; CALLBACK receives (BODY ERROR).
Cookies are selected separately for every destination.  Only approved GET
redirects are followed; never resend a POST after a redirect."
  (let (done dispatched)
    (cl-labels ((finish (body error)
                  (unless done (setq done t) (funcall callback body error))))
      (condition-case problem
          (progn
            (unless (and (memq method '(get post))
                         (nndouban-source--allowed url method)
                         (nndouban-source--allowed source 'get))
              (error "Unsupported Douban destination"))
            (let* ((cookies (funcall thread-reader-douban-cookie-function url))
                   (cookie (thread-reader-douban--cookie-header cookies))
                   (ck (cdr (assoc "ck" cookies)))
                   (query (url-build-query-string
                           (append params (when (eq method 'post) `(("ck" ,ck))))))
                   (plz-curl-default-args '("--disable" "--silent" "--show-error" "--compressed")))
              (when (and (eq method 'post) (not (and ck (assoc "dbcl2" cookies))))
                (error "No Douban login for this request"))
              (setq dispatched t)
              (plz method (if (and (eq method 'get) params) (concat url "?" query) url)
                :headers `(("User-Agent" . ,thread-reader-douban--user-agent)
                           ("Referer" . ,source)
                           ("Cookie" . ,cookie)
                           ,@(when (eq method 'post)
                               `(("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8")
                                 ("Origin" . "https://www.douban.com")
                                 ("X-Requested-With" . "XMLHttpRequest")
                                 ("X-CSRF-TOKEN" . ,ck))))
                :body (when (eq method 'post) (encode-coding-string query 'utf-8))
                :body-type 'binary :as 'response :connect-timeout 10
                :timeout thread-reader-douban-timeout
                :then
                (lambda (response)
                  (let* ((status (plz-response-status response))
                         (location (cdr (assq 'location (plz-response-headers response))))
                         (target (and location (url-expand-file-name location url))))
                    (cond
                     ((<= 200 status 299) (finish (plz-response-body response) nil))
                     ((and (eq method 'get) (memq status '(301 302 303 307 308))
                           (< (or redirects 0) 3) (nndouban-source--allowed target 'get))
                      (nndouban-source--request 'get target source nil #'finish (1+ (or redirects 0))))
                     (t (finish nil (thread-reader-douban--failure
                                     "Douban requires browser login or redirected outside the supported pages"
                                     method (eq method 'post)))))))
                :else (lambda (error) (finish nil (thread-reader-douban--http-error error method))))))
        (error (finish nil (thread-reader-douban--failure
                            (if dispatched "Douban transport failed" (error-message-string problem))
                            method dispatched)))))))

(defun nndouban-source--json (method url source params callback)
  "Request and validate Douban JSON, calling CALLBACK with (PAYLOAD ERROR)."
  (nndouban-source--request
   method url source params
   (lambda (text error)
     (if error (funcall callback nil error)
       (let (payload failure)
         (condition-case nil
             (progn
               (setq payload (json-parse-string text :object-type 'plist :array-type 'list
                                               :null-object nil :false-object :false))
               (when (or (and (plist-get payload :code) (not (equal (plist-get payload :code) 0)))
                         (plist-get payload :localized_message))
                 (setq failure (thread-reader-douban--failure
                                (or (plist-get payload :localized_message) "Douban rejected the request") method))))
           (error (setq failure (thread-reader-douban--failure
                                 "Invalid response from Douban" method (eq method 'post)))))
         (funcall callback payload failure))))))

(defun nndouban-source--image-html (image)
  "Render IMAGE plist as HTML."
  (when-let* ((large (plist-get image :large))
              (url (plist-get large :url)))
    (format "<p><img src=\"%s\"></p>" (xml-escape-string url))))

(defun nndouban-source--text-html (text)
  "Escape Douban plain TEXT and preserve its source line breaks."
  (replace-regexp-in-string
   "\n" "<br>"
   (xml-escape-string
    (replace-regexp-in-string "\r\n?" "\n" (or text "")))
   t t))

(defun nndouban-source--topic-url (status)
  "Return the personal-topic web URL linked by STATUS, if any."
  (let* ((card (plist-get status :card))
         (url (plist-get card :url)))
    (when (and (equal (plist-get card :type) "topic")
               (stringp url)
               (string-match-p
                (rx string-start "https://www.douban.com/topic/" (+ digit) "/")
                url))
      url)))

(defun nndouban-source--review-url (status)
  "Return the public review URL linked by STATUS, if any."
  (let* ((card (plist-get status :card))
         (url (plist-get card :url)))
    (when (and (equal (plist-get card :type) "review")
               (stringp url)
               (string-match-p
                (rx string-start "https://"
                    (or "book" "movie") ".douban.com/review/"
                    (+ digit) "/" string-end)
                url))
      url)))

(defun nndouban-source--reply-notifications (html)
  "Parse reply-notification descriptors from authenticated Douban HTML."
  (with-temp-buffer
    (insert html)
    (let* ((document (libxml-parse-html-region (point-min) (point-max)))
           (nodes (dom-by-class document "new-reply-item")))
      (delq
       nil
       (mapcar
        (lambda (node)
          (when-let* ((node-id (dom-attr node 'id))
                      ((string-match
                        (rx string-start "reply_notify_"
                            (group (+ digit)) string-end)
                        node-id))
                      (notification-id (match-string 1 node-id))
                      (content (car (dom-by-class node "content")))
                      (anchor (car (dom-by-tag content 'a)))
                      (href (dom-attr anchor 'href)))
            (list :notification-id notification-id
                  :resolver-url
                  (nndouban-source--absolute-url href))))
        nodes)))))

(defun nndouban-source--absolute-url (url)
  "Make a root-relative Douban URL URL absolute."
  (when url
    (if (string-prefix-p "/" url)
        (concat "https://www.douban.com" url)
      url)))

(defun nndouban-source--status-target (html)
  "Return (STATUS-ID SOURCE-URL) discovered in status page HTML."
  (when (string-match
         (rx (group "https://www.douban.com/people/" (+ digit) "/status/"
                    (group (+ digit))))
         html)
    (list (match-string 2 html) (match-string 1 html))))

(defun nndouban-source--all-comments (comments)
  "Flatten Douban COMMENTS and their nested replies."
  (cl-mapcan
   (lambda (comment)
     (cons comment
           (nndouban-source--all-comments
            (or (plist-get comment :replies) nil))))
   comments))

(defun nndouban-source--direct-replies (payload user-id &optional root-author-id)
  "Return comments in PAYLOAD that directly reply to USER-ID."
  (seq-filter
   (lambda (comment)
     (let* ((author (plist-get comment :author))
            (ref-comment (plist-get comment :ref_comment))
            (ref-author (plist-get ref-comment :author)))
       (and (not (eq (plist-get comment :is_deleted) t))
            (or (equal (format "%s" (plist-get ref-author :id)) user-id)
                (and (null ref-comment) (equal root-author-id user-id)))
            (not (equal (format "%s" (plist-get author :id)) user-id)))))
   (nndouban-source--all-comments
    (or (plist-get payload :comments) nil))))

(defun nndouban-source--extract-review-html (html)
  "Extract the full public review body from Douban HTML."
  (with-temp-buffer
    (insert html)
    (let* ((document (libxml-parse-html-region (point-min) (point-max)))
           (content (car (dom-by-class document "review-content"))))
      (when content
        (with-temp-buffer
          (dom-print content)
          (buffer-string))))))

(defun nndouban-source--status-html (status)
  "Render a Douban STATUS plist as compact HTML."
  (let ((text (or (plist-get status :text) ""))
        (images (plist-get status :images))
        (card (plist-get status :card))
        (reshared (or (plist-get status :reshared_status)
                      (plist-get status :parent_status))))
    (concat
     (format "<p>%s</p>" (nndouban-source--text-html text))
     (mapconcat #'nndouban-source--image-html images "")
     (when card
       (let ((title (plist-get card :title))
             (url (plist-get card :url))
             (subtitle (plist-get card :subtitle))
             (full-html (plist-get card :full-html)))
         (concat
          "<blockquote>"
          (when (and title (not (string-empty-p title)))
            (if url
                (format "<p><a href=\"%s\"><strong>%s</strong></a></p>"
                        (xml-escape-string url) (xml-escape-string title))
              (format "<p><strong>%s</strong></p>"
                      (xml-escape-string title))))
          (if full-html
              full-html
            (when subtitle
              (format "<p>%s</p>"
                      (nndouban-source--text-html subtitle))))
          (when-let* ((image (plist-get card :image)))
            (nndouban-source--image-html image))
          "</blockquote>")))
     (when reshared
       (format "<blockquote>%s</blockquote>"
               (nndouban-source--status-html reshared))))))

(defun nndouban-source--status-title (status)
  "Return a useful title for Douban STATUS."
  (let* ((author (plist-get status :author))
         (name (or (plist-get author :name) "豆瓣用户"))
         (activity (or (plist-get status :activity) ""))
         (card (plist-get status :card))
         (text (string-trim (replace-regexp-in-string
                             "[\n\r]+" " "
                             (or (plist-get status :text) "")))))
    (string-trim
     (format "%s %s: %s%s"
             name activity
             (if-let* ((card-title (plist-get card :title))
                       ((not (string-empty-p card-title))))
                 (format "《%s》" card-title)
               "")
             text))))

(cl-defstruct (nndouban-source-backend (:include thread-reader-douban-backend))
  account direct-ids root-author-id)

(cl-defstruct (nndouban-source-entry (:include thread-reader-entry))
  author-id)

(defun nndouban-source--user-id (value)
  "Return numeric author VALUE as a string, or nil if unknown."
  (let ((id (format "%s" value)))
    (when (string-match-p "\\`[0-9]+\\'" id) id)))

(defun nndouban-source--with-author (entry user-id)
  "Copy ENTRY with its numeric author USER-ID."
  (make-nndouban-source-entry
   :id (thread-reader-entry-id entry) :parent-id (thread-reader-entry-parent-id entry)
   :author (thread-reader-entry-author entry) :author-id (nndouban-source--user-id user-id)
   :body (thread-reader-entry-body entry) :body-format (thread-reader-entry-body-format entry)
   :url (thread-reader-entry-url entry) :time (thread-reader-entry-time entry)
   :children-cursor (thread-reader-entry-children-cursor entry)
   :placeholder-p (thread-reader-entry-placeholder-p entry)))

(defun nndouban-source--comment-authors (comments)
  "Index author IDs from COMMENTS, nested replies and referenced ancestors."
  (let ((authors (make-hash-table :test #'equal))
        (stack (copy-sequence comments)))
    (while stack
      (let* ((comment (pop stack))
             (id (nndouban-source--user-id (plist-get comment :id)))
             (author (nndouban-source--user-id (plist-get (plist-get comment :author) :id))))
        (when (and id author) (puthash (concat "comment:" id) author authors))
        (setq stack (append (plist-get comment :replies) stack))
        (when-let* ((ref (plist-get comment :ref_comment))) (push ref stack))))
    authors))

(defun nndouban-source--status-discussion (status &optional fallback-user)
  "Normalize STATUS to a discussion with its root and comments cursor."
  (let* ((id (thread-reader-douban--remote-id (plist-get status :id)))
         (author (plist-get status :author))
         (user (or (plist-get author :id) fallback-user))
         (url (or (and (nndouban-source--status-id (plist-get status :sharing_url))
                       (plist-get status :sharing_url))
                  (if user (format "https://www.douban.com/people/%s/status/%s/" user id)
                    (format "https://m.douban.com/status/%s/" id)))))
    (make-thread-reader-discussion
     :id id :url url :title (nndouban-source--status-title status)
     :cursor '(:kind comments :start 0)
     :entries (list (make-nndouban-source-entry
                     :id (concat "status:" id) :author (or (plist-get author :name) "豆瓣用户")
                     :author-id (nndouban-source--user-id (plist-get author :id))
                     :time (plist-get status :create_time) :url url
                     :body-format 'html :body (nndouban-source--status-html status))))))

(cl-defmethod thread-reader-backend-open ((backend nndouban-source-backend) url callback)
  (if (thread-reader-douban--topic-id url)
      (funcall (if (thread-reader-douban--group-topic-p url)
                   #'nndouban-source--group-page
                 (lambda (source done)
                   (thread-reader-douban--request 'get source source nil done)))
               url
       (lambda (html error)
         (if error (funcall callback nil error)
           (let (discussion failure)
             (condition-case problem
                 (let* ((document (with-temp-buffer
                                    (insert html)
                                    (libxml-parse-html-region (point-min) (point-max))))
                        (meta (or (car (dom-by-class document "\\barticle-meta\\b"))
                                  (car (dom-by-class document "\\btopic-doc\\b"))))
                        (author (or (car (dom-by-class meta "\\bauthor-name\\b"))
                                    (car (dom-by-class meta "\\bfrom\\b"))))
                        (href (or (dom-attr author 'href)
                                  (dom-attr (car (dom-by-tag author 'a)) 'href))))
                   (when (and href (string-match "/people/\\([0-9]+\\)/" href))
                     (setf (nndouban-source-backend-root-author-id backend) (match-string 1 href)))
                   (setq discussion (thread-reader-douban--html html url))
                   (setf (thread-reader-discussion-entries discussion)
                         (mapcar (lambda (entry)
                                   (nndouban-source--with-author
                                    entry (nndouban-source-backend-root-author-id backend)))
                                 (thread-reader-discussion-entries discussion))))
               (error (setq failure (error-message-string problem))))
             (funcall callback discussion failure)))))
    (let ((id (nndouban-source--status-id url)))
      (unless id (error "Unsupported Douban discussion URL"))
      (nndouban-source--json
       'get (concat "https://m.douban.com/rexxar/api/v2/status/" id) url nil
       (lambda (payload error)
         (if error (funcall callback nil error)
           (let (discussion failure)
             (condition-case problem
                 (progn
                   (setq discussion (nndouban-source--status-discussion
                                     (or (plist-get payload :status) payload)))
                   (setf (nndouban-source-backend-root-author-id backend)
                         (format "%s" (plist-get (plist-get (or (plist-get payload :status) payload) :author) :id)))
                   (unless (equal id (thread-reader-discussion-id discussion))
                     (error "Douban returned another broadcast")))
               (error (setq failure (error-message-string problem))))
             (funcall callback discussion failure))))))))

(cl-defmethod thread-reader-backend-children
  ((backend nndouban-source-backend) discussion parent cursor callback)
  (let* ((source (thread-reader-discussion-url discussion))
         (topic (thread-reader-douban--topic-id source))
         (id (thread-reader-discussion-id discussion))
         (kind (plist-get cursor :kind))
         (start (plist-get cursor :start))
         (count (max 1 (min 100 nndouban-source-page-size)))
         (comment (plist-get cursor :comment-id))
         (base (if topic thread-reader-douban--api
                 "https://m.douban.com/rexxar/api/v2/status/"))
         (url (concat base (if (eq kind 'comments) (concat id "/comments")
                             (concat "comment/" (thread-reader-douban--remote-id comment) "/replies")))))
    (unless (and (integerp start) (>= start 0)
                 (or (and (eq kind 'comments) (null parent))
                     (and (eq kind 'replies) parent
                          (equal (thread-reader-entry-id parent) (concat "comment:" comment)))))
      (error "Invalid Douban comments cursor"))
    (funcall
     (if topic #'thread-reader-douban--json #'nndouban-source--json)
     'get url source `(("start" ,(number-to-string start)) ("count" ,(number-to-string count))
                      ("nested" "1") ("order_by" "time"))
     (lambda (payload error)
       (if error (funcall callback nil error)
         (let (page failure)
           (condition-case problem
               (let* ((field (if (eq kind 'comments) :comments :replies))
                      (comments (plist-get payload field))
                      (normalized (copy-sequence payload)))
                 (unless (plist-member payload field) (error "Missing Douban comments field"))
                 (unless topic
                   ;; Broadcasts use an empty string for a root comment's parent.
                   (dolist (item (nndouban-source--all-comments comments))
                     (when (equal (plist-get item :parent_comment_id) "")
                       (setf (plist-get item :parent_comment_id) nil))))
                 ;; Older broadcast endpoints omit start/count, unlike personal topics.
                 (unless (plist-member normalized :start) (setq normalized (plist-put normalized :start start)))
                 (unless (plist-member normalized :count) (setq normalized (plist-put normalized :count count)))
                 (unless (plist-member normalized :total)
                   (error "Missing Douban comment total; cannot safely finish pagination"))
                 (let ((context (copy-thread-reader-discussion discussion)))
                   (unless topic
                     (setf (thread-reader-discussion-entries context)
                           (cons (make-thread-reader-entry :id (concat "topic:" id))
                                 (thread-reader-discussion-entries discussion))))
                   (setq page (thread-reader-douban--page normalized context cursor)))
                 (let ((authors (nndouban-source--comment-authors comments)))
                   (setf (thread-reader-page-entries page)
                         (mapcar (lambda (entry)
                                   (nndouban-source--with-author
                                    entry (gethash (thread-reader-entry-id entry) authors)))
                                 (thread-reader-page-entries page))))
                 (unless topic
                   (dolist (entry (thread-reader-page-entries page))
                     (when (equal (thread-reader-entry-parent-id entry) (concat "topic:" id))
                       (setf (thread-reader-entry-parent-id entry) (concat "status:" id)))))
                 (when (nndouban-source-backend-account backend)
                   (dolist (reply (nndouban-source--direct-replies
                                   (list :comments comments) (nndouban-source-backend-account backend)
                                   (nndouban-source-backend-root-author-id backend)))
                     (puthash (concat "comment:" (thread-reader-douban--remote-id (plist-get reply :id)))
                              t (nndouban-source-backend-direct-ids backend)))))
             (error (setq failure (error-message-string problem))))
           (funcall callback page failure)))))))

(cl-defmethod thread-reader-backend-reply
  ((_backend nndouban-source-backend) discussion parent body callback)
  (if (thread-reader-douban--topic-id (thread-reader-discussion-url discussion))
      (cl-call-next-method)
    (let* ((source (thread-reader-discussion-url discussion))
           (id (nndouban-source--status-id source))
           (ref (thread-reader-douban--comment-id (thread-reader-entry-id parent))))
      (unless (and id (or ref (equal (thread-reader-entry-id parent) (concat "status:" id))))
        (error "Invalid broadcast reply target"))
      (nndouban-source--json
       'post (format "https://m.douban.com/rexxar/api/v2/status/%s/create_comment" id) source
       `(("resp_type" "c_dict") ("text" ,body) ,@(when ref `(("ref_cid" ,ref))))
       (lambda (payload error)
         (if error (funcall callback nil error)
           (let (entry failure)
             (condition-case nil
                 (let* ((comment (or (plist-get payload :comment) (plist-get payload :data) payload))
                        (remote (thread-reader-douban--remote-id (plist-get comment :id))))
                   (setq entry (make-nndouban-source-entry
                                :id (concat "comment:" remote) :parent-id (thread-reader-entry-id parent)
                                :author-id (nndouban-source--user-id (plist-get (plist-get comment :author) :id))
                                :body (or (plist-get comment :text) body)
                                :author (or (plist-get (plist-get comment :author) :name) "我")
                                :time (plist-get comment :create_time)
                                :url (concat source "#comment_" remote))))
               (error (setq failure (make-thread-reader-send-error
                                    :message "Douban did not confirm a comment ID; check the website"
                                    :uncertain t))))
             (funcall callback entry failure))))))))

(defun nndouban-source-discussion (url callback &optional account)
  "Fetch all of URL's discussion sequentially, then call CALLBACK.
CALLBACK receives (DISCUSSION ERROR DIRECT-IDS).  ACCOUNT optionally filters
direct replies.  Incomplete fetches leave the backend's old cache intact."
  (let ((backend (make-nndouban-source-backend :name 'douban :account account
                                             :direct-ids (make-hash-table :test #'equal)))
        (buffer (generate-new-buffer " *nndouban fetch*"))
        (seen (make-hash-table :test #'equal)) done)
    (with-current-buffer buffer (thread-reader-mode))
    (cl-labels
        ((finish (discussion error)
           (unless done
             (setq done t)
             (when (buffer-live-p buffer) (kill-buffer buffer))
             (funcall callback discussion error (nndouban-source-backend-direct-ids backend))))
         (step ()
           (unless done
             (condition-case problem
                 (with-current-buffer buffer
                   (setf (thread-reader-discussion-entries thread-reader--discussion)
                         (mapcar (lambda (id) (gethash id thread-reader--entries)) thread-reader--order))
                   (if-let* ((next (thread-reader--next-page)))
                       (let* ((id (car next)) (parent (and id (gethash id thread-reader--entries)))
                              (cursor (if parent (thread-reader-entry-children-cursor parent)
                                        (thread-reader-discussion-cursor thread-reader--discussion)))
                              (key (list id cursor)))
                         (when (gethash key seen) (error "Douban repeated a pagination cursor"))
                         (puthash key t seen)
                         (thread-reader-backend-children
                          backend thread-reader--discussion parent cursor
                          (lambda (page error)
                            (if error (finish nil error)
                              (condition-case problem
                                  (with-current-buffer buffer
                                    (thread-reader--merge (thread-reader-page-entries page))
                                    (if id (setf (thread-reader-entry-children-cursor (gethash id thread-reader--entries))
                                                 (thread-reader-page-cursor page))
                                      (setf (thread-reader-discussion-cursor thread-reader--discussion)
                                            (thread-reader-page-cursor page)))
                                    (run-at-time 0 nil #'step))
                                (error (finish nil (error-message-string problem))))))))
                     (finish thread-reader--discussion nil)))
               (error (finish nil (error-message-string problem)))))))
      (condition-case problem
          (thread-reader-backend-open
           backend url
           (lambda (discussion error)
             (if error (finish nil error)
               (condition-case problem
                   (with-current-buffer buffer
                     (setq thread-reader--discussion discussion)
                     (thread-reader--merge (thread-reader-discussion-entries discussion) t)
                     (step))
                 (error (finish nil (error-message-string problem)))))))
        (error (finish nil (error-message-string problem)))))))

(defun nndouban-source-timeline (user callback)
  "Fetch the latest timeline page for USER; CALLBACK takes (DISCUSSIONS ERROR).
Older locally stored entries are retained by the Gnus backend."
  (thread-reader-douban--remote-id user)
  (let ((url (format "https://m.douban.com/rexxar/api/v2/status/user_timeline/%s" user)))
    (nndouban-source--json
     'get url url `(("start" "0") ("count" ,(number-to-string nndouban-source-page-size)))
     (lambda (payload error)
       (if error (funcall callback nil error)
         (let (items failure)
           (condition-case problem
               (progn
                 (unless (plist-member payload :items) (error "Missing timeline data"))
                 (dolist (wrapper (plist-get payload :items))
                   (unless (eq (plist-get wrapper :deleted) t)
                     (when-let* ((status (plist-get wrapper :status)))
                       (let ((topic (nndouban-source--topic-url status)))
                         (push (cons (nndouban-source--status-discussion status user) topic) items))))))
             (error (setq failure (error-message-string problem))))
           (if failure (funcall callback nil failure)
             (let (discussions)
               (cl-labels
                   ((next ()
                      (if (null items) (funcall callback (nreverse discussions) nil)
                        (pcase-let ((`(,discussion . ,topic) (pop items)))
                          (if (not topic) (progn (push discussion discussions) (next))
                            ;; A failed enrichment must retain the canonical topic ID.
                            (setf (thread-reader-discussion-id discussion) (thread-reader-douban--topic-id topic)
                                  (thread-reader-discussion-url discussion) topic
                                  (thread-reader-entry-id (car (thread-reader-discussion-entries discussion)))
                                  (concat "topic:" (thread-reader-douban--topic-id topic)))
                            (thread-reader-backend-open
                             (make-nndouban-source-backend :name 'douban) topic
                             (lambda (full _error)
                               (push (or full discussion) discussions)
                               (next))))))))
                 (next))))))))))

(defun nndouban-source-notifications (account callback)
  "Fetch direct replies to ACCOUNT; CALLBACK takes (RESULTS ERROR).
Each result is (DISCUSSION . DIRECT-IDS).  Keep unrelated comments as context
only when the reader explicitly opens the full discussion."
  (unless (equal account (nndouban-source--account))
    (user-error "Firefox is logged into another Douban account"))
  (let ((index "https://www.douban.com/reply_notify/"))
    (nndouban-source--request
     'get index index nil
     (lambda (html error)
       (if error (funcall callback nil error)
         (let ((notifications (nndouban-source--reply-notifications html)) results errors
               (seen (make-hash-table :test #'equal)))
           ;; A login/challenge page must not be mistaken for an empty inbox.
           (if (and (null notifications)
                    (not (string-match-p "reply_notify\\|回应我的\\|没有.*回应" html)))
               (funcall callback nil "Reply notification page unavailable; check Firefox login")
             (cl-labels
                 ((next ()
                    (if (null notifications)
                        (funcall callback (nreverse results)
                                 (when errors (format "%d notification discussions failed: %s"
                                                      (length errors) (car errors))))
                      (let ((url (plist-get (pop notifications) :resolver-url)))
                        (nndouban-source--request
                         'get url index nil
                         (lambda (page error)
                           (if error (progn (push error errors) (next))
                             (let* ((status (nndouban-source--status-target page))
                                    (topic (and (string-match "https://www\\.douban\\.com/topic/[0-9]+/" page)
                                                (match-string 0 page)))
                                    (source (or (cadr status) topic)))
                               (cond
                                ((not source) (push "Unsupported notification target" errors) (next))
                                ((gethash source seen) (next))
                                (t (puthash source t seen)
                                   (nndouban-source-discussion
                                    source
                                    (lambda (discussion error direct)
                                      (if error (push error errors)
                                        (push (cons discussion direct) results))
                                      (next)) account)))))))))))
               (next)))))))))


(nnoo-declare nndouban)
(defvoo nndouban-directory (expand-file-name "nndouban/" gnus-directory)
  "Directory for local article snapshots and stable number mappings.")
(defvoo nndouban--store nil)
(defvoo nndouban-status-string "")
(nnoo-define-basics nndouban)

(cl-defstruct nndouban--db file groups posts busy)
(defvar nndouban--stores (make-hash-table :test #'equal))
(defvar nndouban--server "douban")

(defun nndouban--load (file)
  "Read a FILE snapshot without evaluating code."
  (let ((store (make-nndouban--db :file file :busy (make-hash-table :test #'equal))))
    (when (file-exists-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (equal (plist-get data :version) 1) (error "Unsupported nndouban cache version"))
        (setf (nndouban--db-groups store) (plist-get data :groups)
              (nndouban--db-posts store) (plist-get data :posts))))
    store))

(defun nndouban--save (store)
  "Atomically save STORE; private article content is readable by its owner."
  (let* ((file (nndouban--db-file store))
         (directory (file-name-directory file))
         temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary (make-temp-file (expand-file-name ".nndouban-" directory)))
          (set-file-modes temporary #o600)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temporary
              (insert
               (json-serialize
                (list :version 1
                      :groups
                      (vconcat
                       (mapcar
                        (lambda (group)
                          (let ((copy (copy-sequence group)))
                            (setf (plist-get copy :entries)
                                  (vconcat
                                   (mapcar (lambda (entry)
                                             (let ((item (copy-sequence entry)))
                                               (setf (plist-get item :targets) (vconcat (plist-get item :targets))
                                                     (plist-get item :pending) (vconcat (plist-get item :pending)))
                                               item))
                                           (plist-get group :entries))))
                            copy)) (nndouban--db-groups store)))
                      :posts (vconcat (nndouban--db-posts store)))
                :null-object nil :false-object :false))))
          (rename-file temporary file t))
      (when (and temporary (file-exists-p temporary)) (delete-file temporary)))))

(deffoo nndouban-open-server (server &optional defs _connectionless)
  (condition-case error
      (progn
        (nnoo-change-server 'nndouban server defs)
        (let ((file (expand-file-name (concat (secure-hash 'sha256 server) ".json") nndouban-directory)))
          (setq nndouban--store (or (gethash file nndouban--stores)
                                   (puthash file (nndouban--load file) nndouban--stores))))
        t)
    (error (nnheader-report 'nndouban "%s" (error-message-string error)))))

(defun nndouban--select (&optional server)
  "Select SERVER's local store."
  (when server
    (unless (nndouban-open-server server) (error "%s" nndouban-status-string)))
  (unless nndouban--store (error "No open nndouban server"))
  nndouban--store)

(defun nndouban--group (store name)
  "Find NAME in STORE."
  (cl-find name (nndouban--db-groups store) :key (lambda (g) (plist-get g :name)) :test #'equal))

(defun nndouban--ensure-group (store name)
  "Find or create one supported subscription kind in STORE."
  (or (nndouban--group store name)
      (progn
        (unless (string-match "\\`\\(timeline\\|replies\\|group\\|topic\\)\\.\\([0-9]+\\)\\'" name)
          (error "Use timeline.USER, replies.ACCOUNT, group.ID or topic.ID"))
        (let ((group (list :name name :kind (match-string 1 name) :user (match-string 2 name)
                           :next 1 :entries nil)))
          (push group (nndouban--db-groups store))
          (nndouban--save store)
          group))))

(defun nndouban--entry (group article)
  "Find ARTICLE by local number or Message-ID in GROUP."
  (cl-find-if (lambda (entry) (if (numberp article) (= article (plist-get entry :number))
                               (equal article (plist-get entry :message-id))))
              (plist-get group :entries)))

(defun nndouban--message-id (discussion id)
  "Make a stable Message-ID for ID within DISCUSSION."
  (format "<%s.%s.%s@douban.invalid>"
          (if (thread-reader-douban--topic-id (thread-reader-discussion-url discussion)) "topic" "status")
          (thread-reader-discussion-id discussion) (replace-regexp-in-string ":" "." id)))

(defun nndouban--line (text)
  "Make remote TEXT safe for a single header field."
  (replace-regexp-in-string "[\r\n\t\x00-\x1f]+" " " (or text "")))

(defun nndouban--date (value)
  "Convert Douban local VALUE into an RFC date."
  (condition-case nil
      (format-time-string "%a, %d %b %Y %T %z"
                          (date-to-time (concat value " +0800")) 28800)
    (error "Thu, 01 Jan 1970 00:00:00 +0000")))

(defun nndouban--import (store group discussion &optional direct)
  "Merge DISCUSSION into GROUP, optionally aggregating DIRECT notification IDs.
Return numbers of notification roots which received new direct replies."
  (let* ((entries (plist-get group :entries))
         (root (cl-find-if (lambda (e) (null (thread-reader-entry-parent-id e)))
                          (thread-reader-discussion-entries discussion)))
         (root-id (nndouban--message-id discussion (thread-reader-entry-id root)))
         root-record new-targets)
    (dolist (item (thread-reader-discussion-entries discussion))
      (let* ((id (nndouban--message-id discussion (thread-reader-entry-id item)))
             (old (nndouban--entry group id))
             (record (or old (list :number (plist-get group :next) :message-id id
                                  :local-id nil :root nil :source nil :discussion-id nil
                                  :title nil :parent nil :author nil :author-id nil :time nil :body nil
                                  :format nil :url nil :placeholder nil :activity nil
                                  :targets nil :pending nil)))
             (parent (thread-reader-entry-parent-id item)))
        (unless old
          (setf (plist-get group :next) (1+ (plist-get group :next)))
          (push record entries)
          (setf (plist-get group :entries) entries))
        ;; Older snapshots lack this key.  Extend the shared record in place;
        ;; `plist-put' would otherwise prepend a new, unshared plist head.
        (unless (plist-member record :author-id)
          (nconc record (list :author-id nil)))
        (unless (plist-member record :activity)
          (nconc record (list :activity nil)))
        ;; Do not overwrite a fully fetched parent with a reference snapshot.
        (unless (and old (not (plist-get old :placeholder)) (thread-reader-entry-placeholder-p item))
          (dolist (pair (list (cons :local-id (thread-reader-entry-id item))
                             (cons :root root-id) (cons :source (thread-reader-discussion-url discussion))
                             (cons :discussion-id (thread-reader-discussion-id discussion))
                             (cons :title (thread-reader-discussion-title discussion))
                             (cons :parent (and parent (nndouban--message-id discussion parent)))
                             (cons :author (thread-reader-entry-author item))
                             (cons :author-id
                                   (or (and (nndouban-source-entry-p item)
                                            (nndouban-source-entry-author-id item))
                                       (plist-get record :author-id)))
                             (cons :time (thread-reader-entry-time item))
                             (cons :body (thread-reader-entry-body item))
                             (cons :format (symbol-name (thread-reader-entry-body-format item)))
                             (cons :url (thread-reader-entry-url item))
                             (cons :placeholder (and (thread-reader-entry-placeholder-p item) t))))
            (setf (plist-get record (car pair)) (cdr pair))))
        (when (equal id root-id) (setq root-record record))))
    (when (and root-record (equal (plist-get group :kind) "group"))
      (setf (plist-get root-record :activity)
            (car (sort (delq nil
                             (cons (plist-get root-record :activity)
                                   (mapcar #'thread-reader-entry-time
                                           (thread-reader-discussion-entries discussion))))
                       #'string>))))
    (when direct
      (let ((targets (plist-get root-record :targets)))
        (maphash
         (lambda (local-id _)
           (let ((id (nndouban--message-id discussion local-id)))
             (unless (member id targets) (push id new-targets) (push id targets)))) direct)
        (setf (plist-get root-record :targets) targets
              (plist-get root-record :pending)
              (delete-dups (append new-targets (plist-get root-record :pending))))))
    (nndouban--save store)
    (when new-targets (list (plist-get root-record :number)))))

(defun nndouban--overview-p (entry group)
  "Whether ENTRY belongs in GROUP's one-row-per-discussion overview."
  (and (null (plist-get entry :parent))
       (or (member (plist-get group :kind) '("timeline" "group" "topic"))
           (plist-get entry :targets))))

(defun nndouban--header (entry group)
  "Create a Gnus mail header for ENTRY in GROUP."
  (let* ((root (nndouban--entry group (plist-get entry :root)))
         (pending (length (plist-get root :pending)))
         (title (concat (unless (null (plist-get entry :parent)) "Re: ")
                        (nndouban--line (plist-get entry :title))
                        (when (and (null (plist-get entry :parent)) (> pending 0))
                          (format " [%d 条新回应]" pending)))))
    (make-full-mail-header
     (plist-get entry :number) title
     (concat (nndouban--line (plist-get entry :author)) " <"
             (or (nndouban-source--user-id (plist-get entry :author-id)) "noreply")
             "@douban.invalid>")
     (nndouban--date
      (cond
       ((and (null (plist-get entry :parent)) (equal (plist-get group :kind) "replies"))
        (car (sort (cons (or (plist-get entry :time) "")
                         (mapcar (lambda (id) (or (plist-get (nndouban--entry group id) :time) ""))
                                 (plist-get root :targets))) #'string>)))
       ((and (null (plist-get entry :parent)) (equal (plist-get group :kind) "group"))
        (or (plist-get entry :activity) (plist-get entry :time)))
       (t (plist-get entry :time)))) (plist-get entry :message-id)
     (or (plist-get entry :parent) "") 0 0 "" nil)))

(deffoo nndouban-retrieve-headers (articles &optional group server _fetch-old)
  (let ((data (nndouban--group (nndouban--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (nndouban--entry data number))
                    ((nndouban--overview-p entry data)))
          (nnheader-insert-nov (nndouban--header entry data)))))
  'nov))

(deffoo nndouban-request-group (group &optional server _dont-check _info)
  (let* ((data (nndouban--group (nndouban--select server) group))
         (visible (cl-count-if (lambda (entry) (nndouban--overview-p entry data)) (plist-get data :entries))))
    (if (not data) (nnheader-report 'nndouban "Unknown subscription")
      (nnheader-insert "211 %d 1 %d %s\n" visible (1- (plist-get data :next)) group t))))

(deffoo nndouban-close-group (_group &optional _server) t)

(deffoo nndouban-request-list (&optional server)
  (let ((store (nndouban--select server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (nndouban--db-groups store))
        (insert (format "%s %d 1 y\n" (plist-get group :name) (1- (plist-get group :next)))))))
  t)

(deffoo nndouban-retrieve-groups (_groups &optional server)
  (nndouban-request-list server) 'active)

(deffoo nndouban-request-list-newsgroups (&optional server)
  (let ((store (nndouban--select server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (nndouban--db-groups store))
        (insert (plist-get group :name) "\t"
                (pcase (plist-get group :kind)
                  ("timeline" "豆瓣动态 ")
                  ("topic" "豆瓣小组话题 ")
                  (_ "豆瓣回应我的 "))
                (plist-get group :user) "\n")))) t)

(deffoo nndouban-request-create-group (group &optional server _args)
  (nndouban--ensure-group (nndouban--select server) group) t)

(deffoo nndouban-request-type (_group &optional _article) 'post)
(deffoo nndouban-asynchronous-p () nil)

(deffoo nndouban-request-thread (header group)
  (let* ((data (nndouban--group (nndouban--select) group))
         (entry (nndouban--entry data (mail-header-id header)))
         (root (plist-get entry :root)))
    (when entry
      (mapcar (lambda (item) (nndouban--header item data))
              (sort (cl-remove-if-not (lambda (item) (equal root (plist-get item :root)))
                                      (copy-sequence (plist-get data :entries)))
                    (lambda (a b) (< (plist-get a :number) (plist-get b :number))))))))

(deffoo nndouban-request-article (article &optional group server buffer)
  (let* ((data (nndouban--group (nndouban--select server) group))
         (entry (nndouban--entry data article)))
    (if (not entry) (nnheader-report 'nndouban "No such cached article")
      (let ((header (nndouban--header entry data))
            (body (or (plist-get entry :body) "")))
        (with-current-buffer (or buffer nntp-server-buffer)
          (erase-buffer)
          (insert "From: " (rfc2047-encode-string (mail-header-from header)) "\n"
                  "Subject: " (rfc2047-encode-string (mail-header-subject header)) "\n"
                  "Date: " (mail-header-date header) "\n"
                  "Message-ID: " (mail-header-id header) "\n"
                  "References: " (mail-header-references header) "\n"
                  "Newsgroups: " group "\n"
                  "Archived-at: <" (nndouban--line (or (plist-get entry :url) (plist-get entry :source))) ">\n"
                  "MIME-Version: 1.0\nContent-Type: text/"
                  (if (equal (plist-get entry :format) "html") "html" "plain")
                  "; charset=utf-8\nContent-Transfer-Encoding: base64\n\n"
                  (base64-encode-string (encode-coding-string body 'utf-8)) "\n")))
      (cons group (plist-get entry :number)))))

(deffoo nndouban-request-update-mark (group article mark)
  (let* ((store (nndouban--select)) (data (nndouban--group store group))
         (entry (nndouban--entry data article)))
    (when (and entry (gnus-read-mark-p mark))
      (let ((root (nndouban--entry data (plist-get entry :root))))
        (when (plist-get root :pending)
          (setf (plist-get root :pending)
                (if (null (plist-get entry :parent)) nil
                  (delete (plist-get entry :message-id) (plist-get root :pending))))
          (nndouban--save store)))))
  mark)

(defun nndouban--wake (group numbers server)
  "Mark changed notification NUMBERS unread through Gnus for GROUP on SERVER."
  (let ((full (gnus-group-prefixed-name group (list 'nndouban server))))
    (when (gnus-get-info full) (gnus-make-articles-unread full numbers))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'gnus-summary-mode) (equal gnus-newsgroup-name full))
          (save-excursion
            (dolist (number numbers)
              (when (gnus-data-find number) (gnus-summary-mark-article number gnus-unread-mark)))))))))

(defun nndouban-update (group &optional server callback)
  "Asynchronously update GROUP on SERVER; CALLBACK receives an error or nil."
  (let* ((server (or server nndouban--server))
         (store (nndouban--select server))
         (data (nndouban--ensure-group store group))
         (busy (nndouban--db-busy store)))
    (when (gethash group busy) (user-error "This Douban subscription is already updating"))
    (puthash group t busy)
    (cl-labels
        ((finish (results error)
           (remhash group busy)
           (condition-case problem
               (let (wake)
                 (dolist (result results)
                   (setq wake (append (nndouban--import store data (car result) (cdr result)) wake)))
                 (when wake (nndouban--wake group wake server)))
             (error (setq error (error-message-string problem))))
           (message "Douban %s: %s" group (or error "updated"))
           (when callback (funcall callback error))))
      (condition-case problem
          (pcase (plist-get data :kind)
            ("timeline"
             (nndouban-source-timeline
              (plist-get data :user)
              (lambda (discussions error)
                (finish (mapcar #'list
                                (cl-remove-if-not
                                 (lambda (discussion)
                                   (nndouban--timeline-include-p
                                    (plist-get data :user) discussion))
                                 discussions))
                        error))))
            ("topic"
             (nndouban-source-discussion
              (format "https://www.douban.com/group/topic/%s/"
                      (plist-get data :user))
              (lambda (discussion error _direct)
                (finish (when discussion (list (cons discussion nil))) error))))
            ("group"
             (nndouban-source-group
              (plist-get data :user)
              (lambda (discussions error)
                (finish (mapcar #'list discussions) error))))
            (_ (nndouban-source-notifications (plist-get data :user) #'finish)))
        (error (finish nil (error-message-string problem)))))))

(deffoo nndouban-request-scan (&optional group server)
  (let ((store (nndouban--select server)))
    (dolist (name (if group (list group) (mapcar (lambda (g) (plist-get g :name)) (nndouban--db-groups store))))
      (unless (gethash name (nndouban--db-busy store)) (nndouban-update name server))))
  t)

(defun nndouban--subscribe (name)
  "Create/update NAME, then open its Gnus overview."
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((method (list 'nndouban nndouban--server))
         (full (gnus-group-prefixed-name name method)))
    (nndouban--ensure-group (nndouban--select nndouban--server) name)
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group name method)))
    (nndouban-update
     name nndouban--server
     (lambda (_error)
       (when (buffer-live-p (get-buffer gnus-group-buffer))
         (with-current-buffer gnus-group-buffer
           (gnus-group-read-group t t full)))))))

;;;###autoload
(defun nndouban-subscribe-timeline (user)
  "Subscribe to the Douban timeline of numeric USER."
  (interactive "sDouban numeric user ID: ")
  (nndouban--subscribe (concat "timeline." (thread-reader-douban--remote-id user))))

;;;###autoload
(defun nndouban-subscribe-replies ()
  "Subscribe to the selected Firefox account's direct replies."
  (interactive)
  (nndouban--subscribe (concat "replies." (nndouban-source--account))))

;;;###autoload
(defun nndouban-subscribe-group-topic (url)
  "Subscribe to a specific Douban group topic URL in Gnus."
  (interactive (list (read-string "Douban group topic URL: " (thing-at-point 'url t))))
  (unless (thread-reader-douban--group-topic-p url)
    (user-error "Enter a https://www.douban.com/group/topic/ID/ URL"))
  (nndouban--subscribe
   (concat "topic." (thread-reader-douban--topic-id url))))

;;;###autoload
(defun nndouban-subscribe-group (id)
  "Subscribe to the latest topic list of numeric Douban group ID."
  (interactive "sDouban group ID: ")
  (nndouban--subscribe (concat "group." (nndouban-source--group-id id))))

(defvar-local nndouban--compose-group-id nil
  "Numeric Douban group for a new-topic Message buffer.")

;;;###autoload
(defun nndouban-new-group-topic (group-id)
  "Compose a new plain-text topic for numeric Douban GROUP-ID.
Use Message's normal C-c C-c to publish; failed or uncertain sends keep the
draft open."
  (interactive "sDouban group ID: ")
  (nndouban-source--group-id group-id)
  (message-news (concat "douban.group." group-id))
  (setq-local nndouban--compose-group-id group-id)
  (setq-local message-send-news-function #'nndouban--send-group-topic)
  (message-goto-subject))

(defun nndouban--send-group-topic (&optional _arg)
  "Submit the current Message draft as a Douban group topic."
  (let* ((group-id (or nndouban--compose-group-id
                       (error "This is not a Douban group topic draft")))
         (title (string-trim (or (message-fetch-field "subject") "")))
         (body (nndouban--post-body))
         (store (nndouban--select nndouban--server))
         (fingerprint (secure-hash 'sha256
                                   (prin1-to-string
                                    (list "new-group-topic" group-id title body))))
         (previous (cl-find fingerprint (nndouban--db-posts store)
                            :test #'equal :key (lambda (item)
                                                 (plist-get item :fingerprint))))
         (attempt (or previous (list :fingerprint fingerprint
                                    :state "pending" :url nil)))
         done published failure)
    (when (string-empty-p title) (error "A Douban group topic needs a title"))
    (when (and previous (equal (plist-get previous :state) "pending"))
      (error "Previous send is uncertain; check Douban before retrying"))
    (if (and previous (equal (plist-get previous :state) "sent"))
        (progn (message "Already published: %s" (plist-get previous :url)) t)
      (unless previous (push attempt (nndouban--db-posts store)))
      (setf (plist-get attempt :state) "pending")
      (nndouban--save store)
      (condition-case problem
          (nndouban-source-publish-group-topic
           group-id title body
           (lambda (url error)
             (unless done
               (setq done t published url failure error)
               (if error
                   (unless (and (thread-reader-send-error-p error)
                                (thread-reader-send-error-uncertain error))
                     (setf (plist-get attempt :state) "failed"))
                 (setf (plist-get attempt :state) "sent"
                       (plist-get attempt :url) url))
               (nndouban--save store))))
        (error (unless done (setq done t failure (error-message-string problem)))
               (nndouban--save store)))
      (let ((deadline (+ (float-time) (* 3 thread-reader-douban-timeout) 10)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (cond
       ((not done) (error "Send timed out; draft retained and submission locked"))
       (failure (error "%s" (if (thread-reader-send-error-p failure)
                                (thread-reader-send-error-message failure) failure)))
       (published (message "Published Douban group topic: %s" published) t)
       (t (error "Douban did not confirm the new topic"))))))

(defun nndouban--context ()
  "Return (STORE GROUP ENTRY SERVER) for the current Gnus summary article."
  (unless (derived-mode-p 'gnus-summary-mode) (user-error "Use a Gnus summary"))
  (let* ((method (gnus-find-method-for-group gnus-newsgroup-name))
         (group (gnus-group-real-name gnus-newsgroup-name)))
    (unless (eq (car method) 'nndouban) (user-error "Not a Douban group"))
    (let* ((store (nndouban--select (cadr method))) (data (nndouban--group store group))
           (entry (nndouban--entry data (gnus-summary-article-number))))
      (unless entry (user-error "No Douban article here"))
      (list store data entry (cadr method)))))

(defun nndouban--include-thread (data entry)
  "Import ENTRY's cached discussion from DATA into the current summary."
  (let ((number (plist-get entry :number))
        (numbers (mapcar (lambda (item) (plist-get item :number))
                         (cl-remove-if-not
                          (lambda (item) (equal (plist-get item :root) (plist-get entry :root)))
                          (plist-get data :entries)))))
    (gnus-summary-goto-subject number t)
    (gnus-summary-refer-thread nil)
    (setq gnus-newsgroup-headers
          (mapcar (lambda (header)
                    (if-let* ((item (nndouban--entry data (mail-header-number header))))
                        (nndouban--header item data)
                      header)) gnus-newsgroup-headers))
    (dolist (header gnus-newsgroup-headers)
      (gnus-dependencies-add-header header gnus-newsgroup-dependencies t))
    ;; The overview's dependency tree predates the newly retrieved headers.
    ;; Include their numbers explicitly before Gnus rebuilds References.
    (gnus-summary-limit (sort (delete-dups (append numbers gnus-newsgroup-limit)) #'<))
    (gnus-summary-goto-subject number t)))

;;;###autoload
(defun nndouban-read-thread ()
  "Load the discussion at point and jump to its first relevant new reply."
  (interactive)
  (pcase-let* ((`(,store ,data ,entry ,_server) (nndouban--context))
               (summary (current-buffer)) (group gnus-newsgroup-name)
               (root (nndouban--entry data (plist-get entry :root)))
               (targets (copy-sequence (or (plist-get root :pending) (plist-get root :targets))))
               (url (plist-get entry :source)))
    (nndouban-source-discussion
     url
     (lambda (discussion error _direct)
       (if error (message "Douban thread: %s" error)
         (nndouban--import store data discussion)
         (when (and (buffer-live-p summary)
                    (with-current-buffer summary
                      (and (derived-mode-p 'gnus-summary-mode) (equal group gnus-newsgroup-name))))
           (with-current-buffer summary
             (nndouban--include-thread data entry)
             (require 'gnus-thread-reader)
             (let ((view (gnus-thread-reader-open))
                   (target (car (sort (delq nil (mapcar (lambda (id) (nndouban--entry data id)) targets))
                                      (lambda (a b) (string< (or (plist-get a :time) "")
                                                            (or (plist-get b :time) "")))))))
               (when target
                 (with-current-buffer view
                   (setq gnus-thread-reader-focus-ids
                         (delq nil (mapcar (lambda (id)
                                            (when-let* ((item (nndouban--entry data id)))
                                              (number-to-string (plist-get item :number)))) targets)))
                   (gnus-thread-reader--reveal (number-to-string (plist-get target :number)))))))))))))

(declare-function gnus-thread-reader-open "gnus-thread-reader" ())
(declare-function gnus-thread-reader--reveal "gnus-thread-reader" (id))

(defun nndouban-refresh ()
  "Update the current Douban subscription, then refresh its Gnus overview."
  (interactive)
  (pcase-let* ((`(,_store ,data ,_entry ,server) (nndouban--context))
               (summary (current-buffer)) (group gnus-newsgroup-name))
    (nndouban-update
     (plist-get data :name) server
     (lambda (_error)
       (when (and (buffer-live-p summary)
                  (with-current-buffer summary (equal group gnus-newsgroup-name)))
         (with-current-buffer summary (gnus-summary-rescan-group t)))))))

(defvar nndouban-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'nndouban-read-thread)
    (define-key map (kbd "RET") #'nndouban-read-thread)
    (define-key map (kbd "r") #'gnus-summary-followup)
    (define-key map (kbd "G") #'nndouban-refresh)
    map))
(define-minor-mode nndouban-summary-mode
  "Douban summary actions: G refreshes, C-c C-t opens the discussion."
  :lighter " Douban" :keymap nndouban-summary-mode-map)

(defun nndouban--summary-setup ()
  "Enable local bindings and a folded overview for Douban summaries."
  (when (and gnus-newsgroup-name
             (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nndouban))
    (setq-local gnus-thread-hide-subtree t)
    (nndouban-summary-mode 1)))
(add-hook 'gnus-summary-prepare-hook #'nndouban--summary-setup)

(defun nndouban--thread-entry (record)
  "Convert cached RECORD back to the shared discussion entry format."
  (make-nndouban-source-entry
   :id (plist-get record :local-id)
   :author-id (plist-get record :author-id)
   :author (or (plist-get record :author) "")
   :body (or (plist-get record :body) "")
   :body-format (if (equal (plist-get record :format) "html") 'html 'plain)
   :url (plist-get record :url) :time (plist-get record :time)))

(defun nndouban--post-body ()
  "Read one text/plain MIME body from the current Gnus posting buffer."
  (let ((mm-decrypt-option 'never) (mm-verify-option 'never) handle)
    (unwind-protect
        (progn
          (setq handle (mm-dissect-buffer t))
          (unless (and (bufferp (car handle))
                       (equal (mm-handle-media-type handle) "text/plain"))
            (error "Douban replies support plain text only; remove attachments"))
          (let* ((charset (mail-content-type-get (mm-handle-type handle) 'charset))
                 (text (decode-coding-string (mm-get-part handle)
                                            (or (mm-charset-to-coding-system charset) 'utf-8))))
            (when (string-empty-p (string-trim text)) (error "Empty Douban reply"))
            text))
      (when handle (mm-destroy-parts handle)))))

(defun nndouban--submit (store data parent body)
  "Submit BODY to PARENT, preserving uncertain sends in STORE.
Gnus's posting interface is synchronous; service process events while waiting
for the existing asynchronous website adapter.  Never retry uncertain sends."
  (let* ((fingerprint (secure-hash 'sha256
                                 (prin1-to-string (list (plist-get data :name)
                                                       (plist-get parent :message-id) body))))
         (previous (cl-find fingerprint (nndouban--db-posts store)
                            :test #'equal :key (lambda (p) (plist-get p :fingerprint))))
         (attempt (or previous (list :fingerprint fingerprint :state "pending")))
         (root (nndouban--entry data (plist-get parent :root)))
         (discussion (make-thread-reader-discussion
                      :id (plist-get parent :discussion-id) :url (plist-get parent :source)
                      :title (plist-get parent :title) :entries (list (nndouban--thread-entry root))))
         done result failure)
    (when (and previous (equal (plist-get previous :state) "pending"))
      (error "Previous send is uncertain; inspect Douban before using nndouban-clear-uncertain-sends"))
    (if (and previous (equal (plist-get previous :state) "sent")) t
      (unless previous (push attempt (nndouban--db-posts store)))
      (setf (plist-get attempt :state) "pending")
      ;; Write before issuing the POST so Emacs restart cannot silently retry it.
      (nndouban--save store)
      (condition-case problem
          (thread-reader-backend-reply
           (make-nndouban-source-backend :name 'douban) discussion
           (nndouban--thread-entry parent) body
           (lambda (entry error)
             (unless done
               (setq done t failure error result entry)
               (if error
                   (unless (and (thread-reader-send-error-p error)
                                (thread-reader-send-error-uncertain error))
                     (setf (plist-get attempt :state) "failed"))
                 (setf (plist-get attempt :state) "sent")
                 (setf (thread-reader-discussion-entries discussion)
                       (list (nndouban--thread-entry root) entry))
                 (condition-case problem
                     (nndouban--import store data discussion)
                   (error (message "Reply accepted; cache update failed: %s" (error-message-string problem)))))
               (nndouban--save store))))
        ;; An escaping error may follow dispatch or even server acceptance.
        ;; Keep the durable lock unless a callback confirmed rejection.
        (error (unless done (setq done t failure (error-message-string problem)))
               (ignore-errors (nndouban--save store))))
      (let ((deadline (+ (float-time) (* 3 thread-reader-douban-timeout) 10)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (cond
       ((not done) (error "Send timed out; the draft is retained and this submission is locked"))
       (failure (error "%s" (if (thread-reader-send-error-p failure)
                                (thread-reader-send-error-message failure) failure)))
       (t (and result t))))))

(deffoo nndouban-request-post (&optional server)
  "Post a reply or a new group topic through Douban, never SMTP or NNTP."
  (condition-case problem
      (let* ((store (nndouban--select server))
             (name (message-fetch-field "newsgroups"))
             (refs (split-string (or (message-fetch-field "references") "")))
             (group (and name (gnus-group-real-name name)))
             (data (and refs group (nndouban--group store group)))
             (parent (and data (nndouban--entry data (car (last refs))))))
        (when (or (null name) (string-match-p "[,\r\n]" name))
          (error "Select exactly one Douban group"))
        (cond
         ((and (string-match "\\`group\\.\\([1-9][0-9]*\\)\\'" group)
               (null refs))
          (let ((nndouban--compose-group-id (match-string 1 group)))
            (nndouban--send-group-topic)))
         ((and parent (not (plist-get parent :placeholder)))
          (nndouban--submit store data parent (nndouban--post-body)))
         (t (error "Reply to a known Douban article or post to a group.ID"))))
    (error (nnheader-report 'nndouban "%s" (error-message-string problem)))))

(defun nndouban-clear-uncertain-sends ()
  "Unlock uncertain submissions after checking the website for duplicates."
  (interactive)
  (unless (yes-or-no-p "Have you checked Douban and confirmed these replies were NOT published? ")
    (user-error "Uncertain submissions remain locked"))
  (let ((store (nndouban--select nndouban--server)))
    (setf (nndouban--db-posts store)
          (cl-remove "pending" (nndouban--db-posts store) :test #'equal
                     :key (lambda (p) (plist-get p :state))))
    (nndouban--save store)))

;; A news-like backend: d changes reading state; it never deletes a web post.
(gnus-declare-backend "nndouban" 'post)
(nnoo-define-skeleton nndouban)
(provide 'nndouban)
;;; nndouban.el ends here
