;;; nndouban-source.el --- Douban retrieval for Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Pure HTML/status parsers adapted from Dzming Li's elfeed-adapters-douban.el.
;; No Elfeed runtime dependency.

;;; Code:
(require 'thread-reader-douban)
(require 'seq)
(require 'url)

(defcustom nndouban-source-page-size 20
  "Items requested per Douban page."
  :type 'natnum :group 'thread-reader-douban)

(defun nndouban-source--group-id (id)
  "Validate numeric Douban group ID."
  (unless (and (stringp id) (string-match-p "\\`[1-9][0-9]*\\'" id))
    (error "Expected a numeric Douban group ID"))
  id)

(defun nndouban-source--group-page (url callback)
  "Read group topic URL and call CALLBACK with (HTML ERROR).
The desktop group page can reject curl even when Emacs's URL transport works.
Do not follow redirects with Cookie headers."
  (unless (thread-reader-douban--group-topic-p url)
    (error "Unsupported Douban group topic URL"))
  (condition-case nil
      (let* ((cookies (funcall thread-reader-douban-cookie-function url))
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
                (funcall callback
                         (buffer-substring-no-properties (point) (point-max)) nil))
            (kill-buffer (current-buffer)))))
    (error (funcall callback nil "Douban group topic access denied; check Firefox login"))))

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

(provide 'nndouban-source)
;;; nndouban-source.el ends here
