;;; thread-reader-douban.el --- Douban personal topics for thread-reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gnus-thread-reader "0.1.0") (firefox-cookies "0.1.0") (plz "0.9.1"))
;; Keywords: comm, hypermedia

;;; Commentary:

;; Read https://www.douban.com/topic/ID/ and /group/topic/ID/ discussions.
;; Cookies are obtained
;; through firefox-cookies (formerly browser-cookies), using the profile
;; selected by the user.  Registering this backend does not read cookies.
;; Web endpoints were inspected on 2026-09-30; see README.org.

;;; Code:

(require 'thread-reader)
(require 'firefox-cookies)
(require 'plz)
(require 'json)
(require 'url-parse)
(require 'url-util)
(require 'dom)
(require 'xml)

(defgroup thread-reader-douban nil
  "Read and reply to Douban personal topics."
  :group 'thread-reader)

(defcustom thread-reader-douban-cookie-function #'firefox-cookies-get
  "Function taking a URL and returning applicable (NAME . VALUE) cookies.
It must apply domain, path, expiry and container filtering for that URL."
  :type 'function)

(defcustom thread-reader-douban-page-size 20
  "Number of comments or replies requested per page."
  :type 'natnum)

(defcustom thread-reader-douban-timeout 30
  "Maximum request duration in seconds."
  :type 'number)

(defconst thread-reader-douban--user-agent
  "Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0")

(defconst thread-reader-douban--api "https://m.douban.com/rexxar/api/v2/group/topic/")

(cl-defstruct (thread-reader-douban-backend (:include thread-reader-backend)))

(defun thread-reader-douban--text (node)
  "Return all text in DOM NODE on both Emacs 29 and newer versions."
  (if (fboundp 'dom-inner-text)
      (dom-inner-text node)
    (with-no-warnings (dom-texts node))))

(defun thread-reader-douban--topic-id (url)
  "Return the personal or group topic ID in URL, or nil."
  (when (and (stringp url)
             (string-match
              "\\`https://www\\.douban\\.com/\\(?:group/\\)?topic/\\([0-9]+\\)/?\\(?:\\?[^#\r\n]*\\)?\\(?:#[^\r\n]*\\)?\\'"
              url))
    (match-string 1 url)))

(defun thread-reader-douban--group-topic-p (url)
  "Return non-nil when URL identifies a Douban group topic."
  (and (thread-reader-douban--topic-id url)
       (string-prefix-p "https://www.douban.com/group/topic/" url)))

(defun thread-reader-douban--remote-id (value)
  "Return VALUE as a validated decimal identifier."
  (let ((id (cond ((stringp value) value)
                  ((and (integerp value) (> value 0)) (number-to-string value)))))
    (unless (and id (string-match-p "\\`[0-9]+\\'" id))
      (error "Douban returned an invalid identifier"))
    id))

(defun thread-reader-douban--number (value)
  "Return VALUE as a nonnegative pagination number."
  (cond ((and (integerp value) (>= value 0)) value)
        ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
         (string-to-number value))
        (t (error "Douban returned invalid pagination metadata"))))

(defun thread-reader-douban--cookie-header (cookies)
  "Serialize filtered COOKIES, rejecting malformed header data."
  (mapconcat
   (lambda (pair)
     (unless (and (consp pair) (stringp (car pair)) (stringp (cdr pair))
                  (not (string-empty-p (car pair)))
                  (not (string-match-p "[;= \t\r\n]" (car pair)))
                  (not (string-match-p "[;\r\n]" (cdr pair))))
       (error "The cookie provider returned an invalid cookie"))
     (concat (car pair) "=" (cdr pair)))
   cookies "; "))

(defun thread-reader-douban--request-url-p (url)
  "Return non-nil for supported, credential-safe request URL shapes."
  (or (thread-reader-douban--topic-id url)
      (string-match-p
       (concat "\\`" (regexp-quote thread-reader-douban--api)
               "\\(?:[0-9]+/\\(?:comments\\|allow_comment\\|create_comment\\)"
               "\\|comment/[0-9]+/replies\\)\\'")
       url)))

(defun thread-reader-douban--failure (text method &optional uncertain)
  "Make an error with TEXT for METHOD, marking UNCERTAIN submissions."
  (if (eq method 'post)
      (make-thread-reader-send-error :message text :uncertain uncertain)
    text))

(defun thread-reader-douban--http-error (failure method)
  "Describe FAILURE without exposing request headers or response bodies."
  (let* ((record (if (plz-error-p failure) failure
                   (and (listp failure) (cl-find-if #'plz-error-p failure))))
         (response (and record (plz-error-response record)))
         (status (and response (plz-response-status response)))
         (definite (and status (<= 400 status) (< status 500) (/= status 408))))
    (thread-reader-douban--failure
     (cond ((memq status '(401 403))
            "Douban denied access; open the original page in the selected Firefox profile and check login/verification")
           ((eq status 429) "Douban rate limit reached; try again later")
           (status (format "Douban HTTP %d" status))
           (t "Douban request failed or timed out"))
     method (not definite))))

(defun thread-reader-douban--request (method url source params callback)
  "Request URL using METHOD and PARAMS, with SOURCE as Referer.
CALLBACK receives (BODY ERROR).  Never follow credential-bearing redirects."
  (let ((completed nil) (dispatched nil))
    (cl-labels ((finish (body err)
                 (unless completed
                   (setq completed t)
                   (funcall callback body err))))
      (condition-case err
          (progn
            (unless (and (memq method '(get post))
                         (thread-reader-douban--request-url-p url)
                         (thread-reader-douban--topic-id source))
              (error "Unsupported Douban request destination"))
            (let* ((cookies (funcall thread-reader-douban-cookie-function url))
                   (cookie-header (thread-reader-douban--cookie-header cookies))
                   (ck (cdr (assoc "ck" cookies)))
                   (api (string-prefix-p thread-reader-douban--api url))
                   (fields (append params (when (and api ck) `(("ck" ,ck)))))
                   (query (url-build-query-string fields))
                   (headers `(("User-Agent" . ,thread-reader-douban--user-agent)
                              ("Referer" . ,source)
                              ("Accept" . ,(if api "application/json" "text/html"))
                              ,@(when cookies `(("Cookie" . ,cookie-header)))
                              ,@(when (eq method 'post)
                                  `(("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8")
                                    ("Origin" . "https://www.douban.com")
                                    ("X-CSRF-TOKEN" . ,ck)))))
                   ;; Ignore curlrc and do not use --location: Cookie headers
                   ;; must never be forwarded to a login/challenge destination.
                   (plz-curl-default-args
                    '("--disable" "--silent" "--show-error" "--compressed")))
              (when (and (eq method 'post)
                         (not (and (assoc "dbcl2" cookies) ck)))
                (error "No Douban login in the selected Firefox profile"))
              (setq dispatched t)
              (plz method (if (and (eq method 'get) (not (string-empty-p query)))
                              (concat url "?" query) url)
                :headers headers
                :body (when (eq method 'post) (encode-coding-string query 'utf-8))
                :body-type 'binary :as 'response
                :connect-timeout 10 :timeout thread-reader-douban-timeout
                :then (lambda (response)
                        (if (<= 200 (plz-response-status response) 299)
                            (finish (plz-response-body response) nil)
                          (finish nil (thread-reader-douban--failure
                                       "Douban redirected the request; open the original page in Firefox"
                                       method (eq method 'post)))))
                :else (lambda (failure)
                        (finish nil (thread-reader-douban--http-error failure method))))))
        (error
         (finish nil
                 (if dispatched
                     (thread-reader-douban--http-error err method)
                   (thread-reader-douban--failure (error-message-string err) method))))))))

(defun thread-reader-douban--json (method url source params callback)
  "Request JSON from URL with METHOD, SOURCE and PARAMS for CALLBACK."
  (thread-reader-douban--request
   method url source params
   (lambda (body err)
     (if err (funcall callback nil err)
       (let (result failure)
         (condition-case nil
             (let* ((payload (json-parse-string body :object-type 'plist :array-type 'list
                                                :null-object nil :false-object :false))
                    (code (plist-get payload :code))
                    (explanation (plist-get payload :localized_message)))
               (if (or (and code (not (equal code 0)))
                       (and (stringp explanation) (not (string-empty-p explanation))))
                   (setq failure
                         (thread-reader-douban--failure
                          (if (stringp explanation) explanation
                            (format "Douban rejected the request (code %s)" code)) method))
                 (setq result payload)))
           (error (setq failure (thread-reader-douban--failure
                                 "Douban returned invalid JSON; open the original page to check the result"
                                 method (eq method 'post)))))
         (funcall callback result failure))))))

(defun thread-reader-douban--html (html url)
  "Extract a discussion from HTML at URL, without executing page scripts."
  (let* ((topic-id (thread-reader-douban--topic-id url))
         (document (with-temp-buffer
                     (insert html)
                     (libxml-parse-html-region (point-min) (point-max))))
         (body (car (dom-by-class document "\\btopic-richtext\\b")))
         (title (or (car (dom-by-class document "\\btopic-title\\b"))
                    (car (dom-by-tag (car (dom-by-id document "content")) 'h1))))
         (meta (or (car (dom-by-class document "\\barticle-meta\\b"))
                   (car (dom-by-class document "\\btopic-doc\\b"))))
         (author (or (car (dom-by-class meta "\\bauthor-name\\b"))
                     (car (dom-by-class meta "\\bfrom\\b"))))
         (time (car (dom-by-class meta "\\bcreate-time\\b"))))
    (unless (and body topic-id)
      (error "Douban topic content missing; check login, page access or a changed page layout"))
    (make-thread-reader-discussion
     :id topic-id :url url
     :title (let ((text (and title (string-trim (thread-reader-douban--text title)))))
              (if (or (null text) (string-empty-p text))
                  (concat "豆瓣话题 " topic-id) text))
     :entries
     (list (make-thread-reader-entry
            :id (concat "topic:" topic-id) :url url
            :author (if author (string-trim (thread-reader-douban--text author)) "豆瓣用户")
            :time (and time (string-trim (thread-reader-douban--text time)))
            :body-format 'html
            :body (with-temp-buffer (dom-print body) (buffer-string))))
     :cursor '(:kind comments :start 0))))

(defun thread-reader-douban--comment-id (id)
  "Return the remote ID of a normalized comment ID."
  (when (and (stringp id) (string-match "\\`comment:\\([0-9]+\\)\\'" id))
    (match-string 1 id)))

(defun thread-reader-douban--comment-body (comment)
  "Return COMMENT's text, including image links and deletion state."
  (concat
   (cond ((eq (plist-get comment :is_deleted) t) "[该回复已被删除]")
         ((eq (plist-get comment :is_censoring) t) "[该回复审核中]")
         (t (or (plist-get comment :text) "")))
   (mapconcat
    (lambda (photo)
      (let ((url (or (plist-get (plist-get photo :large) :url)
                     (plist-get photo :url))))
        (if (and (stringp url) (string-prefix-p "https://" url))
            (concat "\n[图片] " url) "")))
    (plist-get comment :photos) "")))

(defun thread-reader-douban--entries (comments discussion &optional container)
  "Normalize COMMENTS for DISCUSSION, optionally under CONTAINER.
Use reference snapshots for ancestors not yet loaded.  Full comments win
over reference snapshots, independent of response order."
  (let ((records (make-hash-table :test #'equal))
        (known (make-hash-table :test #'equal))
        (order nil)
        (root (concat "topic:" (thread-reader-discussion-id discussion))))
    (dolist (entry (thread-reader-discussion-entries discussion))
      (puthash (thread-reader-entry-id entry) entry known))
    (cl-labels
        ((collect (comment fallback partial)
           (let* ((remote (thread-reader-douban--remote-id (plist-get comment :id)))
                  (id (concat "comment:" remote))
                  (old (gethash id records)))
             (unless (and partial (gethash id known))
               (unless old (push id order))
               (when (or (not old) (and (nth 2 old) (not partial)))
               (puthash id (list comment fallback partial) records)
               (when-let* ((ref (plist-get comment :ref_comment)))
                 (collect ref fallback t))
               (unless partial
                 (dolist (reply (plist-get comment :replies))
                   (collect reply id nil))))))))
      (dolist (comment comments) (collect comment (or container root) nil)))
    (let (entries missing)
      (dolist (id (nreverse order))
        (pcase-let* ((`(,comment ,fallback ,partial) (gethash id records))
                     (ref (plist-get comment :ref_comment))
                     (parent-remote (or (plist-get ref :id)
                                        (plist-get comment :parent_comment_id)))
                     (parent (if parent-remote
                                 (concat "comment:" (thread-reader-douban--remote-id parent-remote))
                               fallback))
                     (remote (thread-reader-douban--remote-id (plist-get comment :id)))
                     (old (gethash id known))
                     (total (plist-get comment :total_replies))
                     (next (plist-get comment :next_reply_start)))
          (when (equal parent id) (error "Douban returned a self-referencing reply"))
          (unless (or (gethash parent known) (gethash parent records))
            (push parent missing))
          (unless (and partial old)
            (push (make-thread-reader-entry
                   :id id :parent-id parent :placeholder-p partial
                   :author (or (plist-get (plist-get comment :author) :name) "豆瓣用户")
                   :body (thread-reader-douban--comment-body comment)
                   :time (plist-get comment :create_time)
                   :url (concat (car (split-string (thread-reader-discussion-url discussion) "#"))
                                "#comment_" remote)
                   :children-cursor
                   (when (and total
                              (> (thread-reader-douban--number total)
                                 (thread-reader-douban--number
                                  (or next (length (plist-get comment :replies))))))
                     (list :kind 'replies :comment-id remote
                           :start (thread-reader-douban--number
                                   (or next (length (plist-get comment :replies)))))))
                  entries))))
      (dolist (id (delete-dups missing))
        (push (make-thread-reader-entry
               :id id :parent-id root :placeholder-p t
               :author "未加载的父回复" :body "[父回复尚未加载或已删除]")
              entries))
      (nreverse entries))))

(defun thread-reader-douban--page (payload discussion cursor)
  "Convert PAYLOAD for DISCUSSION and CURSOR into a normalized page."
  (let* ((kind (plist-get cursor :kind))
         (field (if (eq kind 'comments) :comments :replies))
         (comments (plist-get payload field))
         (start (thread-reader-douban--number (plist-get payload :start)))
         (count (thread-reader-douban--number (plist-get payload :count)))
         (total (thread-reader-douban--number (plist-get payload :total)))
         (next (+ start count)))
    (unless (and (plist-member payload field) (listp comments)
                 (= start (plist-get cursor :start)) (> count 0))
      (error "Douban returned an unexpected comments page"))
    (when (and (< next total) (null comments))
      (error "Douban returned an empty page before the end of the discussion"))
    (make-thread-reader-page
     :entries (thread-reader-douban--entries
               comments discussion
               (when (eq kind 'replies)
                 (concat "comment:" (plist-get cursor :comment-id))))
     :cursor (when (< next total)
               (let ((cursor (copy-sequence cursor)))
                 (plist-put cursor :start next))))))

(cl-defmethod thread-reader-backend-match-p
  ((_backend thread-reader-douban-backend) url)
  (and (thread-reader-douban--topic-id url) t))

(cl-defmethod thread-reader-backend-open
  ((_backend thread-reader-douban-backend) url callback)
  (unless (thread-reader-douban--topic-id url) (error "Unsupported Douban topic URL"))
  ;; Retain query parameters: Douban can deny the bare URL of a shared topic.
  (setq url (car (split-string url "#")))
  (thread-reader-douban--request
   'get url url nil
   (lambda (html err)
     (if err (funcall callback nil err)
       (let (discussion failure)
         (condition-case error
             (setq discussion (thread-reader-douban--html html url))
           (error (setq failure (error-message-string error))))
         ;; Comments are loaded separately.  A failed comments request must
         ;; not make the successfully retrieved article disappear.
         (funcall callback discussion failure))))))

(cl-defmethod thread-reader-backend-children
  ((_backend thread-reader-douban-backend) discussion parent cursor callback)
  (let* ((source (thread-reader-discussion-url discussion))
         (topic (thread-reader-douban--remote-id (thread-reader-discussion-id discussion)))
         (kind (plist-get cursor :kind))
         (start (thread-reader-douban--number (plist-get cursor :start)))
         (count (min 100 (max 1 thread-reader-douban-page-size)))
         (path
          (pcase kind
            ('comments
             (when parent (error "Discussion pagination requires a root cursor"))
             (concat topic "/comments"))
            ('replies
             (let ((id (thread-reader-douban--remote-id (plist-get cursor :comment-id))))
               (unless (and parent (equal (thread-reader-entry-id parent) (concat "comment:" id)))
                 (error "Reply cursor belongs to another entry"))
               (concat "comment/" id "/replies")))
            (_ (error "Unknown Douban pagination cursor")))))
    (thread-reader-douban--json
     'get (concat thread-reader-douban--api path) source
     `(("start" ,(number-to-string start)) ("count" ,(number-to-string count))
       ,@(when (eq kind 'comments) '(("nested" "1") ("order_by" "time"))))
     (lambda (payload err)
       (if err (funcall callback nil err)
         (let (page failure)
           (condition-case error
               (setq page (thread-reader-douban--page payload discussion cursor))
             (error (setq failure (error-message-string error))))
           (funcall callback page failure)))))))

(cl-defmethod thread-reader-backend-can-reply-p
  ((_backend thread-reader-douban-backend) discussion entry)
  (and (thread-reader-douban--topic-id (thread-reader-discussion-url discussion))
       (or (thread-reader-douban--comment-id (thread-reader-entry-id entry))
           (equal (thread-reader-entry-id entry)
                  (concat "topic:" (thread-reader-discussion-id discussion))))))

(cl-defmethod thread-reader-backend-reply
  ((_backend thread-reader-douban-backend) discussion parent body callback)
  (let* ((source (thread-reader-discussion-url discussion))
         (topic (thread-reader-douban--remote-id (thread-reader-discussion-id discussion)))
         (endpoint (concat thread-reader-douban--api topic "/"))
         (ref (thread-reader-douban--comment-id (thread-reader-entry-id parent))))
    (unless (or ref (equal (thread-reader-entry-id parent) (concat "topic:" topic)))
      (error "Invalid Douban reply target"))
    ;; Permission checks are read-only and are repeated at submission time.
    (thread-reader-douban--json
     'get (concat endpoint "allow_comment") source nil
     (lambda (permission err)
       (cond
        (err (funcall callback nil err))
        ((not (eq (plist-get permission :allow_comment) t))
         (funcall callback nil (or (plist-get permission :forbid_reason)
                                   "Douban does not allow replies to this topic")))
        (t
         (thread-reader-douban--json
          'post (concat endpoint "create_comment") source
          `(("text" ,body) ("resp_type" "c_dict") ("sync_to_status" "0")
            ,@(when ref `(("ref_cid" ,ref))))
          (lambda (payload error)
            (if error (funcall callback nil error)
              (let (entry failure)
                (condition-case nil
                    (let* ((comment (or (plist-get payload :comment)
                                        (plist-get payload :data) payload))
                           (id (thread-reader-douban--remote-id (plist-get comment :id))))
                      (setq entry
                            (make-thread-reader-entry
                             :id (concat "comment:" id)
                             :parent-id (thread-reader-entry-id parent)
                             :author (or (plist-get (plist-get comment :author) :name) "我")
                             :time (plist-get comment :create_time)
                             :body (or (plist-get comment :text) body)
                             :url (concat source "#comment_" id))))
                  (error (setq failure
                               (make-thread-reader-send-error
                                :message "Douban did not return a confirmed comment ID; check the website before retrying"
                                :uncertain t))))
                (funcall callback entry failure)))))))))))

(thread-reader-register-backend (make-thread-reader-douban-backend :name 'douban))

;;;###autoload
(defun thread-reader-douban-open (url)
  "Open a Douban personal topic URL in thread-reader."
  (interactive (list (read-string "Douban topic URL: " (thing-at-point 'url t))))
  (thread-reader-open url 'douban))

(provide 'thread-reader-douban)
;;; thread-reader-douban.el ends here
