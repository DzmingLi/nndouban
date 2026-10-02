;;; nndouban.el --- Douban timelines and reply inbox for Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (thread-reader-douban "0.1.0") (gnus-thread-reader "0.1.0"))
;; Keywords: news, comm

;;; Commentary:

;; Subscription types: timeline.USER, replies.ACCOUNT and topic.ID.  The normal
;; Gnus overview contains one article per discussion.  Replies are cached as
;; real messages with stable numbers and References, exposed by request-thread.
;; Fetching is asynchronous; Gnus article reads use the local snapshot only.

;;; Code:
(require 'nndouban-source)
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
        (unless (string-match "\\`\\(timeline\\|replies\\|topic\\)\\.\\([0-9]+\\)\\'" name)
          (error "Use timeline.USER, replies.ACCOUNT or topic.ID"))
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
                                  :format nil :url nil :placeholder nil :targets nil :pending nil)))
             (parent (thread-reader-entry-parent-id item)))
        (unless old
          (setf (plist-get group :next) (1+ (plist-get group :next)))
          (push record entries)
          (setf (plist-get group :entries) entries))
        ;; Older snapshots lack this key.  Extend the shared record in place;
        ;; `plist-put' would otherwise prepend a new, unshared plist head.
        (unless (plist-member record :author-id)
          (nconc record (list :author-id nil)))
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
       (or (member (plist-get group :kind) '("timeline" "topic"))
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
      (if (and (null (plist-get entry :parent)) (equal (plist-get group :kind) "replies"))
          (car (sort (cons (or (plist-get entry :time) "")
                           (mapcar (lambda (id) (or (plist-get (nndouban--entry group id) :time) ""))
                                   (plist-get root :targets))) #'string>))
        (plist-get entry :time))) (plist-get entry :message-id)
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
              (lambda (discussions error) (finish (mapcar #'list discussions) error))))
            ("topic"
             (nndouban-source-discussion
              (format "https://www.douban.com/group/topic/%s/"
                      (plist-get data :user))
              (lambda (discussion error _direct)
                (finish (when discussion (list (cons discussion nil))) error))))
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
  "Post a reply through Douban, never SMTP or NNTP."
  (condition-case problem
      (let* ((store (nndouban--select server))
             (name (message-fetch-field "newsgroups"))
             (refs (split-string (or (message-fetch-field "references") "")))
             (group (and name (gnus-group-real-name name)))
             (data (and group (nndouban--group store group)))
             (parent (and data (nndouban--entry data (car (last refs))))))
        (unless (and parent (not (plist-get parent :placeholder))
                     (not (string-match-p "[,\r\n]" name)))
          (error "Reply to a known Douban message; new topics and crossposting are unsupported"))
        (nndouban--submit store data parent (nndouban--post-body)))
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
(add-to-list 'gnus-valid-select-methods '(nndouban "douban"))
(nnoo-define-skeleton nndouban)
(provide 'nndouban)
;;; nndouban.el ends here
