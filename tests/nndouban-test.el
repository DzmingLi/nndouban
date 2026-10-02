;;; nndouban-test.el --- Offline integration tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nndouban)
(require 'gnus-thread-reader)

(defun nndouban-test--discussion (&optional extra)
  (make-thread-reader-discussion
   :id "123" :url "https://www.douban.com/people/7/status/123/" :title "测试广播"
   :entries
   (append
    (list (make-thread-reader-entry :id "status:123" :author "作者" :time "2026-10-02 08:00:00"
                                    :body "主帖正文")
          (make-thread-reader-entry :id "comment:1" :parent-id "status:123" :author "我"
                                    :time "2026-10-02 08:10:00" :body "我的原评论")
          (make-thread-reader-entry :id "comment:2" :parent-id "comment:1" :author "对方"
                                    :time "2026-10-02 08:20:00" :body "第一条回应")
          (make-thread-reader-entry :id "comment:3" :parent-id "comment:1" :author "对方"
                                    :time "2026-10-02 08:30:00" :body "第二条回应"))
    (when extra (list (make-thread-reader-entry :id "comment:4" :parent-id "comment:2"
                                               :author "对方" :time "2026-10-02 08:40:00"
                                               :body "新的回应"))))))

(defun nndouban-test--direct (&rest ids)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (id ids) (puthash id t table)) table))

(defmacro nndouban-test--store (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "nndouban-test-" t))
          (store (nndouban--load (expand-file-name "snapshot.json" directory)))
          (group (nndouban--ensure-group store "replies.42"))
          (nndouban--store store))
     (unwind-protect (progn ,@body)
       (delete-directory directory t))))

(ert-deftest nndouban-test-aggregation-stable-numbers-and-restart ()
  (nndouban-test--store
    (should (equal '(1) (nndouban--import store group (nndouban-test--discussion)
                                        (nndouban-test--direct "comment:2" "comment:3"))))
    (should (= 1 (cl-count-if (lambda (e) (nndouban--overview-p e group)) (plist-get group :entries))))
    (let* ((root (nndouban--entry group 1))
           (numbers (mapcar (lambda (e) (cons (plist-get e :message-id) (plist-get e :number)))
                            (plist-get group :entries)))
           (reloaded (nndouban--load (nndouban--db-file store)))
           (again (nndouban--group reloaded "replies.42")))
      (should (= 2 (length (plist-get root :pending))))
      (should (string-match-p "08:30:00" (mail-header-date (nndouban--header root group))))
      (should-not (nndouban--import reloaded again (nndouban-test--discussion)
                                   (nndouban-test--direct "comment:2" "comment:3")))
      (should (equal numbers (mapcar (lambda (e) (cons (plist-get e :message-id) (plist-get e :number)))
                                     (plist-get again :entries))))
      (nndouban-request-update-mark "replies.42" 1 gnus-read-mark)
      (should-not (plist-get root :pending))
      (should (equal '(1) (nndouban--import store group (nndouban-test--discussion t)
                                          (nndouban-test--direct "comment:2" "comment:3" "comment:4"))))
      (should (= 1 (length (plist-get root :pending))))
      (should (= 5 (plist-get (nndouban--entry group "<status.123.comment.4@douban.invalid>") :number)))
      (should (= #o600 (logand #o777 (file-modes (nndouban--db-file store))))))))

(ert-deftest nndouban-test-gnus-overview-and-thread-headers ()
  (nndouban-test--store
    (nndouban--import store group (nndouban-test--discussion) (nndouban-test--direct "comment:2" "comment:3"))
    (let ((nntp-server-buffer (generate-new-buffer " *nndouban headers*")))
      (unwind-protect
          (progn
            (should (eq 'nov (nndouban-retrieve-headers '(1 2 3 4) "replies.42")))
            (with-current-buffer nntp-server-buffer
              (should (= 1 (count-lines (point-min) (point-max))))
              (should (string-match-p "2 条新回应" (buffer-string))))
            (let ((headers (nndouban-request-thread (nndouban--header (nndouban--entry group 1) group) "replies.42")))
              (should (= 4 (length headers)))
              (should (equal (mail-header-references (nth 2 headers)) "<status.123.comment.1@douban.invalid>")))
            (with-current-buffer nntp-server-buffer
              (should (nndouban-request-article 3 "replies.42" nil (current-buffer)))
              (should (equal '(plain . "第一条回应") (gnus-thread-reader--body-from-buffer)))))
        (kill-buffer nntp-server-buffer)))))

(ert-deftest nndouban-test-header-injection-and-source-boundaries ()
  (nndouban-test--store
    (let ((discussion (nndouban-test--discussion)))
      (setf (thread-reader-discussion-title discussion) "Title\nBcc: victim@example.org")
      (nndouban--import store group discussion)
      (should-not (string-match-p "[\n\r]" (mail-header-subject (nndouban--header (nndouban--entry group 1) group)))))
    (should (equal (plist-get (nndouban--ensure-group store "topic.123") :kind)
                   "topic"))
    (should-error (nndouban--ensure-group store "topic.bad")))
  (should-not (nndouban-source--allowed "https://evil.example/notification/reply_notify?id=1" 'get))
  (should-not (nndouban-source--allowed "https://www.douban.com.evil.example/reply_notify/" 'get))
  (should-not (nndouban-source--allowed "https://www.douban.com/reply_notify/" 'post))
  (should (nndouban-source--allowed "https://m.douban.com/rexxar/api/v2/status/123/create_comment" 'post)))

(ert-deftest nndouban-test-direct-replies-exclude-unrelated-and-self ()
  (let* ((me '(:id "42" :name "我"))
         (comments (list (list :id 1 :author me :replies
                               (list (list :id 2 :is_deleted :false :author '(:id "7")
                                           :ref_comment (list :id 1 :author me))))
                         '(:id 3 :author (:id "9") :text "unrelated")
                         (list :id 4 :author me :ref_comment (list :id 1 :author me)))))
    (should (equal '(2) (mapcar (lambda (c) (plist-get c :id))
                               (nndouban-source--direct-replies (list :comments comments) "42"))))
    (should (equal '(2 3) (mapcar (lambda (c) (plist-get c :id))
                                 (nndouban-source--direct-replies (list :comments comments) "42" "42"))))))

(ert-deftest nndouban-test-status-pagination-and-root-parent ()
  (let* ((backend (make-nndouban-source-backend :name 'douban :account "42"
                                              :direct-ids (make-hash-table :test #'equal)))
         (discussion (nndouban-test--discussion)) result failure)
    (cl-letf (((symbol-function 'nndouban-source--json)
               (lambda (_method _url _source _params callback)
                 (funcall callback '(:total 2 :comments ((:id 9 :text "hi" :author (:id "7" :name "other")
                                                         :ref_comment (:id 1 :author (:id "42"))))) nil))))
      (let ((nndouban-source-page-size 1))
        (thread-reader-backend-children backend discussion nil '(:kind comments :start 0)
                                        (lambda (page error) (setq result page failure error)))))
    (should-not failure)
    (should (= 1 (plist-get (thread-reader-page-cursor result) :start)))
    (should (gethash "comment:9" (nndouban-source-backend-direct-ids backend)))))

(ert-deftest nndouban-test-send-confirmation-and-uncertain-deduplication ()
  (nndouban-test--store
    (nndouban--import store group (nndouban-test--discussion))
    (let ((calls 0))
      (cl-letf (((symbol-function 'thread-reader-backend-reply)
                 (lambda (_backend _discussion parent body callback)
                   (cl-incf calls)
                   (should (equal (thread-reader-entry-id parent) "comment:2"))
                   (funcall callback (make-thread-reader-entry :id "comment:20" :parent-id "comment:2"
                                                               :author "me" :body body) nil))))
        (should (nndouban--submit store group (nndouban--entry group 3) "reply"))
        (should (nndouban--submit store group (nndouban--entry group 3) "reply"))
        (should (= calls 1)))
      (cl-letf (((symbol-function 'thread-reader-backend-reply)
                 (lambda (_backend _discussion _parent _body callback)
                   (cl-incf calls)
                   (funcall callback nil (make-thread-reader-send-error :message "timeout" :uncertain t)))))
        (should-error (nndouban--submit store group (nndouban--entry group 3) "uncertain"))
        (should-error (nndouban--submit (nndouban--load (nndouban--db-file store)) group
                                       (nndouban--entry group 3) "uncertain"))
        (should (= calls 2))))))

(ert-deftest nndouban-test-group-topic-publish-confirmation-and-lock ()
  (nndouban-test--store
    (let ((calls 0)
          (nndouban--server "test"))
      (cl-letf (((symbol-function 'nndouban--select)
                 (lambda (&optional _server) store))
                ((symbol-function 'nndouban--post-body)
                 (lambda () "第一段\n第二段"))
                ((symbol-function 'nndouban-source-publish-group-topic)
                 (lambda (id title body callback)
                   (cl-incf calls)
                   (should (equal id "174786"))
                   (should (equal title "标题"))
                   (should (equal body "第一段\n第二段"))
                   (funcall callback
                            "https://www.douban.com/group/topic/123456/" nil))))
        (with-temp-buffer
          (message-mode)
          (insert "Newsgroups: douban.group.174786\nSubject: 标题\n\n正文")
          (setq-local nndouban--compose-group-id "174786")
          (should (nndouban--send-group-topic))
          (should (nndouban--send-group-topic))
          (should (= calls 1))
          (should (equal (plist-get (car (nndouban--db-posts store)) :state)
                         "sent")))))))

(ert-deftest nndouban-test-group-topic-draft-content ()
  (let* ((content (json-parse-string
                   (nndouban-source--draft-content "第一段\n第二段")
                   :object-type 'plist :array-type 'list))
         (blocks (plist-get content :blocks)))
    (should (equal (mapcar (lambda (block) (plist-get block :text)) blocks)
                   '("第一段" "第二段")))
    (should (equal (plist-get (car blocks) :type) "unstyled"))))

(ert-deftest nndouban-test-real-gnus-and-reader-integration ()
  (let* ((directory (make-temp-file "nndouban-gnus-" t))
         (gnus-home-directory directory) (gnus-directory directory)
         (gnus-startup-file (expand-file-name "newsrc" directory))
         (gnus-init-file nil) (gnus-site-init-file nil)
         (gnus-select-method '(nnnil "")) (gnus-secondary-select-methods nil)
         (gnus-use-dribble-file nil) (gnus-use-cache nil) (gnus-agent nil)
         (gnus-save-newsrc-file nil) (gnus-read-newsrc-file nil)
         (gnus-check-new-newsgroups nil) (gnus-inhibit-startup-message t)
         (gnus-summary-display-arrow nil)
         (original-buffers (buffer-list)) view)
    (unwind-protect
        (progn
          (gnus-no-server)
          (nndouban-open-server "fixture" `((nndouban-directory ,directory)))
          (let* ((store nndouban--store) (group (nndouban--ensure-group store "replies.42")))
            (nndouban--import store group (nndouban-test--discussion)
                              (nndouban-test--direct "comment:2" "comment:3")))
          (with-current-buffer gnus-group-buffer
            (gnus-group-make-group "replies.42" `(nndouban "fixture" (nndouban-directory ,directory)))
            (gnus-group-read-group t t "nndouban+fixture:replies.42"))
          (with-current-buffer gnus-summary-buffer
            (should (eq (key-binding (kbd "RET")) #'nndouban-read-thread))
            (gnus-summary-goto-subject 1)
            (let ((group (nndouban--group nndouban--store "replies.42")))
              (nndouban--include-thread group (nndouban--entry group 1)))
            (let ((before (copy-sequence gnus-newsgroup-unreads)))
              (cl-letf (((symbol-function 'nndouban-source-discussion)
                         (lambda (_url callback &optional _account)
                           (funcall callback (nndouban-test--discussion) nil nil))))
                (nndouban-read-thread)
                (setq view (window-buffer (selected-window))))
              (with-current-buffer view
                (should (equal "3" (thread-reader--current-id)))
                (should (equal '("4" "3") gnus-thread-reader-focus-ids))
                (gnus-thread-reader--cancel)
                (while gnus-thread-reader--queue
                  (gnus-thread-reader--load-one view gnus-thread-reader--generation)
                  (gnus-thread-reader--cancel))
                (should (= 4 (hash-table-count thread-reader--entries)))
                (should (equal "2" (thread-reader-entry-parent-id (gethash "3" thread-reader--entries))))
                (should (string-match-p "第一条回应" (buffer-string)))
                (should (equal before (buffer-local-value 'gnus-newsgroup-unreads gnus-thread-reader--summary)))
                (goto-char (gethash "3" thread-reader--positions))
                (gnus-thread-reader-mark-read)
                (should-not (memq 3 (buffer-local-value 'gnus-newsgroup-unreads gnus-thread-reader--summary)))
                (should (= 1 (length (plist-get (nndouban--entry (nndouban--group nndouban--store "replies.42") 1) :pending))))
                (gnus-thread-reader-tick)
                (should (memq 3 (buffer-local-value 'gnus-newsgroup-marked gnus-thread-reader--summary)))
                (gnus-thread-reader-mark-unread)
                (goto-char (gethash "1" thread-reader--positions))
                (thread-reader-toggle)
                (gnus-thread-reader-next-unread)
                (should-not (invisible-p (point)))
                (let ((selected (thread-reader--current-id)))
                  (gnus-thread-reader-refresh)
                  (should (equal selected (thread-reader--current-id))))
                (goto-char (gethash "3" thread-reader--positions))
                (let (posted)
                  (cl-letf (((symbol-function 'gnus-summary-followup)
                             (lambda (&rest _) (setq posted (gnus-summary-article-number))))
                            ((symbol-function 'gnus-summary-reply)
                             (lambda (&rest _) (ert-fail "Mail transport selected"))))
                    (gnus-thread-reader-reply)
                    (should (equal 3 posted))))))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer original-buffers)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest nndouban-test-notification-resolvers-coalesce ()
  (let ((requests 0) (discussions 0) result failure)
    (cl-letf (((symbol-function 'nndouban-source--account) (lambda () "42"))
              ((symbol-function 'nndouban-source--request)
               (lambda (_method url _source _params callback &optional _redirects)
                 (cl-incf requests)
                 (funcall callback
                          (if (string-suffix-p "/reply_notify/" url)
                              (concat "<div class='new-reply-item' id='reply_notify_1'><div class='content'>"
                                      "<a href='/notification/reply_notify?id=1'>回应</a></div></div>"
                                      "<div class='new-reply-item' id='reply_notify_2'><div class='content'>"
                                      "<a href='/notification/reply_notify?id=2'>回应</a></div></div>")
                            "<a href='https://www.douban.com/people/7/status/123/?tab=comment#sep'>thread</a>") nil)))
              ((symbol-function 'nndouban-source-discussion)
               (lambda (_url callback &optional account)
                 (should (equal account "42"))
                 (cl-incf discussions)
                 (funcall callback (nndouban-test--discussion) nil
                          (nndouban-test--direct "comment:2" "comment:3")))))
      (nndouban-source-notifications "42" (lambda (items error) (setq result items failure error)))
      (should-not failure)
      (should (= 3 requests))
      (should (= 1 discussions))
      (should (= 1 (length result)))
      (should (= 2 (hash-table-count (cdar result)))))))

(ert-deftest nndouban-test-broadcast-empty-parent-schema ()
  (let* ((backend (make-nndouban-source-backend :name 'douban))
         (discussion (make-thread-reader-discussion
                      :id "123" :url "https://www.douban.com/people/7/status/123/"
                      :entries (list (make-thread-reader-entry :id "status:123")))) page failure)
    (cl-letf (((symbol-function 'nndouban-source--json)
               (lambda (_method _url _source _params callback)
                 (funcall callback '(:total 1 :start 0 :count 20 :comments
                                            ((:id 456 :text "reply" :parent_comment_id ""))) nil))))
      (thread-reader-backend-children backend discussion nil '(:kind comments :start 0)
                                      (lambda (value error) (setq page value failure error)))
      (should-not failure)
      (should (= 1 (length (thread-reader-page-entries page))))
      (should (equal "status:123" (thread-reader-entry-parent-id (car (thread-reader-page-entries page))))))))

(ert-deftest nndouban-test-mime-alternative-and-attachment ()
  (with-temp-buffer
    (insert "MIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=outer\n\n"
            "--outer\nContent-Type: multipart/alternative; boundary=inner\n\n"
            "--inner\nContent-Type: text/plain; charset=utf-8\n\nplain fallback\n"
            "--inner\nContent-Type: text/html; charset=utf-8\n\n<p>HTML body</p>\n--inner--\n"
            "--outer\nContent-Type: text/plain\nContent-Disposition: attachment; filename=secret.txt\n\nattachment body\n--outer--\n")
    (let ((body (gnus-thread-reader--body-from-buffer)))
      (should (eq (car body) 'html))
      (should (string-match-p "HTML body" (cdr body)))
      (should-not (string-match-p "plain fallback\\|attachment body" (cdr body))))
    (should-error (nndouban--post-body))))

(ert-deftest nndouban-test-stable-author-identity ()
  (nndouban-test--store
    (let* ((discussion (nndouban-source--status-discussion
                        '(:id "123" :author (:id "281084685" :name "原昵称") :text "body")))
           (root (car (thread-reader-discussion-entries discussion))))
      (should (equal "281084685" (nndouban-source-entry-author-id root)))
      (nndouban--import store group discussion)
      (setf (plist-get group :entries)
            (mapcar (lambda (entry)
                      (let ((old (copy-sequence entry))) (cl-remf old :author-id) old))
                    (plist-get group :entries)))
      (setf (thread-reader-entry-author root) "新昵称")
      (nndouban--import store group discussion)
      (let* ((reloaded (nndouban--load (nndouban--db-file store)))
             (data (nndouban--group reloaded "replies.42"))
             (header (nndouban--header (nndouban--entry data 1) data)))
        (should (equal "新昵称 <281084685@douban.invalid>" (mail-header-from header)))
        (should (equal "新昵称" (gnus-thread-reader--author header)))
        (should (= 1 (length (plist-get data :entries))))))
    (let ((authors (nndouban-source--comment-authors
                    '((:id 1 :author (:id 42 :name "相同昵称") :replies
                           ((:id 2 :author (:id 43 :name "相同昵称")
                                 :ref_comment (:id 3 :author (:id 44)))))))))
      (should (equal "42" (gethash "comment:1" authors)))
      (should (equal "43" (gethash "comment:2" authors)))
      (should (equal "44" (gethash "comment:3" authors))))))
