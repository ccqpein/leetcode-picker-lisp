(defpackage #:leetcode-picker
  (:use #:CL #:arrow-macros)
  (:export #:main)
  )

(in-package #:leetcode-picker)

(declaim (optimize (speed 3) (space 3)))

(defparameter *leetcode-all-quiz-url* "https://leetcode.com/api/problems/all/")
(defparameter *leetcode-api* "https://leetcode.com/graphql")

(defparameter *vault-path* (sb-posix:getcwd))
(defparameter *leetcode-token*
  (car (uiop:read-file-lines (format nil "~a/vault/leetcode-token" *vault-path*)))
  "cookies csrftoken")

(defparameter *leetcode-session*
  (car (uiop:read-file-lines (format nil "~a/vault/leetcode-session" *vault-path*)))
  "cookies LEETCODE_SESSION")

(defparameter *leetcode-all-quiz* nil)
(defparameter *leetcode-all-quiz-cache-path* (format nil "~a/vault/leetcode-all-quiz-cache" *vault-path*))

(defun restore-all-quiz-from-file (file)
  (with-open-file (s (merge-pathnames file) :if-does-not-exist :error)
    (let ((content (the string (read-line s))))
      (if (string/= "" content)
          (setf *leetcode-all-quiz* (yason:parse content))))
    ))

(defun fetch-all-quiz-list (&key cache-file)
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "curl" `(,*leetcode-all-quiz-url*
                                 "-H" ,(format nil "Cookie: csrftoken=~a;LEETCODE_SESSION=~a"
                                               *leetcode-token*
                                               *leetcode-session*)
                                 )
                        :search t
                        :output out
                        :error nil)
    (let* ((response (get-output-stream-string out))
           (json-response (yason:parse response))
           (all-stat-status-pairs (the list (gethash "stat_status_pairs" json-response))))
      
      (setf *leetcode-all-quiz* all-stat-status-pairs) ;; cache it in memory

      ;; cache in file 
      (if cache-file (with-open-file (file (merge-pathnames *leetcode-all-quiz-cache-path*)
                                           :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
                       (yason:encode *leetcode-all-quiz* file)))
      
      all-stat-status-pairs)))

(defun get-question-title-slug (quiz-stat)
  (let ((stat (gethash "stat" quiz-stat)))
    (the string (gethash "question__title_slug" stat))
    ))

(defun get-question-id (quiz-stat)
  (let ((stat (gethash "stat" quiz-stat)))
    (the fixnum (gethash "frontend_question_id" stat))
    ))

(defun fetch-question-description (title-slug)
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "curl" `(,*leetcode-api*
                                 "-H" "content-type: application/json"
                                 "-H" ,(format nil "Referer: https://leetcode.com/problems/~a/" title-slug)
                                 "-H" ,(format nil "Cookie: csrftoken=~a;LEETCODE_SESSION=~a"
                                               *leetcode-token*
                                               *leetcode-session*)
                                 "--data-raw"
                                 ,(format nil "{\"operationName\":\"questionData\",\"variables\":{\"titleSlug\":\"~a\"},\"query\":\"query questionData($titleSlug: String!) { question(titleSlug: $titleSlug) { content } }\"}" title-slug))
                        :search t
                        :output out
                        :error nil)
    (let* ((response (get-output-stream-string out))
           (json-response (yason:parse response)))
      (gethash "content" (gethash "question" (gethash "data" json-response))))))

(defun fetch-quiz-description-by-id (id)
  (declare (fixnum id))
  (let ((all-quiz-list (if *leetcode-all-quiz*
                           *leetcode-all-quiz*
                           (fetch-all-quiz-list :cache-file t))))
    (let (q-title-slug)
      (setf q-title-slug (get-question-title-slug
                          (find-if (lambda (q) (= id (get-question-id q)))
                                   (the list all-quiz-list))))
      
      (if (string= "" q-title-slug) (error "cannot find this id"))      
      
      q-title-slug
      )))

(defun describe-clean (describe)
  (-<> describe
    (cl-ppcre:regex-replace-all "</{0,1}p>" <> "")
    (cl-ppcre:regex-replace-all "</{0,1}em>" <> "")
    (cl-ppcre:regex-replace-all "&nbsp;" <> "")
    
    (cl-ppcre:regex-replace-all "</{0,1}code>" <> "`")
    (cl-ppcre:regex-replace-all "</{0,1}strong.*?>" <> "**")
    (cl-ppcre:regex-replace-all "</{0,1}pre>" <> "```")
    (cl-ppcre:regex-replace-all "[\\t]+<li>" <> "+ ")
    (cl-ppcre:regex-replace-all "&lt;=" <> "<=")
    (cl-ppcre:regex-replace-all "<sup>" <> "^")
    
    (cl-ppcre:regex-replace-all "((</sup>)|(</li>))" <> "")
    (cl-ppcre:regex-replace-all "</{0,1}ul>" <> "")
    ))

(defun write-readme (describe link)
  (with-open-file (file (pathname (format nil "~a/README.md" (sb-posix:getcwd)))
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (declare (stream file))
    (format file
            "# README #
[source](~a)

~a
"
            link describe)
    ))

(defun main (&optional id)
  (unless id (setf id (the fixnum (parse-integer (cadr sb-ext:*posix-argv*)))))

  ;; restore cache
  ;; only error is cache isn't create yet
  (ignore-errors (restore-all-quiz-from-file *leetcode-all-quiz-cache-path*))

  (let (title-slug
        describe)
    (handler-case (fetch-quiz-description-by-id id)
      (error ()
        ;; let fetch-all-quiz-list do again and update the cache file
        (setf *leetcode-all-quiz* nil) 
        (fetch-quiz-description-by-id id))
      (:no-error (title) (setf title-slug title)))

    (setf describe (fetch-question-description title-slug))

    (write-readme (describe-clean describe)
                  (format nil "https://leetcode.com/problems/~a/" title-slug))
    ))
