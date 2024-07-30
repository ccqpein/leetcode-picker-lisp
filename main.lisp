(defpackage :leetcode-picker
  (:use :CL :arrow-macros :str)

  (:import-from
   :jkl-cmd
   :curl)
  
  (:export :main)
  )

(in-package #:leetcode-picker)

(declaim (optimize (speed 3) (space 3)))

(defparameter *leetcode-all-quiz-url* "https://leetcode.com/api/problems/all/")
(defparameter *leetcode-api* "https://leetcode.com/graphql")

(defparameter *vault-path* #.(uiop:pathname-directory-pathname (uiop/lisp-build:current-lisp-file-pathname)))
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
  "fetch tha all quiz list from leetcode with token and session.
when :cache-file is t, write all list in cache file for future use"
  (let ((out (make-string-output-stream)))
    (curl *leetcode-all-quiz-url* :H (format nil "Cookie: csrftoken=~a;LEETCODE_SESSION=~a"
                                             *leetcode-token*
                                             *leetcode-session*)
                                  :jkl-output out
                                  :jkl-error nil)
    
    (let* ((response (get-output-stream-string out))
           (json-response (yason:parse response))
           (all-stat-status-pairs (the list (gethash "stat_status_pairs" json-response))))
      
      (setf *leetcode-all-quiz* all-stat-status-pairs) ;; cache it in memory

      ;; cache in file 
      (if cache-file (prog1
                         (with-open-file (file (merge-pathnames *leetcode-all-quiz-cache-path*)
                                               :direction :output
                                               :if-exists :supersede
                                               :if-does-not-exist :create)
                           (yason:encode *leetcode-all-quiz* file))
                       (format t "generate cache in ~a~%" (merge-pathnames *leetcode-all-quiz-cache-path*))))
      
      all-stat-status-pairs)))

(defun get-question-title-slug (quiz-stat)
  (let ((stat (gethash "stat" quiz-stat)))
    (the string (gethash "question__title_slug" stat))
    ))

(defun get-question-title (quiz-stat)
  (let ((stat (gethash "stat" quiz-stat)))
    (the string (gethash "question__title" stat))
    ))

(defun get-question-id (quiz-stat)
  (let ((stat (gethash "stat" quiz-stat)))
    (the fixnum (gethash "frontend_question_id" stat))
    ))

(defun get-question-title-slug-by-id (id)
  (declare (fixnum id))
  (let ((all-quiz-list (if *leetcode-all-quiz*
                           *leetcode-all-quiz*
                           (fetch-all-quiz-list :cache-file t))))
    
    (let (q-title-slug)
      (setf q-title-slug (get-question-title-slug
                          (find-if (lambda (q) (= id (get-question-id q)))
                                   (the list all-quiz-list))))
      
      (if (string= "" q-title-slug) (error "cannot find this id"))
      q-title-slug)))

(defun get-question-by-id (id)
  "get the question of id"
  (declare (fixnum id))
  (let ((all-quiz-list (if *leetcode-all-quiz*
                           *leetcode-all-quiz*
                           (fetch-all-quiz-list :cache-file t))))
    (find-if (lambda (q) (= id (get-question-id q)))
             (the list all-quiz-list))))

(defun get-question-difficulty (quiz-stat)
  (let ((difficulty (gethash "difficulty" quiz-stat)))
    ;; 1 => easy; 2 => med; 3 => hard
    (the fixnum (gethash "level" difficulty))
    ))

(defun get-question-paid-only (quiz-stat)
  (the boolean (gethash "paid_only" quiz-stat)))

(defun fetch-question-description (title-slug)
  (let ((out (make-string-output-stream)))
    (curl *leetcode-api*
          :H "content-type: application/json"
          :H (format nil "Referer: https://leetcode.com/problems/~a/" title-slug)
          :H (format nil "Cookie: csrftoken=~a;LEETCODE_SESSION=~a"
                     *leetcode-token*
                     *leetcode-session*)
          :data-raw (format nil "{\"operationName\":\"questionData\",\"variables\":{\"titleSlug\":\"~a\"},\"query\":\"query questionData($titleSlug: String!) { question(titleSlug: $titleSlug) { content } }\"}" title-slug)
          
          :jkl-output out
          :jkl-error nil
          )
    (let* ((response (get-output-stream-string out))
           (json-response (yason:parse response)))
      (the string (gethash "content" (gethash "question" (gethash "data" json-response)))))))

(defun fetch-quiz-description-by-id (id)
  "find the quiz has id in *leetcode-all-quiz*. fetch the description"
  (declare (fixnum id))
  (let (q-title-slug)
    (setf q-title-slug (get-question-title-slug-by-id id))
    
    (fetch-question-description q-title-slug)
    ))

(defun describe-clean (describe)
  (-<> describe
    (cl-ppcre:regex-replace-all "</{0,1}p>" <> "")
    (cl-ppcre:regex-replace-all "</{0,1}em>" <> "")
    
    (cl-ppcre:regex-replace-all "&nbsp;" <> "")
    (cl-ppcre:regex-replace-all "&gt;" <> ">")
    (cl-ppcre:regex-replace-all "&quot;" <> "\"")
    (cl-ppcre:regex-replace-all "&lt;" <> "<")

    (cl-ppcre:regex-replace-all "</{0,1}ol>" <> "")
    (cl-ppcre:regex-replace-all "</{0,1}code>" <> "`")
    (cl-ppcre:regex-replace-all "</{0,1}strong.*?>" <> "**")
    (cl-ppcre:regex-replace-all "</{0,1}b.*?>" <> "**")
    (cl-ppcre:regex-replace-all "</{0,1}pre>" <> "```")
    (cl-ppcre:regex-replace-all "[\\t]+<li>" <> "+ ")
    (cl-ppcre:regex-replace-all "<sup>" <> "^")
    
    (cl-ppcre:regex-replace-all "((</sup>)|(</li>))" <> "")
    (cl-ppcre:regex-replace-all "</{0,1}ul>" <> "")

    (cl-ppcre:regex-replace-all "&#39;" <> "'")

    (do* ((a <>)
          (aa (multiple-value-list
               (cl-ppcre:scan-to-strings "([\\s\\S]*?)<div class=\"example-block\">([\\s\\S]*?)</div>([\\s\\S]*)" a))
              (multiple-value-list
               (cl-ppcre:scan-to-strings "([\\s\\S]*?)<div class=\"example-block\">([\\s\\S]*?)</div>([\\s\\S]*)" a)))
          )
         ((equal aa '(nil)) a)
      (let ((res (second aa)))
        (declare ((simple-array string (*)) res))
        (setf a (apply #'str:concat `(,(elt res 0) "```" ,(elt res 1) "```" ,(elt res 2))))))
    ))

(defun write-description-to-md-file (file describe link)
  (with-open-file (file (pathname (format nil "~a/~a" (sb-posix:getcwd) file))
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

(defun random-pick-one-quiz (&key difficulty)
  "randomly try several times to pick the quiz match the difficulty (if it is given)"
  (declare ((or null fixnum) difficulty))
  (let ((all-quiz-list (if *leetcode-all-quiz*
                           *leetcode-all-quiz*
                           (fetch-all-quiz-list :cache-file t))))
    (setf *random-state* (make-random-state t))
    (let ((try-time 10))
      (loop repeat try-time ;; try many times in case infinity loop
            for q = (nth (random (list-length all-quiz-list)) all-quiz-list)
            when (and (not (get-question-paid-only q))
                      (or (not difficulty) (= difficulty (get-question-difficulty q))))
              return q
            finally (error (format nil "try ~a times and got none" try-time))
            ))
    ))

(defun refresh-quiz-cache-cli (cmd)
  (when (clingon:getopt cmd :refresh)
    (fetch-all-quiz-list :cache-file t)
    ))

(defun get-quiz-description-cli-options ()
  `(,(clingon:make-option
      :integer
      :description "the id of quiz"
      :short-name #\n
      :long-name "id"
      :required t
      :key :id
      )
    ,(clingon:make-option
      :string
      :description "output file"
      :short-name #\o
      :long-name "output"
      :key :output
      )
    ,(clingon:make-option
      :flag
      :description "refresh the all quiz cache"
      :short-name #\r
      :long-name "refresh"
      :key :refresh)))

(defun get-quiz-description-cli-handler (cmd)
  (refresh-quiz-cache-cli cmd)
  
  (let ((id (clingon:getopt cmd :id))
        (output (clingon:getopt cmd :output)))
    
    (let (title-slug
          describe
          (quiz (get-question-by-id id)))

      (unless quiz
        (setf *leetcode-all-quiz* nil
              quiz (get-question-by-id id)))

      (setf title-slug (get-question-title-slug quiz))
      
      (setf describe (fetch-question-description title-slug))
      
      (if output
          (write-description-to-md-file
           output
           (describe-clean describe)
           (format nil "https://leetcode.com/problems/~a/" title-slug))
          (format t "Link: https://leetcode.com/problems/~a/
ID: ~a
Title: ~a
~%~a"
                  title-slug
                  (get-question-id quiz)
                  (get-question-title quiz)
                  (describe-clean describe))))
    ))

;;; (clingon:parse-command-line (leetcode-picker-cli) '("get-random-quiz" "-d" "easy" "-r"))
(defun get-random-quiz-cli-options ()
  `(,(clingon:make-option
      :enum
      :description "the difficulty of quiz"
      :short-name #\d
      :long-name "difficulty"
      :key :difficulty
      :items '(("easy" . 1)
               ("medium" . 2)
               ("hard" . 3)))
    ,(clingon:make-option
      :string
      :description "output file"
      :short-name #\o
      :long-name "output"
      :key :output
      )
    ,(clingon:make-option
      :flag
      :description "refresh the all quiz cache"
      :short-name #\r
      :long-name "refresh"
      :key :refresh)))

(defun get-random-quiz-cli-handler (cmd)
  (refresh-quiz-cache-cli cmd)
  (let ((d (clingon:getopt cmd :difficulty))
        (output (clingon:getopt cmd :output))
        quiz)
    
    (setf quiz (random-pick-one-quiz :difficulty d))

    (let (title-slug describe)

      (setf title-slug (get-question-title-slug quiz))

      (setf describe (fetch-question-description title-slug))

      (if output
          (write-description-to-md-file
           output
           (describe-clean describe)
           (format nil "https://leetcode.com/problems/~a/" title-slug))
          (format t "Link: https://leetcode.com/problems/~a/
ID: ~a
Title: ~a
~%~a"
                  title-slug
                  (get-question-id quiz)
                  (get-question-title quiz)
                  (describe-clean describe)))
      )))

(defun leetcode-picker-cli ()
  (clingon:make-command
   :name "leetcode-picker"
   :description "pick the leetcode quiz"
   :version "0.1.0"
   :handler (lambda (cmd) (clingon:print-usage-and-exit cmd t))
   :sub-commands `(,(clingon:make-command
                     :name "get-description"
                     :description "pick the description of leetcode quiz"
                     :options (get-quiz-description-cli-options)
                     :handler #'get-quiz-description-cli-handler
                     )
                   ,(clingon:make-command
                     :name "get-random-quiz"
                     :description "pick a random leetcode quiz"
                     :options (get-random-quiz-cli-options)
                     :handler #'get-random-quiz-cli-handler
                     ))))

(defun main ()
  ;; restore cache
  ;; only error is cache isn't create yet
  (ignore-errors (restore-all-quiz-from-file *leetcode-all-quiz-cache-path*))
  (clingon:run (leetcode-picker-cli)))
