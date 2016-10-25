#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:plaster)
(defvar *default-salt* "gHjjaL213adjz9AC")
(defvar *user-pastes-per-page* 20)
(defvar *public-pastes-limit* 20)

(define-trigger db:connected ()
  ;; PID: -1 or _id of annotated parent.
  ;; Type: Has to be a type-name in plaster-types.
  ;; View: 0 Public, 1 Unlisted, 2 Private, 3 Encrypted
  (db:create 'plaster '((pid :id) (title (:varchar 64)) (author (:varchar 32)) (type (:varchar 32))
                        (time (:integer 5)) (text :text) (view (:integer 2)) (hits :integer) (ip (:varchar 16)))
             :indices '(pid author ip))
  (db:create 'plaster-types '((title (:varchar 32)) (name (:varchar 64)) (mime (:varchar 32))))
  (db:create 'plaster-users '((user (:varchar 32)) (theme (:varchar 32)) (default-type (:varchar 32))) :indices '(user))
  (db:create 'plaster-themes '((title (:varchar 32)) (name (:varchar 32)))))

(defun id->hash (id) (write-to-string id :base 36))

(defun hash->id (hash) (or (parse-integer (or hash "") :radix 36 :junk-allowed T) -1))

(defun user-url (user)
  (if (string-equal (if (stringp user) user (user:username user)) "anonymous")
      ""
      (format NIL "/user/~a" (user:username (if (stringp user) (user:get user) user)))))

(defun paste-url (id &optional annot)
  (format NIL "/view/~a~@[#annotation-~a~]" (id->hash id) (when annot (id->hash annot))))

(defun new-url (&key repaste annotate)
  (cond
    (repaste (format NIL "/new?repaste=~a" (id->hash repaste)))
    (annotate (format NIL "/new?annotate=~a" (id->hash annotate)))))

(defun view->name (view)
  (case view
    (0 "Public")
    (1 "Unlisted")
    (2 "Private")
    (3 "Encrypted")
    (T "???")))

(defun type->title (type)
  (gethash "title" (first (db:select 'plaster-types (db:query (:= 'mime type)) :fields '(title) :amount 1))))

(defun type->mode (type)
  (gethash "name" (first (db:select 'plaster-types (db:query (:= 'mime type)) :fields '(name) :amount 1))))

(defun format-time (time)
  (local-time:format-timestring
   NIL (local-time:unix-to-timestamp time)
   :format '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2))))

(defmacro string-or (default &rest values)
  (let ((var (gensym)) (def (gensym)))
    `(let* ((,def ,default)
            (,var (or ,@values "")))
       (if (= 0 (length ,var)) ,def ,var))))

(defun crlf->lf (string)
  (cl-ppcre:regex-replace-all (format NIL "~C~C" #\return #\linefeed) string (string #\linefeed)))

(defun encrypt (text password)
  (let ((salt (defaulted-config *default-salt* :encrypt-salt)))
    (when (< (length text) 16)
      (setf text (concatenate 'string text (make-string (- 16 (length text)) :initial-element #\Space))))
    (concatenate
     'string
     (cryptos:simple-hash text salt :iterations 1 :digest 'ironclad:md5) "-"
     (cryptos:encrypt text (cryptos:pbkdf2-key password salt :digest :sha256 :iterations 1)))))

(defun decrypt (text password)
  (destructuring-bind (hash text) (cl-ppcre:split "-" text)
    (let* ((salt (defaulted-config *default-salt* :encrypt-salt))
           (decrypted (cryptos:decrypt text (cryptos:pbkdf2-key password salt :digest :sha256 :iterations 1)))
           (hashed (cryptos:simple-hash decrypted salt :iterations 1 :digest 'ironclad:md5)))
      (when (string-equal hashed hash)
        decrypted))))

(defun paste-accessible-p (paste &optional (user (auth:current)))
  ;; The order of tests is significant since otherwise the decryption side-effect
  ;; cannot take place. This is worse for performance on average, but dismissable.
  (and paste
       (or (and (or (not (= (dm:field paste "view") 3))
                    (and (post/get "password")
                         (< 0 (length (post/get "password")))
                         ;; We've come this far, decrypt it and set it so we don't have to do it twice.
                         (setf (dm:field paste "text")
                               (decrypt (dm:field paste "text") (post/get "password")))))
                (or (not (= (dm:field paste "view") 2))
                    (and user (string-equal (user:username user) (dm:field paste "author"))))
                ;; View permissions cascade from parent, so check it.
                (or (= (dm:field paste "pid") -1)
                    (paste-accessible-p (dm:get-one 'plaster (db:query (:= '_id (dm:field paste "pid")))) user)))
           (and user (user:check user (perm plaster admin))))))

(defparameter *captcha-salt* (make-random-string))
(defparameter *captchas* '("divisible" "determined" "questionable" "difficult" "simplistic" "always" "never" "however" "occasionally" "certainly"
                           "creative" "video" "games" "whatever" "realistic" "severe" "explosion" "wizard" "witch" "confederation"
                           "united" "guess" "estimate" "uncertainty" "forgetful" "loathing" "nevermind" "incorrect" "detective" "deduction"
                           "reasoning" "evidence" "incident" "curiosity" "thoughtful" "assemble" "story" "conclusion" "possibility" "culprit"
                           "solved" "probability" "equation" "careful" "consider" "detail" "problematic" "complication" "comparison" "doubt"))
(defun generate-captcha ()
  (let* ((el (random-elt *captchas*))
         (elmix (copy-seq el)))
    (loop for i from 0 below 2
          do (setf (elt elmix (+ (random (- (length el) 2)) 1)) #\-))
    (values
     elmix
     (cryptos:pbkdf2-hash el *captcha-salt*))))

(defun paste-editable-p (paste user)
  (and user (or (user:check user (perm plaster admin))
                (string-equal (dm:field paste "author") (user:username user)))))

(defmacro assert-api (&body forms)
  "Assert multiple things at once.
Each form should be of the following format:
 (ASSERTION-FORM &rest EXTRA-ARGS)"
  `(progn
     ,@(mapcar #'(lambda (form)
                   `(unless ,(car form) (error 'api-error :message ,@(cdr form))))
               forms)))
