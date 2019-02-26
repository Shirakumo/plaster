#|
 This file is a part of Purplish
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.plaster)

(defparameter *captcha-words*
  #("endless" "knowledge" "wisdom" "quality" "quantity" "unknown" "speech"
    "drink" "package" "recipient" "password" "sparkle" "helping"
    "answer" "reason" "question" "captcha" "family" "friend" "awesome"
    "great" "amazing" "something" "anything" "somewhere" "someone"
    "negative" "positive"))

(define-trigger radiance:startup ()
  (defaulted-config (make-random-string) :captcha-sign-key))

(defun encrypt-captcha-solution (solution)
  (cryptos:encrypt (format NIL "~a~a" (make-random-string 16) solution) (config :captcha-sign-key)))

(defun decrypt-captcha-solution (solution)
  (subseq (cryptos:decrypt solution (config :captcha-sign-key)) 16))

(defun captcha-correct-p (entered encrypted-solution)
  (string= entered
           (decrypt-captcha-solution encrypted-solution)))

(defun scrambled (string &optional (start 0) (end (length string)))
  (let ((sub (subseq string start end)))
    (rotatef (aref sub 1)
             (aref sub (max 1 (- (length sub) 2))))
    (dotimes (i (min 2 (round (/ (length sub) 2))) sub)
      (rotatef (aref sub (random (length sub)))
               (aref sub (random (length sub)))))))

(defun generate-captcha ()
  (let* ((word (svref *captcha-words*
                      (random (length *captcha-words*))))
         (scrambled (format NIL "~c~a~c"
                            (char word 0)
                            (scrambled word 1 (1- (length word)))
                            (char word (1- (length word))))))
    (values scrambled word)))
