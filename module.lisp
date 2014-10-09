#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:plaster
  (:shadowing-import-from #:radiance #:make-keyword)
  (:use #:cl #:radiance #:lquery #:alexandria))
