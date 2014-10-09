#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem plaster
  :class "radiance:module"
  :defsystem-depends-on (:radiance) 
  :author "Nicolas Hafner"
  :description "A pasting service for Radiance."
  :version "0.9.1" 
  :license "Artistic" 
  :homepage "http://github.com/Shinmera/plaster"
  :components ((:file "module")
               (:file "toolkit")
               (:file "api")
               (:file "frontend")
               ;;(:file "backend")
               (:file "pastebin-type-map"))
  :depends-on ((:interface :database)
               (:interface :data-model)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               :r-clip
               :alexandria
               :crypto-shortcuts
               :drakma
               :cl-ppcre))
