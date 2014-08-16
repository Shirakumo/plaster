#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem plaster
  :class "radiance:module"
  :defsystem-depends-on (:radiance)
  :name "Plaster Pasting Service" 
  :author "Nicolas Hafner"
  :version "0.0.1" 
  :license "Artistic" 
  :homepage "http://github.com/Shinmera/radiance-plaster"
  :components ((:file "module")
               (:file "frontend")
               ;;(:file "backend")
               (:file "pastebin-type-map")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :data-model)
               (:interface :user)
               (:interface :profile)
               (:interface :auth)
               :alexandria
               :crypto-shortcuts
               :drakma
               :cl-ppcre))
