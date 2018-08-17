#|
 This file is a part of Purplish
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :version "1.0.0"
  :description "A paste bin service for Radiance"
  :homepage "https://Shirakumo.github.io/plaster/"
  :bug-tracker "https://github.com/Shirakumo/plaster/issues"
  :source-control (:git "https://github.com/Shirakumo/plaster.git")
  :components ((:file "module")
               (:file "objects")
               (:file "frontend")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               (:interface :rate)
               :r-data-model
               :r-clip
               :crypto-shortcuts))
