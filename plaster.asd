(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :version "1.0.0"
  :description "A paste bin service for Radiance"
  :homepage "https://Shirakumo.github.io/plaster/"
  :bug-tracker "https://github.com/Shirakumo/plaster/issues"
  :source-control (:git "https://github.com/Shirakumo/plaster.git")
  :components ((:file "module")
               (:file "objects")
               (:file "captcha")
               (:file "frontend")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               (:interface :rate)
               :r-data-model
               :r-clip
               :alexandria
               :crypto-shortcuts
               :cl-ppcre))
