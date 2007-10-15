;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:umpa-system
  (:use #:cl #:asdf))

(in-package #:umpa-system)

(defsystem "umpa-lumpa"
    :description "your standard set of personal scratch an itch snippets"
    :version "0.1"
    :author "Ties Stuij <ties@stuij.se>"
    :license "LLGPL"
    :depends-on (:arnesi)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "helpers" :depends-on ("packages"))))))