(defpackage :vrp
  (:use :cl-user :common-lisp)
  (:shadow random round)
  (:export with-basic-solution
           pp-solution
           bformat
           build-neighborhood-tree
           exhaustive-exploration
           then
           else))

(in-package :vrp)
