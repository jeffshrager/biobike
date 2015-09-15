;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

(defparameter *bbl-implementation-files*
  '(
    "symbols"
    "bbi-package"
    "defs"
    "readtable"
    "brackets"
    "stack"
    "form-processor"
    "df"
    "df-separate"
    "df-parse"
    "df-verify"
    "df-convert"
    "df-codegen"
    "df-mappings"
    "df-call"
    "lambda-lists"
    "bbloader"
    ))

(load-system* "biol:BBL;" *bbl-implementation-files*)

(when (fboundp 'provides) (funcall 'provides :bbl-implementation))


