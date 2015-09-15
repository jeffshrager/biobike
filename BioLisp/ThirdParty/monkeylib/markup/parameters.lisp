;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

(defparameter *paragraph-tags* '(:paragraph :modeline :item :href :text :td :p :blockquote :example)
  "Tags that should be parsed as paragraphs when they occur at the top level.")

(defparameter *subdocument-tags* '(:subdoc :note :sidebar :bullets :table :tr :link)
  "Tags that should be parsed as documents that can contain paragraphs.")

(defparameter *list-items-tags* '(:numbered-list-item :bulleted-list-item)
  "Tags that should be combined into a list.")

(defparameter *indented-paragraph-tags* '((2 . :blockquote) (4 . :example))
  "Mapping from indentation levels to tag names.")

(defparameter *outline-tag-base* "H"
  "Prefix used to create tag names for Emacs outline header paragraphs.")

(defparameter *invisible* '(:modeline :foo)
  "Tags that are not included in the output of RENDER.")

(defparameter *copyright* "2005, Peter Seibel"
  "Included in copyright notice included in footer of PDF output.")

(defparameter *tab-width* 8
  "Number of spaces to read instead of a tab character in leading
  indentation. Set to NIL to read tabs as tabs.")