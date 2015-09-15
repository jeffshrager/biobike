;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar                                            |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

#+(or :allegro :lispworks)
(asdf:operate 
 'asdf:load-op  :com.gigamonkeys.json
 :force cl-user::*force-load-system*-recompile*)

(defparameter *nvpl-files* 
  '(
    "packages"

    "sseqview-defs"
    "sseqview"

    "unique-ids"
    "defs"
    #-:sframes
    "non-sframes" 
    #+:sframes
    "sframes"
    "utils"
    "sexp"

    "bnf"
    "snippet-defs"
    "defgenerics"
    "snippet-hash"
    
    "errors"
    "receive-client-messages"
    "server-init"
    "shared-sessions"
    "workspace-history"
    "commands"
    "input-boxes"
    "cut-and-paste"
    "execute"
    "result-handlers"
    "send-client-messages"
    
    "snippet-creation"
    "template-code"
    "operators"
    "utility-operators"
    "display-sessions"
    "surround"
    "help"
    "clear-and-delete"
    "keys" 
    "repl"
    "background"
    "boxes"
    "boxes-to-json"
    "box-flags"
    "codegen"
    "loop-templates"
    "templates"
    
    "menus"
    "palette-menu-tools"
    "snippet-menu-tools"
    "palette-menus"
    "organism-submenus"
    "dynamic-palette-menus"
    "module-menus"
    "snippet-menus"

    "drag-and-drop"

    "nsw"
    "user-sessions"
    "autosave"
    
    "memory-usage"
    
    ))


(load-system* "websrc:vplcode;" *nvpl-files*)

(when (fboundp 'provides) (funcall 'provides :server-vpl))


