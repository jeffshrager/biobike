(in-package :cl-user)

(defpackage :com.gigamonkeys.json.parser
  (:use :cl :com.gigamonkeys.parser)
  (:shadow :string :exp)
  (:export :parse-json))

(defpackage :com.gigamonkeys.json
  (:use :cl
	:com.gigamonkeys.foo
	:com.gigamonkeys.foo.javascript
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.json.parser)
  (:import-from :com.gigamonkeys.foo.javascript 
		:new-env
		:statement-or-expression
		:top-level-environment)
  (:import-from :com.gigamonkeys.foo.text-output :new-pretty-printer)
  (:export :json :parse-json :to-json))
