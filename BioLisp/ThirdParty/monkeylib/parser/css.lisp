(in-package :com.gigamonkeys.css)

;; %option case-insensitive

(defun hex-digit-char-p (char) (digit-char-p char 16))
(defun non-ascii-char-p (char) (<= #x80 (char-code char) #x10ffff))

(defchartype any-char '(satisfies characterp))
(defchartype ascii-letter '(member '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
				     #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)))

(defchartype digit '(satisfies digit-char-p))

(defchartype h ;; [0-9a-f]
  '(satisfies hex-digit-char-p))

(defchartype nonascii ;; [\200-\377]
  '(satisfies non-ascii-char-p))

(defprod unicode () ;; \\{h}{1,6}(\r\n|[ \t\r\n\f])?
  (#\\ (+ h) (? (/ (#\Return #\Newline) (/ #\Space #\Tab #\Return #\Newline)))))

(defprod escape () ;; {unicode}|\\[^\r\n\f0-9a-f]
  (/ unicode (#\\ (& any-char (! (/ #\Return #\Newline #\Page h))))))

(defprod nmstart () ;; [_a-z]|{nonascii}|{escape}
  (/ #\_ ascii-letter nonascii escape))

(defprod nmchar () ;; [_a-z0-9-]|{nonascii}|{escape}
  (/ #\_ ascii-letter #\- nonascii escape))

(defprod string1 () ;; \"([^\n\r\f\\"]|\\{nl}|{escape})*\"
  (#\" 
   (* (/ (& any-char (! (/ #\Newline #\Return #\Page #\")))
	 (#\\ nl)
	 escape))
   #\"))
         
(defprod string2 () ;; \'([^\n\r\f\\']|\\{nl}|{escape})*\'
  (#\' 
   (* (/ (& any-char (! (/ #\Newline #\Return #\Page #\")))
	 (#\\ nl)
	 escape))
   #\'))

(defprod invalid1 () ;; \"([^\n\r\f\\"]|\\{nl}|{escape})*
  (#\" 
   (* (/ (& any-char (! (/ #\Newline #\Return #\Page #\")))
	 (#\\ nl)
	 escape))))

(defprod invalid2 () ;; \'([^\n\r\f\\']|\\{nl}|{escape})*
  (#\1
   (* (/ (& any-char (! (/ #\Newline #\Return #\Page #\")))
	 (#\\ nl)
	 escape))))

(defprod ident () ;; -?{nmstart}{nmchar}*
  ((? #\-) nmstart (* nmchar)))

(defprod name () ;; {nmchar}+
  (+ nmchar))

(defprod num () ;; [0-9]+|[0-9]*"."[0-9]+
  (/ (+ digit) ((* digit) #\. (+ digit))))

(defprod string () ;; {string1}|{string2}
  (/ string1 string2))

(defprod invalid () ;; {invalid1}|{invalid2}
  (/ invalid1 invalid2))

(defprod url () ;; ([!#$%&*-~]|{nonascii}|{escape})*
  (* (/ #\! #\# #\$ #\% #\& #\* #\- #\~ nonascii escape)))

(defprod s () ;; [ \t\r\n\f]
  (/ #\Space #\Tab #\Return #\Newline #\Page))

(defprod w () ;; {s}*
  (* s))

(defprod nl () ;; \n|\r\n|\r|\f
  (/ #\Newline (#\Return #\Newline) #\Return #\Page))

(defprod A () ;; a|\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?
  )

(defprod C () ;; c|\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?
  )

(defprod D () ;; d|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
  )

(defprod E () ;; e|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
  )

(defprod G () ;; g|\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?|\\g
  )

(defprod H () ;; h|\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?|\\h
  )

(defprod I () ;; i|\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?|\\i
  )

(defprod K () ;; k|\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?|\\k
  )

(defprod M () ;; m|\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?|\\m
  )

(defprod N () ;; n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
  )

(defprod P () ;; p|\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?|\\p
  )

(defprod R () ;; r|\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\r
  )

(defprod S () ;; s|\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?|\\s
  )

(defprod T () ;; t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
  )

(defprod X () ;; x|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\x
  )

(defprod Z () ;; z|\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?|\\z
  )


%%

{s}+			{return S;}

\/\*[^*]*\*+([^/*][^*]*\*+)*\/		/* ignore comments */
{s}+\/\*[^*]*\*+([^/*][^*]*\*+)*\/	{unput(' '); /*replace by space*/}

"<!--"		{return CDO;}
"-->"			{return CDC;}
"~="			{return INCLUDES;}
"|="			{return DASHMATCH;}

{w}"{"			{return LBRACE;}
{w}"+"			{return PLUS;}
{w}">"			{return GREATER;}
{w}","			{return COMMA;}

{string}		{return STRING;}
{invalid}		{return INVALID; /* unclosed string */}

{ident}			{return IDENT;}

"#"{name}		{return HASH;}

"@import"		{return IMPORT_SYM;}
"@page"			{return PAGE_SYM;}
"@media"		{return MEDIA_SYM;}
"@charset"		{return CHARSET_SYM;}

"!"{w}"important"	{return IMPORTANT_SYM;}

{num}{E}{M}		{return EMS;}
{num}{E}{X}		{return EXS;}
{num}{P}{X}		{return LENGTH;}
{num}{C}{M}		{return LENGTH;}
{num}{M}{M}		{return LENGTH;}
{num}{I}{N}		{return LENGTH;}
{num}{P}{T}		{return LENGTH;}
{num}{P}{C}		{return LENGTH;}
{num}{D}{E}{G}		{return ANGLE;}
{num}{R}{A}{D}		{return ANGLE;}
{num}{G}{R}{A}{D}	{return ANGLE;}
{num}{M}{S}		{return TIME;}
{num}{S}		{return TIME;}
{num}{H}{Z}		{return FREQ;}
{num}{K}{H}{Z}		{return FREQ;}
{num}{ident}		{return DIMENSION;}

{num}%			{return PERCENTAGE;}
{num}			{return NUMBER;}

"url("{w}{string}{w}")"	{return URI;}
"url("{w}{url}{w}")"	{return URI;}
{ident}"("		{return FUNCTION;}

.			{return *yytext;}