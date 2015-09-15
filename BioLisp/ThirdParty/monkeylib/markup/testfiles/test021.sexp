((:P "This is a paragraph" (:NOTE (:P "And this is a simple note"))
  ". And some more of
the same paragraph and blah blah blah and here's"
  (:NOTE
   (:P "a footnote that
crosses a line boundary"))
  " some more."
  (:NOTE
   (:P "This footnote is itself a
whole document with:")
   (:EXAMPLE "some code in it
  indented line
    a more indented line
unindented line")
   (:P "and some more stuff") (:BLOCKQUOTE "and a blockquote")
   (:P "And a list:")
   (:BULLETS (:ITEM (:P "foo")) (:ITEM (:P "bar")) (:ITEM (:P "baz"))))
  " and the rest of the original paragraph."))