#!/bin/sh

for file in `find . -name \*[A-Z]\* | grep -v CVS`
do
  _l=`echo $file | tr [A-Z] [a-z]`
  ln -sf `pwd`/$file $_l
done
