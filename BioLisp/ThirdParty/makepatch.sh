#!/bin/sh

# usage: ./makepatch.sh foo foo-1.2.3/ | (cd foo; patch -p1)

diff --new-file --recursive --unified -x .svn -x CVS $1 $2
