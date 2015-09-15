#!/bin/sh

# Author:  JP Massar.

# This sub-script is run from the runbwl-instance.sh script.
# It starts up a Lisp which loads in the timestamp.lisp file
# which causes the name of a log file to be output to standard
# output.

# Three arguments are required:

# 1.  The extension to be given to the logfile, currently 'bwlscript'.
# Thus the full name of the logfile will be <something>.bwlscript
# This extension is passed into the Lisp via the command line via the
# -e flag.

# 2.  The Lisp executable path.

# 3.  The path to the toplevel Weblistener source directory, from which
# we compute the path to the timestamp.lisp file.

LISP=$2
FILE=$3/Scripts/timestamp.lisp

# Old version to get rid of '; Exiting Lisp'
# $LISP -L $FILE -qq -kill -- -e $1 | egrep -v "(^WARNING)|^\;\ Exiting\ Lisp"

# New version to get rid of '; Exiting'
# Newer hack to get rid of anything printed out before the file name.
# Had to make a change to the lisp code to put a new line in front of the file name
$LISP -L $FILE -qq -kill -- -e $1 | egrep -v "(^WARNING)|^\;\ Exiting" | tail -1

