#!/bin/bash

# Author:  JP Massar.

# This script is to be run as the 'user' which is the designated account
# from which the Weblistener/BioLingua is run.  It starts up a Lisp
# which is expected to read that user's cl-init.cl file in that user's
# home directory.  The cl-init.cl file is used to configure the particular
# instance which is run out of that account.

# Four arguments are required:

# 1.  The path to the Lisp executable we will be using as our base.

LISP=$1
NICELISP=$1


# 2.  The path to the root of the Weblistener/BioLingua source, without a '/

WBSRC=$2

# 3.  The path to the directory above the Weblistener visitor directories,
# without a '/'

LOGPATH=$3/system/session-logs

# 4.  The name of the startup file to load, which must be located in the 
# WEBLISTENER-SOURCE directory

WEBLOAD=$WBSRC/$4

# An optional fifth argument is the port number

# An optional sixth argument is the priority to run the process at

#
# This defines the extension for the file containing the output of our
# Lisp/Weblistener/BioLingua startup, and subsequent operation.

TTYPE=bwlscript

# The Lisp startup code.  This file gets loaded when we invoke Lisp below,
# and causes everything to start up.

LISPINIT=$WBSRC/Scripts/new-bwlinit.lisp

echo Lisp executable $LISP
echo Weblistener Source directory $WBSRC
echo Weblistener Lisp startup file $LISPINIT
echo Weblistener load file $WEBLOAD
echo Weblistener port $5

# Create 'system' directory under toplevel log directory if it doesn't
# exist, and create 'session-logs' subdirectory under 'system' if it
# doesn't exist.

mkdir -pv $LOGPATH

# We create the name of a new log file to be based on the current time and date.
# Then we insure a file of that name exists, so we can make a symbolic to it.
# We want to have a symbolic link called 'current' in the log directory
# which will point to the current log file, and a symbolic link called
# 'previous' which will point to the one that used to be current.

echo Calling tsscript.sh $TTYPE $LISP $WBSRC
LOGFILE=$LOGPATH/`$WBSRC/Scripts/tsscript.sh $TTYPE $LISP $WBSRC`
echo Complete logfile path $LOGFILE

rm -f $LOGFILE
echo foo > $LOGFILE
# NEED TO TEST WHETHER 'current' EXISTS!
mv -f $LOGPATH/current $LOGPATH/previous
ln -s $LOGFILE $LOGPATH/current

# Okay, we let her rip...

echo starting Allegro BioWebListener $LISP
echo logging output to $LOGFILE

# if the optional sixth argument is provided it must be a priority number
# (between -20 and +20).  we use the nice command in that case to start up the lisp
# with a nonstandard priority 

if [ $6 ] ; 
 then
     NICELISP="nice -n +$6 $1" ;
    echo process running at priority $6
 else
   echo process running at normal priority
fi

# -- : pass the subsequent arguments into Lisp for processing by
#      SYS:WITH-COMMAND-LINE-ARGUMENTS
# --port : The port.  Will have value 'T' in Lisp if not provided as an
#      argument to the shell.
# --webload :  where to find the software that loads the system.
# >&   : send all output (standard output and standard error) to log file
# &    : Run the Lisp in the background.
# echo : send this text to Lisp as the first thing it reads/evals once it
#        has initialized itself.  (This is the only thing it ever
#        reads/evals since the LISPINIT file puts itself into a sleep loop).

echo "(load \"$LISPINIT\") " | $NICELISP -- --port $5 --webload $WEBLOAD >& $LOGFILE &

echo Weblistener started in background

echo Lisp processes currently running
ps -A -l | grep alisp



