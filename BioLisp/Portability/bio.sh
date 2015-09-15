#!/bin/sh

if [ x"$SBCL_HOME" != x ]; then
  SBCL=$SBCL_HOME/../../bin/sbcl 
else
  SBCL=`/usr/bin/env sbcl`
fi

BIODIR=${BIODIR-/usr/local/bio}

echo "Using" $SBCL "as SBCL executable."
echo "Starting from" $BIODIR

$SBCL --load $BIODIR/BioLisp/Portability/sbbio.lisp --load $BIODIR/BioLisp/blload.lisp
