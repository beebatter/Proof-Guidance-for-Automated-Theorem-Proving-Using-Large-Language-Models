#!/bin/bash

#set -x

DIR=`dirname "$0"`

"$DIR/eprover" $@  2>&1 | grep  "\['final'\|'proof'\]" | grep cnf

