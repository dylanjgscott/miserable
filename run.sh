#!/bin/sh
#Launcher for the company interpreter
#Takes 1 or more arguments
#   - A source file to be interpreted
#   - zero or more args 

numArgs="$#"
#check for interpreter file
if [ -x company ]; then
    #check for args
    if [ $numArgs -lt 1 ]; then
        printf "Wrong number of argumments. Expected one or more but %s given.\n" $numArgs
    else
        #pass args to company and launch
        ./company $@
    fi
else
    #no company
    printf "Misery Loves company please run make and check the README file\n"
fi

