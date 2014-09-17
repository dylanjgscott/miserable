#!/bin/sh
#Launcher for the misery compiler
#Takes 2 arguments:
#   - A file to be comppiled
#   - A destination file for the intermediate language

numArgs="$#"

if [ -x misery ]; then
    #check num args
    if [ $numArgs -ne 2 ]; then
        printf "Wrong number of arguments given. 2 expected but %s given.\n" $numArgs 
    else
        #pass the args to misery and finish
        ./misery $1 > $2
    fi
else
    #no misery found - please create
    echo "No compiler found. Please run make and have a look at the README file"
fi
#./misery
