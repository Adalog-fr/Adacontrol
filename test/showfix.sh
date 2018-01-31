#!/bin/sh
# Compare the fixed files as produced by runfix.sh with the original ones
# Allows to launch a diff tool to view the patches
#
# usage: 
#   ./showfix [<pattern>]
#   ./showfix -h

if [ "${OSTYPE:-}" = "linux-gnu" ] ; then
    MERGE='/usr/bin/meld'
else
    MERGE='c:/Program Files (x86)/Meld/Meld.exe'
fi

if [ "$1" = "-h" ] ; then
    echo "usage: showfix [<pattern>]"
    exit
fi

list=`find fixed/res -maxdepth 1 -name "${1:-*}.ad[sb]" -printf "%f "`
nb_fix=$(echo $list | wc -w)
current=0
for f in $list ; do
    while true ; do
        current=$((current+1))
	echo -n "$current/$nb_fix $f: [Diff, Interactive, Nothing, Quit]? "
	read REP
	case $REP in
	    d | D )
                echo "           <... ref, >... res"
		diff "$f" "fixed/res/$f"  2>/dev/null
                break;;
	    i | I )
                "$MERGE" "$f" "fixed/res/$f"
                break;;
	    n | N )
		break;;
	    q | Q )
		exit;;
	esac
    done
done
