#!/bin/sh

if [ "${OSTYPE:-}" = "linux-gnu" ] ; then
    MERGE='/usr/bin/meld'
else
    MERGE='c:/Program Files (x86)/Meld/Meld.exe'
fi

if [ "$1" = "-h" ] ; then
    echo "usage: showfix [<pattern>]"
    exit
fi

list=`find fixed/res -maxdepth 1 -name "${1:-*.ad[sb]}" -printf "%f "`
for f in $list ; do
    while true ; do
	echo -n "$f: [Diff, Interactive, Nothing, Quit]? "
	read REP
	case $REP in
	    d | D )
                echo "           <... ref, >... res"
		diff ../ref/${test_case} ${test_case}  2>/dev/null
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
