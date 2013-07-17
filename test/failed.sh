#!/bin/bash
if [ $OSTYPE == "linux-gnu" ] ; then
    WINMERGE='/usr/bin/meld'
else
    WINMERGE='/cygdrive/c/Program Files (x86)/KDiff3/kdiff3.exe'
fi

(cd res
    for test_case in *.txt; do
	diff=`diff ${test_case} ../ref/${test_case}`
	if [ "$diff" != "" ]; then
	    while true ; do
		echo -n "$test_case: [Diff, Interactive, Validate, Nothing, Quit]? "
		read REP
		case $REP in
	            d | D )
			diff ${test_case} ../ref/${test_case} 2>/dev/null;;
	            i | I )
			"$WINMERGE" ${test_case} ../ref/${test_case} 2>/dev/null;;
	            v | V )
			cp  ${test_case} ../ref/${test_case}
			break;;
		    n | N )
			break;;
		    q | Q )
			exit;;
		esac
	    done
	fi;
    done
)
