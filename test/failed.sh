#!/bin/bash

PATCHFILE=testpatch.diff

if [ $OSTYPE == "linux-gnu" ] ; then
    WINMERGE='/usr/bin/meld'
    EMACS=emacs
else
    WINMERGE='/cygdrive/c/Program Files/KDiff3/kdiff3.exe'
    EMACS=emacs.bat
fi

(cd res
    for test_case in *.txt; do
	diff=`diff ${test_case} ../ref/${test_case}`
	if [ "$diff" != "" ]; then
	    while true ; do
		echo -n "$test_case: [Diff, Edit, Interactive, save Patch, Validate, Nothing, Quit]? "
		read REP
		case $REP in
	            d | D )
			diff ../ref/${test_case} ${test_case}  2>/dev/null;;
                    e | E )
			${EMACS} ${test_case};;
	            i | I )
			"$WINMERGE" ../ref/${test_case} ${test_case} 2>/dev/null;;
		    p | P )
			diff --unified ../ref/${test_case} ${test_case}  >>../${PATCHFILE} 2>/dev/null;
			echo "Patch appended to $PATCHFILE";;
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
