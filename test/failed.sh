#!/bin/sh

PATCHFILE=testpatch.diff

if [ "${OSTYPE:-}" = "linux-gnu" ] ; then
    WINMERGE='/usr/bin/meld'
    EMACS=emacs
else
    WINMERGE='c:/Program Files (x86)/Meld/Meld.exe'
    EMACS=emacs.bat
fi

if [ \( ! -d res \) -o \( ! -d ref \) ] ; then
    echo "Missing res or ref directory";
    exit
fi

(cd res
    found=0
    for test_case in *.txt *.ad[sb] ; do
	diff=`diff ${test_case} ../ref/${test_case}`
	if [ "$diff" != "" ]; then
            found=1
	    while true ; do
		echo -n "$test_case: [Diff, Edit, Interactive, save Patch, Validate, Nothing, Quit]? "
		read REP
		case $REP in
	            d | D )
                        echo "           <... ref, >... res"
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
    if [ $found -eq 0 ] ; then
        echo "*** PASSED"
    fi
)

