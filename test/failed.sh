#!/bin/sh
# Checks differences between ./ref/ and ./res/ directories, and offers to view differences if any
# Note that this script can be called with -e set, hence the various  " || true"
# 

###################################################
# Set according to preferences

# Name of patch file if you respond "p":
PATCHFILE=testpatch.diff

# Utilities according to your OS
case $(uname) in
    *win*|*Win*|*WIN*)  # Windows, Cygwin...
	MERGER='c:/Program Files (x86)/Meld/Meld.exe'
	EDITOR=emacs.bat
	;;
    *)                  # Assume *nix
	MERGER='/usr/bin/meld'
	EDITOR=emacs
	;;
esac

# Nothing to set below this line
###################################################

if [ \( ! -d res \) -o \( ! -d ref \) ] ; then
    echo "Missing res or ref directory";
    exit
fi

(cd res
    found=0
    list=`find ./ -maxdepth 1 \( -name "*.txt" -or -name "*.ad[sb]" \) -printf "%P "`
    for test_case in $list ; do
        if [ -e ../ref/${test_case} ] ; then
	    diff=`diff ${test_case} ../ref/${test_case}` || true
	    if [ "$diff" != "" ]; then
                found=1
	        while true ; do
		    echo -n "$test_case: [Diff, Edit, Interactive, save Patch, Validate, Nothing, Quit]? "
		    read REP
		    case $REP in
	                d | D )
                            echo "           <... ref, >... res"
			    diff ../ref/${test_case} ${test_case} 2>/dev/null || true ;;
                        e | E )
			    ${EDITOR} ${test_case} || true ;;
	                i | I )
			    "$MERGER" ../ref/${test_case} ${test_case} 2>/dev/null || true ;;
		        p | P )
			    diff --unified ../ref/${test_case} ${test_case}  >>../${PATCHFILE} 2>/dev/null || true ;
			    echo "Patch appended to $PATCHFILE";;
	                v | V )
			    cp  ${test_case} ../ref/${test_case} || true
			    break;;
		        n | N )
			    break;;
		        q | Q )
			    exit;;
		    esac
	        done
	    fi;
        else
	    while true ; do
		echo -n "No reference for $test_case: [Edit, Validate, Nothing, Quit]? "
		read REP
		case $REP in
                    e | E )
			${EDITOR} ${test_case} || true ;;
	            v | V )
			cp  ${test_case} ../ref/${test_case} || true 
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

