#!/bin/sh
# Usage:
# ./runfix.sh [-q] [<adactl options>]
# ./runfix.sh -h 
# If -q is given as the first parameter:
#   run in "quiet" mode: just print PASSED if there are no errors
#   names of failing tests are still reported in case of errors
# Other options are passed to AdaControl (notably -d)
# -h : print help and exit

#
# include functions
#
. run_funcs.sh

########################################################
# Actual beginning
########################################################

#
# Initialization
#
for E in "../src/adactl_fix.exe" "../src/adactl_fix" "../adactl_fix.exe" "../adactl_fix" $(which adactl_fix); do
   if [ -e $E ] ; then
       EXECUTABLE=$E
       break
   fi
done

if [ -z $EXECUTABLE ] ; then
    echo "No Adactl_Fix executable found"
    exit
fi

case "${1:-}" in
    -q)
        SILENT=1
        shift
        ADACTL_FIX="$EXECUTABLE $*"
        ;;
    -h)
        echo "Usage:"
        echo "   ./runfix.sh [-q] [<adactl_fix options>]"
        echo "   ./runfix.sh -h"
        exit
        ;;
    *)
        SILENT=0
        ADACTL_FIX="$EXECUTABLE -v $*"
        ;;
esac

mkdir -p fixed/res
rm -f fixed/res/*

put_line_line
put_title_line
put_title_line "`${ADACTL_FIX} -h 2>&1 | head -n 1 | tr -d \\\\r`"
put_title_line "($EXECUTABLE)"
put_title_line
put_title_line "VALIDATION"
put_title_line
put_title_line "$(date)"
put_title_line
put_line_line

#
# Tests
#

list=`find res/ -maxdepth 1 -name "t_*.txt" -printf "%f "`
nb_fixes=0
run_start=`date +%s`
for i in $list; do
    nb_fixes=$((nb_fixes+1))
    ${ADACTL_FIX} res/$i -o fixed/res/
done
dos2unix -q fixed/res/*.ad[sb]

#
# Tests finalization
#

run_stop=`date +%s`
list=`find fixed/ref -name "*.ad[sb]" -printf "%f "`

nb_passed=0
nb_failed=0
put_line_line
put_title_line "Test result for $nb_fixes fix files "
put_line_line
for test_case in $list; do
    diff=`diff --strip-trailing-cr fixed/res/${test_case} fixed/ref/${test_case} 2>&1 || true`
    if [ -z "$diff" ]; then
	nb_passed=$((nb_passed+1))
        message="       PASSED"
    else
	nb_failed=$((nb_failed+1))
	message="FAILED       "
    fi;
    if [ $SILENT -eq 0 ]; then
	printf "=> %-60s%-13s <=\n" ${test_case} "$message"
    fi
done

put_line_line
put_title_line "`${ADACTL_FIX} -h 2>&1 | head -n 1 | tr -d \\\\r`"
put_title_line "Passed= $nb_passed, Failed= $nb_failed"
put_line_line

if [ $SILENT -eq 1 ] ; then
    if [ $nb_failed -eq 0 ] ; then
	echo PASSED
    else
	echo "FAILED ($nb_failed)"
    fi
else
    print_time "Total run time: " `expr $run_stop - $run_start`
    if [ $nb_failed -ne 0 ] ; then
	(cd fixed ; . ../failed.sh)
    fi
fi
