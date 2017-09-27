#!/bin/sh
# Usage:
# ./run.sh [-q] [<adactl options>]
# ./run.sh -h 
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
for E in "../src/adactl.exe" "../src/adactl" "../adactl.exe" "../adactl" $(which adactl); do
   if [ -e $E ] ; then
       EXECUTABLE=$E
       break
   fi
done

if [ -z $EXECUTABLE ] ; then
    echo "No AdaControl executable found"
    exit
fi

case "${1:-}" in
    -q)
        SILENT=1
        shift
        ADACTL="$EXECUTABLE -F gnat_short $*"
        ;;
    -h)
        echo "Usage:"
        echo "   ./run.sh [-q] [<adactl options>]"
        echo "   ./run.sh -h"
        exit
        ;;
    *)
        SILENT=0
        ADACTL="$EXECUTABLE -v -F gnat_short $*"
        ;;
esac


mkdir -p res ref fixed/res fixed/ref
rm -f res/* fixed/res/*.ad[sb]

put_line_line
put_title_line
put_title_line "`${ADACTL} -h version 2>&1 | tr -d \\\\r`"
put_title_line "($EXECUTABLE)"
put_title_line
put_title_line "VALIDATION"
put_title_line
put_title_line "$(date)"
put_title_line
put_line_line

#
# Framework tests, must be on case-by-case
# In alphabetical order
#
run_start=`date +%s`
nb_fw=0

put_line "--- General framework tests"
test_case=tfw_help
nb_fw=$((nb_fw+1))
${ADACTL} -h all 2>&1 \
	| tr -d \\r >res/${test_case}.txt
${ADACTL} -h 2>&1 \
	| tr -d \\r >>res/${test_case}.txt
${ADACTL} -h "^Simplifiable_*" 2>&1 \
	| tr -d \\r >>res/${test_case}.txt
${ADACTL} -h "variables ^tag_*" 2>&1 \
	| tr -d \\r >>res/${test_case}.txt
# Remove line with version number to avoid false failures when using a different version
sed -i "/with ASIS/d" res/${test_case}.txt

# This one has full path names in the result file, the result depends on the directory 
# where it's run from...
# translate \ to / to make independant from OS, keep only the "test/" part of the path
test_case=tfw_formats
nb_fw=$((nb_fw+1))
${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb \
	| tr -d \\r \
        | sed "s%\\\\%/%g; s/^.*test/test/" >res/${test_case}.txt

# This one requires precompilation, or some trees will be missing
gcc -c -gnatct xfw_inhibit.adb tfw_inhibit_?.ad[sb]
test_case=tfw_inhibit
nb_fw=$((nb_fw+1))
${ADACTL} -w -f conf/${test_case}.aru ${test_case}_*.ad[sb] \
	| tr -d \\r >res/${test_case}.txt

test_case=tfw_naming
nb_fw=$((nb_fw+1))
${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb xfw_naming \
	| tr -d \\r >res/${test_case}.txt

test_case=tfw_rule_off
nb_fw=$((nb_fw+1))
${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb \
	| tr -d \\r >res/${test_case}.txt

test_case=tfw_rule_off_ignored
nb_fw=$((nb_fw+1))
${ADACTL} -wi -f conf/${test_case}.aru ${test_case}.adb \
	| tr -d \\r >res/${test_case}.txt

test_case=tfw_rule_off_inverted
nb_fw=$((nb_fw+1))
${ADACTL} -wi -f conf/${test_case}.aru ${test_case}.adb \
	| tr -d \\r >res/${test_case}.txt

test_case=tfw_rule_off_tags
nb_fw=$((nb_fw+1))
${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb \
	| tr -d \\r >res/${test_case}.txt

test_case=tfw_set
nb_fw=$((nb_fw+1))
${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb \
	| tr -d \\r >res/${test_case}.txt

put_line "--- Syntax check test"
test_case=tfw_check
nb_fw=$((nb_fw+1))
# This test requires -v in all cases
${ADACTL} -Cv -f conf/x_errors.aru 2>&1 \
	| tr -d \\r >res/${test_case}.txt
for I in `find ../rules -name "*.aru" | sort -df `; do
   echo -n "$I: " >>res/${test_case}.txt
   ${ADACTL} -Cv -f $I 2>&1 \
	| tr -d \\r >>res/${test_case}.txt
done
for I in `find conf -name "t_*.aru" -o -name "ts_*.aru" | sort -df `; do
   echo -n "$I: " >>res/${test_case}.txt
   ${ADACTL} -Cv -f $I 2>&1 \
	| tr -d \\r >>res/${test_case}.txt
done

#
# Stress test
# Run all rules over all rules test files.
# We are not interested in the actual output (would be too difficult to analyse),
# just to see if it crashes.
# Result file will contain the context if there is a crash, or just "PASSED" if OK.
# We use this test to make some timing statistics; we run therefore with the -i 
# option, otherwise the time spent in Report would hide the true time of rules.
#
put "--- Stress test... "
test_case=tfw_stress
nb_fw=$((nb_fw+1))
list=`find ./ -maxdepth 1 '(' -name "t_*.adb" -or -name "ts_*.adb" -or -name "tfw_*.adb" -or -name "x_*.ads" -or -name "x_*.adb" -or -name "*-*" ')' -printf "%P "`
export ADACTLINI="set timing global;"
result=0
find ./conf -name "t_*.aru" -printf "source conf/%P;\n" | ${ADACTL} -i -F csvx_short -wd -f - $list \
   1> res/${test_case}.txt 2>&1 \
   || result=$?
export ADACTLINI=
# if -x option, return code is always 1
# replace by 10 if there is a crash
if grep -q "=============" res/${test_case}.txt; then
   result=10
fi

# Create timing file
echo "Rule;Time;Percent" >res/rules_timing.csv
grep -E "^[A-Za-z_]+: [0-9.]+" res/${test_case}.txt | sed "s/: /;/;s/ (\([0-9]*.[0-9]*\).*$/;\1/" >>res/rules_timing.csv
###########################################################################
# Put "PASSED" as the result if OK
if [ $result -le 1 ]; then
   put_line "PASSED"
   rm -f res/${test_case}.txt
   echo "PASSED" | tr -d \\r >res/${test_case}.txt
else
   put_line "FAILED ($result)"
fi

#
# Rules test. All tests are of the form t_<rule name>.ad[bs],
# but do not take separate and child units
#
put_line "--- Rules tests"

list=`find ./ -maxdepth 1 -name "t_*.adb" ! -name "*-*" -printf "%P "`
nb_rules=0
for i in $list; do
    nb_rules=$((nb_rules+1))
    test_case=`echo $i | cut -f 1 -d "."`
    ${ADACTL} -ruw -f conf/${test_case}.aru ${test_case} -o res/${test_case}.txt -P res/${test_case}_fix.txt
    dos2unix -q res/${test_case}.txt res/${test_case}_fix.txt
done

#
# Special rules test. All tests are of the form ts_<rule name>.ad[bs]
# For some reasons (explained), these tests cannot be launched automatically
#

# Units. Special because:
#   Must not include some withed units, must include a non-withed unit
nb_rules=$((nb_rules+1))
test_case=ts_units
${ADACTL} -uw -f conf/${test_case}.aru $test_case+x_units_2 \
	| tr -d \\r >res/${test_case}.txt

#
# Tests finalization
#

run_stop=`date +%s`
list=`find ref -name "*.txt" -printf "%P "`

nb_passed=0
nb_failed=0
put_line_line
put_title_line "Test result for $nb_rules rules tests, $nb_fw framework tests"
put_line_line
for test_case in $list; do
    diff=`diff --strip-trailing-cr res/${test_case} ref/${test_case} 2>&1 || true`
    if [ -z "$diff" ]; then
	nb_passed=$((nb_passed+1))
        message="       PASSED"
    else
	nb_failed=$((nb_failed+1))
	message="FAILED       "
    fi;
    if [ $SILENT -eq 0 ]; then
        if [[ ${test_case} =~ _fix.txt$ ]] ; then
	    printf "=> %-60s%-13s <=\n" "  Fix" "$message"
        else
	    printf "=> %-60s%-13s <=\n" ${test_case} "$message"
        fi
    fi
done

put_line_line
put_title_line "`${ADACTL} -h version 2>&1 | tr -d \\\\r`"
put_title_line "Passed= $nb_passed, Failed= $nb_failed"
put_line_line

rm -f *.ali *.adt

if [ $SILENT -eq 1 ] ; then
    if [ $nb_failed -eq 0 ] ; then
	echo PASSED
    else
	echo "FAILED ($nb_failed)"
    fi
else
    print_time "Total run time: " `expr $run_stop - $run_start`
    if [ $nb_failed -ne 0 ] ; then
	. ./failed.sh
    fi

fi
