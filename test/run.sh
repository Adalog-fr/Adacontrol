#!/bin/sh
# Run AdaControl's test suite
#
# Usage:
# ./run.sh [-q] [-s] [<adactl options>]
# ./run.sh -h 
# -h:
#   print help and exit
# -q:
#   run in "quiet" mode: just print PASSED if there are no errors
#   names of failing tests are still reported in case of errors
# -s:
#   speed up test: do not run framework and stress tests
# Other options are passed to AdaControl (notably -d)

#
# include functions
#
. ./run_funcs.sh

help() {
    echo "Usage:"
    echo "   ./run.sh [-q] [-s] [<adactl options>]"
    echo "   ./run.sh -h"
    echo "   -q: quiet mode, no display"
    echo "   -s: speedup mode, no framework test"
}

########################################################
# Actual beginning
########################################################

#
# Initialization
#

# Default value for gcc, unless already set in the environment.
if test "x$GCC" = x; then
    GCC=gcc
fi

#Check OS
case $(uname) in
    *win*|*Win*|*WIN*)  # Windows, Cygwin...
	EXT=".exe"
	;;
    *)                  # Assume *nix
	EXT=""
	;;
esac

for E in "../src/adactl$EXT" "../adactl$EXT" $(which adactl); do
   if [ -e $E ] ; then
       EXECUTABLE=$E
       break
   fi
done

if [ -z $EXECUTABLE ] ; then
    echo "No AdaControl executable found"
    exit
fi
PRECOMP="`$EXECUTABLE -h generator 2>&1 | tr -d [:cntrl:] | tr \\\\\\\\ /` -c -gnatct"

# Check if Ada95 is supported
echo "procedure junk is begin null; end;" >junk.adb

$GCC -c -gnat95 junk.adb 2>/dev/null
if [ $? = 0 ] ; then
    SUPPORT95=1
else
    SUPPORT95=0
fi
rm -f junk.*

# We set -e to stop immediatly in case of a problem with the script
# Note that it forces adding a " || True" to commands where the expected status is not 0
set -e
SILENT=0
SPEEDUP=0
EXTRA_OPTS="-v"

while true ; do
    case "${1:-}" in
        -h)
            help
            exit
            ;;
        -q)
            SILENT=1
            EXTRA_OPTS=""
            shift
            ;;
        -s)
            SPEEDUP=1
            shift
            ;;
        -*)
            echo "Unknown option ${1:-}"
            help
            exit
            ;;
        *)
            break
            ;;
    esac
done

ADACTL="$EXECUTABLE $EXTRA_OPTS -F gnat_short -G search $*"

mkdir -p res ref fixed/res fixed/ref
rm -f res/* fixed/res/*.ad[sb]

put_line_line
put_title_line
put_title_line "`${ADACTL} -h version 2>&1 | tr -d \\\\r`"
put_title_line "($EXECUTABLE)"
put_title_line
if [ $SUPPORT95 = 1 ] ; then
    put_title_line "VALIDATION WITH ADA 95 SUPPORT"
else
    put_title_line "VALIDATION WITHOUT ADA 95 SUPPORT"
fi
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

if [ $SPEEDUP = 0 ] ; then
    put_line "--- General framework tests"

    test_case=tfw_fixes
    nb_fw=$((nb_fw+1))
    ${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb \
    | tr -d \\r >res/${test_case}.txt

    # This one has full path names in the result file, the result depends on the directory 
    # where it's run from...
    # translate \ to / to make independant from OS, keep only the "test/" part of the path
    # Also, the first line contains a date and time, remove all digits
    test_case=tfw_formats
    nb_fw=$((nb_fw+1))
    ${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb \
	| tr -d \\r \
        | sed "1s/[0-9]//g; s%\\\\%/%g; s/^.*test/test/" >res/${test_case}.txt

    # Check GPR project file. Use the raw executable, since we want to check that
    # options are taken from the project file
    test_case=tfw_gpr
    nb_fw=$((nb_fw+1))
    ${EXECUTABLE} -p ${test_case}.gpr \
	| tr -d \\r >res/${test_case}.txt

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
    # Remove line with version number to avoid false failures when using a different GNAT version
    sed -i "/with ASIS/s/with ASIS.*$//" res/${test_case}.txt

    # This one requires precompilation, or some trees will be missing
    $PRECOMP xfw_inhibit.adb tfw_inhibit_?.ad[sb]
    test_case=tfw_inhibit
    nb_fw=$((nb_fw+1))
    ${ADACTL} -w -f conf/${test_case}.aru ${test_case}_*.ad[sb] \
	| tr -d \\r >res/${test_case}.txt

    test_case=tfw_naming
    nb_fw=$((nb_fw+1))
    ${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb xfw_naming \
	| tr -d \\r >res/${test_case}.txt

    test_case=tfw_rule_file_off
    nb_fw=$((nb_fw+1))
    ${ADACTL} -w -f conf/${test_case}.aru ${test_case}.adb \
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

    test_case=tfw_object_tracker
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
    if [ $SUPPORT95 = 1 ] ; then
        list="$list t95_*.adb"
    fi
    export ADACTLINI="set timing global;"
    result=0
    find ./conf -name "t_*.aru" -printf "source conf/%P;\n" | ${ADACTL} -i -F csvx_short -wd -f - $list \
        1> res/${test_case}.txt 2>&1 \
        || result=$?
    export ADACTLINI=
    # if adactl is run with -x option, return code is always 1
    # replace by 10 if there is a crash
    if grep -q "=============" res/${test_case}.txt; then
        result=10
    fi

    ###########################################################################
    # Put "PASSED" as the result if OK
    if [ $result -le 1 ]; then
    # Create timing file
        echo "Rule;Time;Percent" >res/rules_timing.csv
        grep -E "^[A-Za-z_]+: [0-9.]+ \([0-9.]+%\)" res/${test_case}.txt >>res/rules_timing.csv
        put_line "PASSED"
        rm -f res/${test_case}.txt
        echo "PASSED" | tr -d \\r >res/${test_case}.txt
    else
        put_line "FAILED ($result)"
    fi

    # Clean-up: makes remaining faster, and avoids tests failing due to different compilation order
    rm *.ali *.adt
fi

#
# Rules test. All tests are of the form t_<rule name>.ad[bs],
# but do not take separate and child units
#
put_line "--- Rules tests"

list=`find ./ -maxdepth 1 -name "t_*.adb" ! -name "*-*" -printf "%P "`
if [ $SUPPORT95 = 1 ] ; then
    list="$list t95_*.adb"
fi
nb_rules=0
for i in $list; do
    test_case=`echo $i | cut -f 1 -d "."`
    nb_rules=$((nb_rules+1))
    status=0
    ${ADACTL} -ruw -f conf/${test_case}.aru ${test_case} -o res/${test_case}.txt || true
    dos2unix -q res/${test_case}.txt
done

#
# Special rules test. All tests are of the form ts_<rule name>.ad[bs]
# For some reasons (explained), these tests cannot be launched automatically
#

# Units. Special because:
#   Must not include some withed units, must include a non-withed unit
test_case=ts_units
nb_rules=$((nb_rules+1))
${ADACTL} -uw -f conf/${test_case}.aru ${test_case}+x_units_2 -o res/${test_case}.txt || true
dos2unix -q res/${test_case}.txt

#
# Tests finalization
#
            
run_stop=`date +%s`
if  [ $SPEEDUP = 1 ] ; then
    list=`find ref \( -name "t_*.txt" -o -name "ts_*.txt"                      \) -printf "%P "`
else
    list=`find ref \( -name "t_*.txt" -o -name "ts_*.txt" -o -name "tfw_*.txt" \) -printf "%P "`
fi
if [ $SUPPORT95 = 1 ] ; then
    cd ref
    list=`echo $list t95_*.txt`
    cd ..
fi

nb_passed=0
nb_failed=0
nb_new=0
put_line_line
put_title_line "Test result for $nb_rules rules tests, $nb_fw framework tests"
put_line_line
for test_case in $list ; do
    diff=`diff --strip-trailing-cr res/${test_case} ref/${test_case} 2>&1 || true`
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

(cd res
 for test_case in `ls *.txt`; do
     if [ ! -e ../ref/$test_case ] ; then
	 printf "=> %-60s%-13s <=\n" ${test_case} "          NEW"
         nb_new=$((nb_new+1))
     fi;
 done
)

put_line_line
put_title_line "`${ADACTL} -h version 2>&1 | tr -d \\\\r`"
put_title_line "Passed= $nb_passed, Failed= $nb_failed, New= $nb_new"
if [ $((nb_passed+nb_failed+nb_new)) -ne $((nb_rules+$nb_fw)) ] ; then
    put_title_line "INCONSISTENT RULE COUNT, total=$((nb_passed+nb_failed+nb_new)), $nb_rules rules tests, $nb_fw framework tests"
fi
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
