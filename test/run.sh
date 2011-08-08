#!/bin/bash

OUTPUT_DIR=res
REF_DIR=ref
CONF_DIR=conf
ADACTL="../src/adactl -F gnat_short"

function put () {
    local msg="$1"
    local pad=$2
    local ali=$3

    if [ "$3" = "center" ]; then
	lef=$[(${pad}-${#msg})/2]
	printf "%${lef}s%s" "" "${msg}"
	rig=$[${pad}-${#msg}-${lef}]
	printf "%${rig}s" ""
    elif [ "$3" = "right" ]; then
	printf "%-${pad}s" ${msg}
    else
	printf "%${pad}s" ${msg}
    fi
}

function put_line () {
    put "$1" $2 $3
    printf "\n"
}

function put_title_line () {
put "--"
put "$1" 75 center
put_line "--"
}

function put_line_line () {
put      "----------------------------------------"
put_line "---------------------------------------"
}

#
# Initialization
#
if [ -d $OUTPUT_DIR ]; then
    rm -f $OUTPUT_DIR/*
else
    mkdir $OUTPUT_DIR
fi

put_line_line
put_title_line
put_title_line "`${ADACTL} -h 2>&1 | grep ADACTL`"
put_title_line
put_title_line VALIDATION
put_title_line
put_title_line "$(date)"
put_title_line
put_line_line

#
# Framework tests, must be on case-by-case
#
nb_fw=0

test_case=tfw_naming
nb_fw=$((nb_fw+1))
${ADACTL} -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb xfw_naming

test_case=tfw_help
nb_fw=$((nb_fw+1))
${ADACTL} -h all > ${OUTPUT_DIR}/${test_case}.txt 2>&1

test_case=tfw_rule_off
nb_fw=$((nb_fw+1))
${ADACTL} -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb

test_case=tfw_rule_off_ignored
nb_fw=$((nb_fw+1))
${ADACTL} -vwi -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb

test_case=tfw_inhibit
nb_fw=$((nb_fw+1))
${ADACTL} -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}_1.adb ${test_case}_2.adb ${test_case}_3.adb

echo "--- Syntax check test"
test_case=tfw_check
nb_fw=$((nb_fw+1))
${ADACTL} -C -vf conf/x_errors.aru 2>>${OUTPUT_DIR}/${test_case}.txt
for I in ../rules/*.aru; do
   ${ADACTL} -C -vf $I 2>>${OUTPUT_DIR}/${test_case}.txt
done
for I in conf/t_*.aru; do
   ${ADACTL} -C -vf $I 2>>${OUTPUT_DIR}/${test_case}.txt
done

#
# Stress test
# Run all rules over all rules test files.
# We are not interested in the actual output (would be too difficult to analyse),
# just to see if it crashes.
# Result file will contain the context if there is a crash, or just "PASSED" if OK.
#
echo -n "--- Stress test... "
test_case=tfw_stress
nb_fw=$((nb_fw+1))
list=`find ./ -name "t_*.adb" ! -name "x_*.ads" ! -name "x_*.adb" ! -name "*-*" -printf "%P "`
find ./conf -name "t_*.aru" -printf "source conf/%P;\n" | ${ADACTL} -vwd -f - $list \
   1> ${OUTPUT_DIR}/${test_case}.txt 2>&1
result=$?
if [ $result -le 1 ]; then
   echo "PASSED"
   echo "PASSED" > ${OUTPUT_DIR}/${test_case}.txt
else
   echo "FAILED"
fi

#
# Rules test. All tests are of the form t_<rule name>.ad[bs],
# but do not take separate and child units
#

list=`find ./ -name "t_*.adb" ! -name "*-*" -printf "%P "`
nb_rules=0
for i in $list; do
    nb_rules=$((nb_rules+1))
    test_case=`echo $i | cut -f 1 -d "."`
    ${ADACTL} -rvw -f ${CONF_DIR}/${test_case}.aru \
	-o ${OUTPUT_DIR}/${test_case}.txt -u $test_case
    if [ $? == 0 ] ; then
       echo "  => NO ERROR FOUND !!!"
    fi
done

pushd $REF_DIR 1>/dev/null
list=`ls *.txt`
popd 1>/dev/null

nb_passed=0
nb_failed=0
put_line_line
put_title_line "Test result for $nb_rules rules, $nb_fw framework tests"
put_line_line
for test_case in $list; do
    diff=`diff --strip-trailing-cr $OUTPUT_DIR/${test_case} $REF_DIR/${test_case} 2>&1 `
    if [ "$diff" = "" ]; then
	res="       PASSED"
	nb_passed=$((nb_passed+1))
   else
	res="FAILED       "
	nb_failed=$((nb_failed+1))
    fi;

    printf "=> %-60s%-13s <=\n" ${test_case} "$res"
done

put_line_line
put_title_line "Passed= $nb_passed, Failed= $nb_failed"
put_line_line

rm -f *.ali *.adt
