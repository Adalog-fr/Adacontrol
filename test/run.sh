#!/bin/bash

OUTPUT_DIR=res
REF_DIR=ref
CONF_DIR=conf
ADACTL=../src/adactl

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

test_case=tfw_naming
${ADACTL} -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb xfw_naming.ads xfw_pack

test_case=tfw_help
${ADACTL} -h all > ${OUTPUT_DIR}/${test_case}.txt 2>&1

test_case=tfw_rule_off
${ADACTL} -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb 

test_case=tfw_rule_off_ignored
${ADACTL} -vwi -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb 

test_case=tfw_inhibit
${ADACTL} -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}_1.adb ${test_case}_2.adb ${test_case}_3.adb 

#
# Stress test
# Run all rules over all rules test files.
# We are not interested in the actual output (would be too difficult to analyse),
# just to see if it crashes.
# Result file will contains the context if there is a crash, or just "PASSED" if OK.
#
echo "--- Stress test..."
test_case=tfw_stress
list=`find ./ -name "t_*.adb" ! -name "*-*" -printf "%P "`
find ./conf -name "t_*.aru" -printf "source conf/%P;\n" | ${ADACTL} -vwd -f - $list \
   2>&1 1>/dev/null | tee ${OUTPUT_DIR}/${test_case}.txt
if [ $? -le 1 ]; then
   echo "PASSED" > ${OUTPUT_DIR}/${test_case}.txt
fi
echo "--- End stress test..."

#
# Rules test. All tests are of the form t_<rule name>.ad[bs], 
# but do not take separate and child units
#

list=`find ./ -name "t_*.adb" ! -name "*-*" -printf "%P "`
for i in $list; do
    test_case=`echo $i | cut -f 1 -d "."`
    ${ADACTL} -vw -f ${CONF_DIR}/${test_case}.aru \
	-o ${OUTPUT_DIR}/${test_case}.txt -u $test_case
done

cd ref
list=`ls *.txt`
cd ..

for test_case in $list; do
    diff=`diff $OUTPUT_DIR/${test_case} $REF_DIR/${test_case} \
          2>&1`

    if [ "$diff" = "" ]; then
	res="       PASSED"
    else
	res="FAILED       "
    fi;

    printf "=> %-60s%-13s <=\n" ${test_case} "$res"
done

put_title_line 
put_line_line

rm -f *.ali *.adt