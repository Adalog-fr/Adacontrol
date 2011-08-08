#!/bin/bash

OUTPUT_DIR=res
REF_DIR=ref
CONF_DIR=conf

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
put_title_line 
put_title_line VALIDATION
put_title_line 
put_title_line "$(date)"
put_title_line 
put_title_line 
put_line_line
put_title_line 

test_case=test_case_001

../src/adactl -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb pack_1.ads

test_case=test_case_002

../src/adactl -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb 

test_case=test_case_003

../src/adactl -h all > ${OUTPUT_DIR}/${test_case}.txt 2>&1

test_case=test_case_004

../src/adactl -vw -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb 

test_case=test_case_005

../src/adactl -vwi -f ${CONF_DIR}/${test_case}.aru \
    -o ${OUTPUT_DIR}/${test_case}.txt ${test_case}.adb 

list=`ls test_case_1??.ad[sb]`

for i in $list; do
    test_case=`echo $i | cut -f 1 -d "."`
    ../src/adactl -vw -f ${CONF_DIR}/${test_case}.aru \
	-o ${OUTPUT_DIR}/${test_case}.txt -u $test_case
done

list=`ls test_case_[01]??.adb`

for i in $list; do
    test_case=`echo $i | cut -f 1 -d "."`
    diff=`diff $OUTPUT_DIR/${test_case=}.txt $REF_DIR/${test_case=}.txt \
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