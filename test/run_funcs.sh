# functions used by run*.sh scripts

# Parameters:
# $1: Message
# $2 (opt): size for padding 
# $3 (opt): right | center | left (default)
put () {
    if [ $SILENT -eq 1 ]; then return; fi

    local msg="$1"
    local pad="${2:-}"
    local ali="${3:-}"

    if [ "$ali" = "center" ]; then
	lef=$(((${pad}-${#msg})/2))
	printf "%${lef}s%s" "" "${msg}"
	rig=$((${pad}-${#msg}-${lef}))
	printf "%${rig}s" ""
    elif [ "$ali" = "right" ]; then
	printf "%-${pad}s" "${msg}"
    else
	printf "%${pad}s" "${msg}"
    fi
}

# Parameters:
# see put ()
put_line () {
    if [ $SILENT -eq 1 ]; then return; fi

    put "$1" "${2:-}" "${3:-}"
    printf "\n"
}

# Parameters:
# $1: Message
put_title_line () {
    if [ $SILENT -eq 1 ]; then return; fi

    put "--"
    put "${1:-}" 75 center
    put_line "--"
}

# Parameters:
# none
put_line_line () {
    if [ $SILENT -eq 1 ]; then return; fi

    put      "----------------------------------------"
    put_line "---------------------------------------"
}

# Parameters:
# $1 : Message
# $2 : Time
print_time () {
    t=$2
    if [ $t -ge 3600 ]; then
	h=`expr $t / 3600`"h "
	t=`expr $t % 3600`
    else
	h=""
    fi
    if [ $t -ge 60 ]; then
	m=`expr $t / 60`"mn "
	t=`expr $t % 60`
    else
	m=""
    fi
    s=$t"s."
    put_line  "$1 ${h}${m}${s}"
}
