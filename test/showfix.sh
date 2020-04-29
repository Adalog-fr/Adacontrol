#!/bin/sh
# Compare the fixed files as produced by runfix.sh with the original ones
# Allows to launch a diff tool to view the patches
#
# usage: 
#   ./showfix [<pattern>]
#   ./showfix -h

# At Adalog, we use meld. Replace with your favorite diff tool...
case $(uname) in
    *win*|*Win*|*WIN*)  # Windows, Cygwin...
        MERGE=`where meld | tr -d \\\\r  | tr '\\\\' '/'`
        ;;
    *)                  # Assume *nix
        MERGE=`which meld`
	;;
esac

if [ "$1" = "-h" ] ; then
    echo "usage: showfix [<pattern>]"
    echo "<pattern>: file wildcard without extension (.ad[sb] assumed)"
    exit
fi

list=`find fixed/res -maxdepth 1 -name "${1:-*}.ad[sb]" -printf "%f "`
if [ -z "$list" ] ; then
    echo "$1 not matched in fixed/res/"
    exit;
fi

nb_fix=$(echo $list | wc -w)
current=0
for f in $list ; do
    current=$((current+1))
    while true ; do
	echo -n "$current/$nb_fix $f: [Diff, Interactive, Nothing, Quit]? "
	read REP
	case $REP in
	    d | D )
                echo "-----------------  <... ref, >... res -----------------"
		diff "$f" "fixed/res/$f"  2>/dev/null;;
	    i | I )
                "$MERGE" "$f" "fixed/res/$f";;
	    n | N )
		break;;
	    q | Q )
		exit;;
	esac
    done
done
