# This sed script will comment out all Ada representation clauses
# by adding "--UNREPR " in front of them
# Its effect can be undone by removing all "--UNREPR "
# NOTE: this script uses GNU Sed extensions

/^.*--.*[fF][oO][rR]/{ #ignore for in comments...
             /\(^\|[^a-zA-Z0-9_]\)[fF][oO][rR][^a-zA-Z0-9_].*--/btop # ...except if there is also a good "for"
             bend   
}
 
/^.*".*[fF][oO][rR].*"/bend  #ignore for in strings

/\(^\|[^a-zA-Z0-9_]\)[fF][oO][rR]\([^a-zA-Z0-9_]\|$\)/ { # if we have a true "for" 
:top
   /record/{:endrec   # Record rep clause: search "end record"
             /end[ \n]*record/!{
             N 
             bendrec}
             bdosubst}
   /;/!{       #  Not record rep clause: search ";"
        N      #     add the Next line to the pattern space
        btop   
       }

:dosubst
/[lL][oO][oO][pP]/bend   # nothing to do if we are in a for loop

s/[fF][oO][rR]/--UNREPR &/
s/\n\( *\)/\n\1--UNREPR /g

# Check if we have another repr. clause on the same line
/; *[fF][oO][rR][^a-zA-Z0-9_]/{
 n
 s/[^ ]/--UNREPR &/
 btop
}
}
:end
