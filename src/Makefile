###############################################################################
# Set PATH to ASIS library below

ASIS_TOP = /usr/gnat/asis/

###############################################################################
# Depending on your version of ASIS, the ASIS directory  may be split into  
# several directories or not

# ASIS_INCLUDE = ${ASIS_TOP}
# ASIS_OBJ = ${ASIS_TOP}
# ASIS_LIB = ${ASIS_TOP}

ASIS_INCLUDE = ${ASIS_TOP}/src
ASIS_OBJ = ${ASIS_TOP}/obj
ASIS_LIB = ${ASIS_TOP}/lib

###############################################################################
# SYSTEM COMMANDS (do not change)

W32_RM   = del /q /s /f
W32_SEP  = \\
W32_EXT  = .exe

UNIX_RM  = rm -f
UNIX_SEP = /
UNIX_EXT =


###############################################################################
# Choose your system by adding/removing comment tags

#RM  = ${W32_RM}
#SEP = ${W32_SEP}
#EXT = ${W32_EXT}

RM  = ${UNIX_RM}
SEP = ${UNIX_SEP}
EXT = ${UNIX_EXT}


###############################################################################
# GNAT options, adjust to your taste

GARGS = -v -gnatf -gnatwa -aI${ASIS_INCLUDE} -aO${ASIS_OBJ} -g
CARGS = -cargs -O2 -gnatWh
BARGS = -bargs -E
LARGS = -largs -L${ASIS_LIB} -largs -lasis


###############################################################################
# Do not change anything below this line

.PHONY : *

help :
	@echo "---------------------------------------------------------------"
	@echo "--                                                           --"
	@echo "--    make <entry>                                           --"
	@echo "--                                                           --"
	@echo "--    <entry> ::= help       -- print this message           --"
	@echo "--              | build      -- build all executables        --"
	@echo "--              | adactl     -- build adactl                 --"
	@echo "--              | ptree      -- build ptree                  --"
	@echo "--              | pfni       -- build pfni                   --"
	@echo "--              | clean      -- delete object files          --"
	@echo "--              | veryclean  -- clean + delete executable    --"
	@echo "--                                                           --"
	@echo "---------------------------------------------------------------"


build : adactl ptree pfni;

adactl :
	gnatmake adactl ${GARGS} ${CARGS} ${BARGS} ${LARGS}

ptree :
	gnatmake ptree ${GARGS} ${CARGS} ${BARGS} ${LARGS}

pfni :
	gnatmake pfni ${GARGS} ${CARGS} ${BARGS} ${LARGS}

clean :
	- ${RM} *.o *.ali b~*

veryclean : clean
	- ${RM} *~ adactl${EXT} *~ ptree{EXT} *~ pfni{EXT}

% :
	gnatmake $@ ${GARGS} -gnatc ${CARGS} ${BARGS} 



