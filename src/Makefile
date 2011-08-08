###############################################################################
# Set PATH to ASIS library below (default corresponds to the case where you
# installed ASIS in the Gnat directory)

ASIS_TOP = /usr/gnat/

###############################################################################
# Depending on your version of ASIS, the ASIS directory  may be split into  
# several directories or not. Uncomment the set of declarations that matches
# your installation (default corresponds to the current versions of GnatPro and
# GnatGPL):

# ASIS_INCLUDE = ${ASIS_TOP}
# ASIS_OBJ = ${ASIS_TOP}
# ASIS_LIB = ${ASIS_TOP}

# ASIS_INCLUDE = ${ASIS_TOP}/src
# ASIS_OBJ = ${ASIS_TOP}/obj
# ASIS_LIB = ${ASIS_TOP}/lib

ASIS_INCLUDE = ${ASIS_TOP}/include/asis
ASIS_OBJ = ${ASIS_TOP}/lib/asis
ASIS_LIB = ${ASIS_TOP}/lib/asis

###############################################################################
# SYSTEM COMMANDS (do not change)

W32_RM   = del /q /s /f
W32_EXT  = .exe

UNIX_RM  = rm -f
UNIX_EXT =


###############################################################################
# Choose your system by adding/removing comment tags

#RM  = ${W32_RM}
#EXT = ${W32_EXT}

RM  = ${UNIX_RM}
EXT = ${UNIX_EXT}


###############################################################################
# GNAT options, adjust to your taste

GARGS = -gnatf -gnatwa -aI${ASIS_INCLUDE} -aO${ASIS_OBJ} -g
CARGS = -cargs -O2 -gnatWh -gnat95 -gnato
BARGS = -bargs -E
LARGS = -largs -L${ASIS_LIB} -lasis

# Uncomment the following line if you are running GnatPro 6.1.2 or above
#CARGS_EXTRA = ${CARGS} -fno-tree-vrp

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
	gnatmake adactl ${GARGS} ${CARGS} ${CARGS_EXTRA} ${BARGS} ${LARGS}

ptree :
	gnatmake ptree ${GARGS} ${CARGS} ${CARGS_EXTRA} ${BARGS} ${LARGS}

pfni :
	gnatmake pfni ${GARGS} ${CARGS} ${CARGS_EXTRA} ${BARGS} ${LARGS}

clean :
	- ${RM} *.o *.ali b~*

veryclean : clean
	- ${RM} *~ adactl${EXT} *~ ptree${EXT} *~ pfni${EXT}

% :
	gnatmake $@ ${GARGS} -gnatc ${CARGS} ${CARGS_EXTRA} ${BARGS} 



