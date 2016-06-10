###############################################################################
# GNAT options, adjust to your taste
# GARGS: general (gprbuild) options
# CARGS, BARGS, LARGS: content of -cargs, -bargs, -largs options
# Note that these override the options in build.gpr
GARGS = 
CARGS = 
BARGS =  
LARGS =  


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
	@echo "--              | install    -- install everything           --"
	@echo "--              | clean      -- delete object files          --"
	@echo "--              | veryclean  -- clean + delete executable    --"
	@echo "--                                                           --"
	@echo "---------------------------------------------------------------"


build : adactl ptree pfni;

adactl :
	gprbuild build.gpr adactl ${GARGS} -cargs ${CARGS} -bargs ${BARGS} -largs ${LARGS}

ptree :
	gprbuild build.gpr ptree  ${GARGS} -cargs ${CARGS} -bargs ${BARGS} -largs ${LARGS}

pfni :
	gprbuild build.gpr pfni   ${GARGS} -cargs ${CARGS} -bargs ${BARGS} -largs ${LARGS}

install :
	gprinstall build.gpr -f

clean :
	gprclean -c build.gpr

veryclean : clean
	gprclean build.gpr



