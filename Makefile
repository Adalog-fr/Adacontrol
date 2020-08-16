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

# Do not interfer with gprbuild's parallelism.
.NOTPARALLEL:

executables := adactl adactl_fix ptree pfni

help :
	@echo "---------------------------------------------------------------"
	@echo "--                                                           --"
	@echo "--    make <entry>                                           --"
	@echo "--                                                           --"
	@echo "--    <entry> ::= help       -- print this message           --"
	@echo "--              | build      -- build all executables        --"
	@$(foreach e,$(executables),printf --\
	      "--              | %-11s-- build %-23s--\n" $(e) $(e);)
	@echo "--              | install    -- install everything           --"
	@echo "--              | clean      -- delete object files          --"
	@echo "--              | veryclean  -- clean + delete executable    --"
	@echo "--                                                           --"
	@echo "---------------------------------------------------------------"

build : $(executables);

$(executables) :
	gprbuild build.gpr $@ ${GARGS} -cargs ${CARGS} -bargs ${BARGS} -largs ${LARGS}

install :
	gprinstall build.gpr -f -p

clean :
	gprclean -c build.gpr

veryclean : clean
	gprclean build.gpr



