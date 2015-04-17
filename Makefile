# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the DIR to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

CWD = $(shell pwd)
ESMF_BMILIBDIR=$(CWD)/lib
ESMF_BMIMODDIR=$(CWD)/mod
SRCDIR=$(CWD)/src

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

vpath %.f90 $(SRCDIR)
vpath %.F90 $(SRCDIR)
vpath %.c $(SRCDIR)
vpath %.C $(SRCDIR)

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

# -----------------------------------------------------------------------------
<<<<<<< HEAD
all: directories libesmfbmi.a
# -----------------------------------------------------------------------------

directories :
	mkdir -p mod
	mkdir -p lib

=======
all: libesmfbmi.a
# -----------------------------------------------------------------------------

>>>>>>> a46016f488954e03c673f22e3fa605a42af15fbd
libesmfbmi.a : NUOPC_Model_BMI.o esmfBmiAdapter.o
	cp *.mod $(ESMF_BMIMODDIR)/.
	ar rc $(ESMF_BMILIBDIR)/libesmfbmi.a $^

# -----------------------------------------------------------------------------

# module dependencies:
NUOPC_Model_BMI.o: esmfBmiAdapter.o

# -----------------------------------------------------------------------------
.PHONY: clean distclean info

clean:
	rm -f *.o *.mod $(ESMF_BMILIBDIR)/*.a $(ESMF_BMIMODDIR)/*.mod
distclean: clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================


