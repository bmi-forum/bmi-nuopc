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
ESMF_BMIROOT = $(CWD)/..
ESMF_BMILIBDIR = $(ESMF_BMIROOT)/lib

LOCAL_F90COMPILEPATHS=-I/$(ESMF_BMIROOT)/mod
LOCAL_F90COMPILEOPTS =
LOCAL_F90LINKOPTS =
LOCAL_F90LINKPATHS = -Llib -L$(ESMF_BMILIBDIR)
LOCAL_F90LINKLIBS = -lesmfbmi -ltest_bmi

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(LOCAL_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(LOCAL_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(LOCAL_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(LOCAL_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

# -----------------------------------------------------------------------------
<<<<<<< HEAD
all: directories libtest_bmi.a testApp.exe
# -----------------------------------------------------------------------------
directories :
	mkdir -p lib

=======
all: libtest_bmi.a testApp.exe
# -----------------------------------------------------------------------------
>>>>>>> a46016f488954e03c673f22e3fa605a42af15fbd
testApp.exe: testApp.o testNuopcDriver.o testNuopcBmiCmp.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(LOCAL_F90LINKOPTS) $(LOCAL_F90LINKPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS) $(LOCAL_F90LINKLIBS)

# module dependencies:
testApp.o: testNuopcDriver.o
testNuopcDriver.o: testNuopcBmiCmp.o

# -----------------------------------------------------------------------------
libtest_bmi.a : testModelBmiWrapper.o testModel.o
	ar rc lib/libtest_bmi.a $^
	
# module dependencies:
testModelBmiWrapper.o : testModel.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info
dust:
	rm -f PET*.ESMF_LogFile PET*.VMUserMpiEx.Log *.nc rsl.error.* rsl.out.*
clean:
	rm -f *.exe *.a *.so *.o *.mod
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================


