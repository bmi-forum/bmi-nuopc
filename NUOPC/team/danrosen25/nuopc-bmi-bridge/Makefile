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

BINDIR=./bin
LIBDIR=./lib
OBJDIR=./obj
MODDIR=./mod
SRCDIR=./src

#LOCAL_F90COMPILEOPTS = -w -ffree-form -fconvert=swap
#LOCAL_F90LINKOPTS = -w -ffree-form -fconvert=swap
LOCAL_F90LINKPATHS = -Llib -L$(NETCDF)/lib 
LOCAL_F90LINKLIBS = -lwrf_bmi -lnetcdff -lnetcdf
#LOCAL_F90LINKPATHS = -L lib/
#LOCAL_F90LINKLIBS = -lwrf_bmi


#ESMF_F90COMPILEDIRSLOCAL += -I$(OBJDIR) -I$(MODDIR)
#ESMF_CXXCOMPILEDIRSLOCAL += -I$(OBJDIR) -I$(MODDIR)

FILTER_STRING = -Wl,--no-as-needed
#AS_NEEDED = -Wl,--as-needed

ESMF_F90LINKOPTSASNEEDED = $(filter-out $(FILTER_STRING),$(ESMF_F90LINKOPTS))

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

vpath %.f90 $(SRCDIR)
vpath %.F90 $(SRCDIR)
vpath %.c $(SRCDIR)
vpath %.C $(SRCDIR)

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(LOCAL_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(LOCAL_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

# -----------------------------------------------------------------------------
all: libtest_bmi.a testApp.exe
# -----------------------------------------------------------------------------
testApp.exe: testApp.o testNuopcDriver.o testNuopcBmiCmp.o NUOPC_Model_BMI.o esmfBmiAdapter.o libtest_bmi.a
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

wrfApp.exe: wrfApp.o wrfNuopcDriver.o wrfNuopcBmiCmp.o NUOPC_Model_BMI.o esmfBmiAdapter.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTSASNEEDED) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(LOCAL_F90LINKPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)  $(LOCAL_F90LINKLIBS)

wrfTest.exe:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(LOCAL_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) src/wrfTest.F90
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTSASNEEDED) $(LOCAL_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(LOCAL_F90LINKPATHS) -o wrfTest.exe wrfTest.o $(ESMF_F90ESMFLINKLIBS) $(LOCAL_F90LINKLIBS)

# module dependencies:
testApp.o: testNuopcDriver.o
testNuopcDriver.o: testNuopcBmiCmp.o
testNuopcBmiCmp.o: NUOPC_Model_BMI.o

wrfApp.o: wrfNuopcDriver.o
wrfNuopcDriver.o: wrfNuopcBmiCmp.o
wrfNuopcBmiCmp.o: NUOPC_Model_BMI.o

NUOPC_Model_BMI.o: esmfBmiAdapter.o

# -----------------------------------------------------------------------------
libtest_bmi.a : testModelBmiWrapper.o testModel.o testmodelbmiwrapper.mod
	ar rc libtest_bmi.a $^
	
testModelBmiWrapper.o : testModel.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile *.nc
clean:
	rm -f *.exe *.a *.so *.o *.mod rsl.error.* rsl.out.*
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

test:
	@$(TSTDIR)/testBMIadapter.sh

edit:
	nedit mainApp.F90 driver.F90 modeladapter_bmi.F90 &
