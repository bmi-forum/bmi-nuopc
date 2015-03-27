#!/bin/csh -f
rm -f wrf_bmi.exe 

# -ffree-form
#   Specify the layout used by the source file. The 
#   free form layout was introduced in Fortran 90. 
#   Fixed form was traditionally used in older Fortran 
#   programs. When neither option is specified, the 
#   source form is determined by the file extension.

# -fconvert=swap
#    Specify the representation of data for unformatted
#    files. Valid values for conversion are: native,
#    the default; swap, swap between big- and little-endian;
#    big-endian, use big-endian representation for
#    unformatted files; little-endian, use little-endian
#    representation for unformatted files.
#
#    This option has an effect only when used in the main
#    program. The "CONVERT" specifier and the 
#    GFORTRAN_CONVERT_UNIT environment variable override 
#    the default specified by -fconvert.

gfortran -c -w -ffree-form -fconvert=swap test_driver.F
mpif90 -o wrf_bmi.exe -w -ffree-form -fconvert=swap test_driver.o -L lib/ -lwrf_bmi -L$NETCDF/lib -lnetcdff -lnetcdf 

ls -l wrf_bmi.exe
