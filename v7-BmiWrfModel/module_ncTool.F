module module_ncTool

   implicit none
#include <netcdf.inc>

interface readFromNC
    module procedure get1d_real
    module procedure get1d_int
    module procedure get2d_real
    module procedure get2d_int
    module procedure get3d_int
    module procedure get3d_real
end interface 

interface write2NC
    module procedure w_real_nc1
    module procedure w_int_nc1
    module procedure w_real_nc2
    module procedure w_int_nc2
    module procedure w_real_nc3
    module procedure w_int_nc3
end interface 

interface defNC
    module procedure defNC2
    module procedure defNC3
end interface 

     contains
        subroutine       get1d_real(var_name,out_buff,ix,fileName)
          implicit none
          integer :: ivar, iret,varid,ncid,ix
          real out_buff(ix)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            out_buff = -9999.
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_real(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get1d_real

        subroutine       get2d_real(var_name,out_buff,ix,jx,fileName)
          implicit none
          integer :: ivar, iret,varid,ncid,ix,jx
          real out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            out_buff = -9999.
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_real(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get2d_real

        subroutine get3d_real(var_name,out_buff,ix,jx,nsoil,fileName)
          implicit none
          integer :: ivar, iret,varid,ncid,ix,jx, nsoil
          real out_buff(ix,jx,nsoil)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            out_buff = -9999.
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_real(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get3d_real

        subroutine get1d_int(var_name,out_buff,ix,fileName)
          implicit none
          integer :: ivar, iret,varid,ncid,ix
          integer :: out_buff(ix)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            out_buff = -9999.
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_int(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get1d_int

        subroutine get2d_int(var_name,out_buff,ix,jx,fileName)
          implicit none
          integer :: ivar, iret,varid,ncid,ix,jx
          integer :: out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            out_buff = -9999.
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_int(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get2d_int

       subroutine get3d_int(var_name,out_buff,ix,jx,nsoil,fileName)
          implicit none
          integer :: ivar, iret,varid,ncid,ix,jx, nsoil
          integer:: out_buff(ix,jx,nsoil)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            out_buff = -9999.
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_int(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get3d_int

        subroutine w_real_nc2(ncid,ix,jx,inVar,varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret
           character(len=*) varName
           real inVar(ix,jx)
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ix,jx/),invar)
           return
        end subroutine w_real_nc2

        subroutine w_real_nc1(ncid,ix,inVar,varName)
           implicit none
           integer:: ncid,ix,varid , iret
           character(len=*) varName
           real inVar(ix)
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1/), (/ix/),invar)
           return
        end subroutine w_real_nc1

       subroutine w_int_nc1(ncid,ix,inVar,varName)
           implicit none
           integer:: ncid,ix,varid , iret
           character(len=*) varName
           integer :: inVar(ix)
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_int(ncid, varid, (/1/), (/ix/),invar)
           return
        end subroutine w_int_nc1

       subroutine w_int_nc2(ncid,ix,jx,inVar,varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret
           character(len=*) varName
           integer :: inVar(ix,jx)
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_int(ncid, varid, (/1,1/), (/ix,jx/),invar)
           return
        end subroutine w_int_nc2
 
        subroutine w_int_nc3(ncid,ix,NSOIL,jx,inVar, varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret, nsoil
           character(len=*) varName
           integer:: inVar(ix,nsoil,jx)
             iret = nf_inq_varid(ncid,trim(varName), varid)
             iret = nf_put_vara_int(ncid, varid, (/1,1,1/), (/ix,NSOIL,jx/),inVar)
           return
        end subroutine w_int_nc3

        subroutine w_real_nc3(ncid,ix,jx,NSOIL,inVar, varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret, nsoil
           character(len=*) varName
           real:: inVar(ix,nsoil,jx)
             iret = nf_inq_varid(ncid,trim(varName), varid)
             iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/ix,NSOIL,jx/),inVar)
           return
        end subroutine w_real_nc3

        subroutine defNC2(ncid,fileName,ix,jx,varName)
           implicit none
           character(len=*) :: fileName, varName
           integer :: ix,jx,nsoil, ncid, iret, dimid_ix, dimid_jx, dimid_soil, varid

       iret = nf_create(trim(fileName), NF_CLOBBER, ncid)

       iret = nf_def_dim(ncid, "x", ix, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jx, dimid_jx)

        iret = nf_def_var(ncid,trim(varName),NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)

           iret = nf_enddef(ncid)
        end subroutine defNC2

        subroutine defNC3(ncid,fileName,ix,jx,nsoil,varName)
           implicit none
           character(len=*) :: fileName, varName
           integer :: ix,jx,nsoil, ncid, iret, dimid_ix, dimid_jx, dimid_soil, varid

       iret = nf_create(trim(fileName), NF_CLOBBER, ncid)

       iret = nf_def_dim(ncid, "x", ix, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jx, dimid_jx)
       iret = nf_def_dim(ncid, "depth", nsoil, dimid_soil)  !-- 3-d soils
       iret = nf_def_var(ncid,trim(varName),NF_FLOAT, 3, (/dimid_ix,dimid_soil,dimid_jx/), varid)

           iret = nf_enddef(ncid)
        end subroutine defNC3

        subroutine tt_close(ncid)
            integer :: ncid, iret
            iret = nf_close(ncid)
        end subroutine tt_close

end module module_ncTool

    


       subroutine tt () 
          use module_ncTool
           parameter(ix=10)
           parameter(jx=10)
           parameter(nsoil=2)
           real r1(nsoil)
           real r2(ix,jx)
           real r3(ix,jx,nsoil)
           integer i1(nsoil)
           integer i2(ix,jx)
           integer i3(ix,jx,nsoil)
           integer :: i,j,k,ncid, iret
           r1 = 1
           r2 = 2
           r3 = 3
           i1 = 1
           i2 = 2
           i3 = 3
           call defNC(ncid,"test.nc",ix,jx,nsoil,"r1")
           call write2NC(ncid,nsoil,r1,"r1")
           iret = nf_close(ncid)

           r1 = 0
           r2 = 0
           r3 = 0
           i1 = 0
           i2 = 0
           i3 = 0
           call readFromNC("r1",r1,nsoil,"test.nc")
           write(6,*) "r1= ", r1
           call readFromNC("i3",i3,ix,jx,nsoil,"test.nc")
           write(6,*) "i3= ", i3
        end subroutine tt
