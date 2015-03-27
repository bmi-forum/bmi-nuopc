!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

!yw   USE module_wrf_top, only : wrf_init, wrf_dfi, wrf_run, wrf_finalize, wrf_run_dt

!<DESCRIPTION>
! Main program of WRF model.  Responsible for starting up the model, reading in (and
! broadcasting for distributed memory) configuration data, defining and initializing
! the top-level domain, either from initial or restart data, setting up time-keeping, and
! then calling the <a href=integrate.html>integrate</a> routine to advance the domain
! to the ending time of the simulation. After the integration is completed, the model
! is properly shut down.
!
!</DESCRIPTION>

   IMPLICIT NONE

    integer ii
!    real, allocatable, dimension(:,:) :: pp
    real, pointer :: pp(:,:)
    real, pointer :: pp3(:,:,:)
    integer :: shape (2), ix,jx

     ix = -1

  CALL bmi_wrf_initialize ("noConfig")
    write(6,*) "end call bmi_wrf_initialize"
       write(6,*) "before call BMI_wrf_Get_grid_shape"
       call flush(6)
       call BMI_wrf_Get_grid_shape("xlat", shape)
       write(6,*) "shape =", shape
       call flush(6)
       ix = shape(1)
       jx = shape(2)
       allocate(pp(ix,jx))
       allocate(pp3(ix,4,jx))
       write(6,*) " yyww ix,jx for pp", ix, jx
       call flush(6)
       

   do ii = 1, 120
       call bmi_wrf_update(90.0D0)
!      write(6,*) "before call bmi_wrf_get_2d_real"
!      call flush(6)
   call bmi_wrf_get_3d_real("smois",pp3)
   call bmi_wrf_get_2d_real("xlat",pp)
!      write(6,*) "after call bmi_wrf_get_3d_real"
!      write(6,*) "smois",pp3
       call flush(6)
   end do

       write(6,*) "yyww smois",pp3
       write(6,*) "yyww xlat",pp
       call flush(6) 

  ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.  
  CALL bmi_wrf_finalize

END PROGRAM wrf


