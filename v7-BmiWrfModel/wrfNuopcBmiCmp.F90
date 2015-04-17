module WrfNuopcBmiCmp

    !-----------------------------------------------------------------------------
    ! MODEL Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model_BMI, only: &
        bmi_model_routine_SM        => SetModel, &
        bmi_model_routine_SS        => SetServices, &
        PrintBmiInfo, &
        PrintBmiFieldData
    use module_nctool, only: write2NC, defNC, defnc2, defnc3, w_real_nc3, w_real_nc2, tt_close
  
    implicit none

    private
  
    public SetServices

    real, pointer :: xlat(:,:) !latitude
    real, pointer :: xlong(:,:) !longitude
    real, pointer :: t2(:,:) !2 meter temperature
    real, pointer :: q2(:,:) !2 meter specific humidity
    real, pointer :: u10(:,:) ! 10 meter wind u component
    real, pointer :: v10(:,:) ! 10 meter wind v component
    real, pointer :: psfc(:,:) !surface pressure
    real, pointer :: rain(:,:) !precipitation
    real, pointer :: glw(:,:) ! download long wave radiation at ground level
    real, pointer :: swdown(:,:) ! download short wave radiation at ground level
    real, pointer :: smois(:,:,:) ! soil moisture
    integer       :: nsoil = 4
    integer       :: ncid
    integer, dimension (2) :: shape

!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------

    subroutine SetServices(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        external bmi_wrf_initialize, &
            bmi_wrf_update, &
            bmi_wrf_finalize, &
            bmi_wrf_run_model, &
            bmi_wrf_get_2d_real, &
            bmi_wrf_get_3d_real, &
            BMI_wrf_Get_time_step, &
            BMI_wrf_Get_time_units, &
            BMI_wrf_Get_var_units, &
            BMI_wrf_Get_grid_shape, &
            BMI_wrf_Get_grid_spacing, &
            BMI_wrf_Get_component_name

        rc = ESMF_SUCCESS

        ! Set model - Pass NUOPC Model BMI config file and model procedures
        call bmi_model_routine_SM(configFile = "noConfig", &
            initialize = wrfInitialize, &
            finalize = wrfFinalize, &
            update = wrfUpdate, &
            getVarRank = wrfGetVarRank, &
            getOutputVarNames = wrfGetOutputVarNames, &
            getTimeStep = BMI_wrf_Get_time_step, &
            getTimeUnits = BMI_wrf_Get_time_units, &
            getVarUnits = BMI_wrf_Get_var_units, &
            getGridShape = wrfGetGridShape, &
            getGridSpacing = wrfGetGridSpacing, &
            getReal = wrfGetDouble, &
            getComponentName = wrfGetComponentName, &
            rc = rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! the NUOPC model component will register the generic methods
        call NUOPC_CompDerive(model, bmi_model_routine_SS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    subroutine wrfGetVarRank (var_name, rank)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (out) :: rank

        select case (trim(var_name))
            case ("xlat")
                rank = 2
            case ("xlong")
                rank = 2
            case ("t2")
                rank = 2
            case ("q2")
                rank = 2
            case ("u10")
                rank = 2
            case ("v10")
                rank = 2
            case ("psfc")
                rank = 2
            case ("rain")
                rank = 2
            case ("glw")
                rank = 2
            case ("swdown")
                rank = 2
            case ("smois")
                rank = 3
            case default
                rank = 0
        end select
    end subroutine

    subroutine wrfInitialize(config_file)
        character(len=*),intent(in) ::  config_file

        ! WRF has already been initialized but model
        ! component variables must be initialized
        ! call bmi_wrf_initialize(config_file)

        call BMI_wrf_Get_grid_shape("xlat", shape)
        allocate(xlat(shape(1),shape(2) ))
        allocate(smois(shape(1),nsoil,shape(2) ))
        call BMI_wrf_Get_grid_shape("xlong", shape)
        allocate(xlong(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("t2", shape)
        allocate(t2(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("q2", shape)
        allocate(q2(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("u10", shape)
        allocate(u10(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("v10", shape)
        allocate(v10(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("psfc", shape)
        allocate(psfc(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("glw", shape)
        allocate(glw(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("swdown", shape)
        allocate(swdown(shape(1),shape(2) ))
        call BMI_wrf_Get_grid_shape("rain", shape)
        allocate(rain(shape(1),shape(2) ))

        call PrintBmiInfo()

    end subroutine

    subroutine wrfUpdate(dt)
        real, optional, intent(in) :: dt
        call bmi_wrf_update(90.0D0)
        call bmi_wrf_get_2d_real("xlat",xlat)
        call bmi_wrf_get_2d_real("xlong",xlong)
        call bmi_wrf_get_2d_real("t2",t2)
        call bmi_wrf_get_2d_real("q2",q2)
        call bmi_wrf_get_2d_real("u10",u10)
        call bmi_wrf_get_2d_real("v10",v10)
        call bmi_wrf_get_2d_real("psfc",psfc)
        call bmi_wrf_get_2d_real("rain",rain)
        call bmi_wrf_get_2d_real("glw",glw)
        call bmi_wrf_get_2d_real("swdown",swdown)
        call bmi_wrf_get_3d_real("smois",smois)

    end subroutine

    subroutine wrfGetOutputVarNames (names)
        implicit none
        character (*),pointer, intent (out) :: names(:)
        character (len=22), target, &
            dimension (10) :: output_items = (/ &
            'xlat  ', & !latitude
            'xlong ', & !longitude
            't2    ', & !, & !2 meter temperature
            'q2    ', & !2 meter specific humidity
            'u10   ', & !10 meter wind u component
            'v10   ', & !10 meter wind v component
            'psfc  ', & !, & !surface pressure
            'glw   ', & !precipitation
            'swdown', & !download long wave radiation at ground level
            'rain  ' /) !download short wave radiation at ground level
            ! 'smois ' /) !Soil moisture (removed because not functioning)

        names => output_items
    end subroutine
  
    subroutine wrfGetGridShape (var_name, shape)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, dimension (:), intent (out) :: shape

        call BMI_wrf_Get_grid_shape ( var_name, shape)

    end subroutine

    subroutine wrfGetGridSpacing (var_name, spacing)
        implicit none
        character (len=*), intent (in) :: var_name
        real, dimension (:), intent (out) :: spacing

        call BMI_wrf_Get_grid_spacing ( var_name, spacing)

    end subroutine

    subroutine wrfGetDouble (var_name, dest)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
        implicit none
        character (len=*), intent (in) :: var_name
        real, pointer, intent (inout) :: dest(:)
        ! end declaration section

        real, pointer :: src_as_1d (:)
        integer :: cptrshape
        type (c_ptr) :: src

        select case (trim(var_name))
            case ("xlat")
                src = c_loc (xlat(1,1))
                call BMI_wrf_Get_grid_shape("xlat", shape)
                cptrshape = shape(1) * shape(2)
            case ("xlong")
                src = c_loc (xlong(1,1))
                call BMI_wrf_Get_grid_shape("xlong", shape)
                cptrshape = shape(1) * shape(2)
            case ("t2")
                src = c_loc (t2(1,1))
                call BMI_wrf_Get_grid_shape("t2", shape)
                cptrshape = shape(1) * shape(2)
            case ("q2")
                src = c_loc (q2(1,1))
                call BMI_wrf_Get_grid_shape("q2", shape)
                cptrshape = shape(1) * shape(2)
            case ("u10")
                src = c_loc (u10(1,1))
                call BMI_wrf_Get_grid_shape("u10", shape)
                cptrshape = shape(1) * shape(2)
            case ("v10")
                src = c_loc (v10(1,1))
                call BMI_wrf_Get_grid_shape("v10", shape)
                cptrshape = shape(1) * shape(2)
            case ("psfc")
                src = c_loc (psfc(1,1))
                call BMI_wrf_Get_grid_shape("psfc", shape)
                cptrshape = shape(1) * shape(2)
            case ("rain")
                src = c_loc (rain(1,1))
                call BMI_wrf_Get_grid_shape("rain", shape)
                cptrshape = shape(1) * shape(2)
            case ("glw")
                src = c_loc (glw(1,1))
                call BMI_wrf_Get_grid_shape("glw", shape)
                cptrshape = shape(1) * shape(2)
            case ("swdown")
                src = c_loc (swdown(1,1))
                call BMI_wrf_Get_grid_shape("swdown", shape)
                cptrshape = shape(1) * shape(2)
            case ("smois")
                src = c_loc (smois(1,1,1))
                call BMI_wrf_Get_grid_shape("xlat", shape)
                cptrshape = shape(1) * nsoil * shape(2)
            case default

        end select

        if (associated (dest)) then
            call C_F_POINTER (src, src_as_1d, [cptrshape])
            dest = src_as_1d
        else
            call C_F_POINTER (src, dest, [cptrshape])
        endif
    end subroutine

    subroutine wrfGetComponentName (name)
        implicit none
        character (len=*),pointer, intent (out) :: name
        character (len=22),target :: wrfname = "WRF"

        !call BMI_wrf_Get_component_name(wrfname)
        name => wrfname

    end subroutine

    subroutine wrfFinalize()

        !output
       call BMI_wrf_Get_grid_shape("xlat", shape)
       allocate(xlat(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("xlat",xlat)
       call defNC(ncid,"xlat.nc",shape(1),shape(2),"xlat" )
       call write2NC(ncid,shape(1),shape(2),xlat,"xlat")
       call  tt_close(ncid)

       call BMI_wrf_Get_grid_shape("xlong", shape)
       allocate(xlong(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("xlong",xlong)
       call defNC(ncid,"xlong.nc",shape(1),shape(2),"xlong" )
       call write2NC(ncid,shape(1),shape(2),xlong,"xlong")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("t2", shape)
       allocate(t2(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("t2",t2)
       call defNC(ncid,"t2.nc",shape(1),shape(2),"t2" )
       call write2NC(ncid,shape(1),shape(2),t2,"t2")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("q2", shape)
       allocate(q2(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("q2",q2)
       call defNC(ncid,"q2.nc",shape(1),shape(2),"q2" )
       call write2NC(ncid,shape(1),shape(2),q2,"q2")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("u10", shape)
       allocate(u10(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("u10",u10)
       call defNC(ncid,"u10.nc",shape(1),shape(2),"u10" )
       call write2NC(ncid,shape(1),shape(2),u10,"u10")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("v10", shape)
       allocate(v10(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("v10",v10)
       call defNC(ncid,"v10.nc",shape(1),shape(2),"v10" )
       call write2NC(ncid,shape(1),shape(2),v10,"v10")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("psfc", shape)
       allocate(psfc(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("psfc",psfc)
       call defNC(ncid,"psfc.nc",shape(1),shape(2),"psfc" )
       call write2NC(ncid,shape(1),shape(2),psfc,"psfc")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("glw", shape)
       allocate(glw(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("glw",glw)
       call defNC(ncid,"glw.nc",shape(1),shape(2),"glw" )
       call write2NC(ncid,shape(1),shape(2),glw,"glw")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("swdown", shape)
       allocate(swdown(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("swdown",swdown)
       call defNC(ncid,"swdown.nc",shape(1),shape(2),"swdown" )
       call write2NC(ncid,shape(1),shape(2),swdown,"swdown")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("rain", shape)
       allocate(rain(shape(1),shape(2) ))
       call bmi_wrf_get_2d_real("rain",rain)
       call defNC(ncid,"rain.nc",shape(1),shape(2),"rain" )
       call write2NC(ncid,shape(1),shape(2),rain,"rain")
       call   tt_close(ncid)

       call BMI_wrf_Get_grid_shape("xlat", shape)
       allocate(smois(shape(1),nsoil,shape(2) ))
       call bmi_wrf_get_3d_real("smois",smois)
       call defNC(ncid,"smois.nc",shape(1),shape(2),nsoil,"smois" )
       call write2NC(ncid,shape(1),shape(2),nsoil,smois,"smois")
       call   tt_close(ncid)

        ! WRF Finalizes at application layer
        ! call bmi_wrf_finalize()
    end subroutine

end module
