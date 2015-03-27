module wrfNuopcBmiCmp

    !-----------------------------------------------------------------------------
    ! MODEL Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model_BMI, only: &
        bmi_model_routine_SM        => SetModel, &
        bmi_model_routine_SS        => SetServices
  
    implicit none

    private
  
    public SetServices

!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------
  
    subroutine SetServices(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

!        external bmi_wrf_initialize, &
!            bmi_wrf_update, &
!            bmi_wrf_finalize, &
!            bmi_wrf_run_model, &
!            bmi_wrf_get_2d_real, &
!            bmi_wrf_get_3d_real, &
!            BMI_wrf_Get_time_step, &
!            BMI_wrf_Get_time_units, &
!            BMI_wrf_Get_var_units, &
!            BMI_wrf_Get_grid_shape, &
!            BMI_wrf_Get_grid_spacing, &
!            BMI_wrf_Get_component_name



        rc = ESMF_SUCCESS

        call bmi_wrf_initialize("")
        ! Set model - Pass NUOPC Model BMI config file and model procedures
!        call bmi_model_routine_SM(configFile = "namelist.input", &
!            initialize = bmi_wrf_initialize, &
!            finalize = bmi_wrf_finalize, &
!            update = bmi_wrf_update, &
!            getStartTime = Dummy_Get_start_time, &
!            getEndTime = Dummy_Get_end_time, &
!            getCurrentTime = Dummy_Get_current_time, &
!            getTimeStep = BMI_wrf_Get_time_step, &
!            getTimeUnits = BMI_wrf_Get_time_units, &
!            getVarType = Dummy_Get_var_type, &
!            getVarUnits = BMI_wrf_Get_var_units, &
!            getVarRank = Dummy_Get_var_rank, &
!            getGridType = Dummy_Get_grid_type, &
!            getGridShape = BMI_wrf_Get_grid_shape, &
!            getGridSpacing = BMI_wrf_Get_grid_spacing, &
!            getGridOrigin = Dummy_Get_grid_origin, &
!            getDouble = bmi_wrf_get_2d_real, &
!            getDoubleAt = Dummy_Get_double_at_indices, &
!            setDouble = Dummy_Set_double, &
!            setDoubleAt = Dummy_Set_double_at_indices, &
!            getInputVarNames = Dummy_Get_input_var_names, &
!            getOutputVarNames = Dummy_Get_output_var_names, &
!            getComponentName = BMI_wrf_Get_component_name, &
!            rc = rc)
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

!subroutine Dummy_Get_start_time (start)
!    implicit none
!    real,intent(out) :: start
!    start = 0.
!end subroutine
!
!subroutine Dummy_Get_end_time (end)
!    implicit none
!    real, intent (out) :: end
!    end = 100.
!end subroutine
!
!subroutine Dummy_Get_current_time (time)
!    implicit none
!    real, intent (out) :: time
!    time = 1.
!end subroutine
!
!subroutine Dummy_Get_var_type (var_name, type)
!    implicit none
!    character (len=*), intent (in) :: var_name
!    integer, intent (out) :: type
!    type = 8
!end subroutine
!
!subroutine Dummy_Get_var_rank (var_name, rank)
!    implicit none
!    character (len=*), intent (in) :: var_name
!    integer, intent (out) :: rank
!    rank = 2
!end subroutine
!
!subroutine Dummy_Get_grid_type (var_name, type)
!    implicit none
!    character (len=*), intent (in) :: var_name
!    integer, intent (out) :: type
!    type = 1
!end subroutine
!
!subroutine Dummy_Get_grid_origin (var_name, origin)
!    implicit none
!    character (len=*), intent (in) :: var_name
!    real, dimension (:), intent (out) :: origin
!    origin(1) = 0.
!    origin(2) = 0.
!end subroutine
!
!subroutine Dummy_Get_double_at_indices (var_name, dest, inds)
!    implicit none
!    character (len=*), intent (in) :: var_name
!    real, pointer, intent (inout) :: dest(:)
!    integer, intent (in) :: inds(:)
!end subroutine
!
!subroutine Dummy_Set_double (var_name, src)
!    implicit none
!    character (len=*), intent (in) :: var_name
!    real, intent (in) :: src (*)
!end subroutine
!
!subroutine Dummy_Set_double_at_indices (var_name, inds, src)
!    implicit none
!    character (len=*), intent (in) :: var_name
!    integer, intent (in) :: inds(:)
!    real, intent (in) :: src (*)
!end subroutine
!
!subroutine Dummy_Get_input_var_names (names)
!    implicit none
!    character (*),pointer, intent (out) :: names(:)
!end subroutine
!
!subroutine Dummy_Get_output_var_names (names)
!    implicit none
!    character (*),pointer, intent (out) :: names(:)
!end subroutine

  
end module
