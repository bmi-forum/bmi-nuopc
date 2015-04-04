module WrfNuopcBmiCmp

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
            initialize = bmi_wrf_initialize, &
            finalize = bmi_wrf_finalize, &
            update = bmi_wrf_update, &
            getTimeStep = BMI_wrf_Get_time_step, &
            getTimeUnits = BMI_wrf_Get_time_units, &
            getVarUnits = BMI_wrf_Get_var_units, &
            getGridShape = BMI_wrf_Get_grid_shape, &
            getGridSpacing = BMI_wrf_Get_grid_spacing, &
            getDouble = bmi_wrf_get_2d_real, &
            getComponentName = BMI_wrf_Get_component_name, &
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
  
end module
