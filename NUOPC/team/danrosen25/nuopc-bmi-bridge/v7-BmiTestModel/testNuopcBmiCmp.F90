module TestNuopcBmiCmp

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

    external BMI_Initialize, &
             BMI_Finalize, &
             BMI_Update, &
             BMI_Get_start_time, &
             BMI_Get_end_time, &
             BMI_Get_current_time, &
             BMI_Get_time_step, &
             BMI_Get_time_units, &
             BMI_Get_var_type, &
             BMI_Get_var_units, &
             BMI_Get_var_rank, &
             BMI_Get_grid_type, &
             BMI_Get_grid_shape, &
             BMI_Get_grid_spacing, &
             BMI_Get_grid_origin, &
             BMI_Get_double, &
             BMI_Get_double_at_indices, &
             BMI_Set_double, &
             BMI_Set_double_at_indices, &
             BMI_Get_input_var_names, &
             BMI_Get_output_var_names, &
             BMI_Get_component_name
    
        rc = ESMF_SUCCESS

        ! Set model - Pass NUOPC Model BMI config file and model procedures
        call bmi_model_routine_SM(configFile = "test.cfg", &
            initialize = BMI_Initialize, &
            finalize = BMI_Finalize, &
            update = BMI_Update, &
            getStartTime = BMI_Get_start_time, &
            getEndTime = BMI_Get_end_time, &
            getCurrentTime = BMI_Get_current_time, &
            getTimeStep = BMI_Get_time_step, &
            getTimeUnits = BMI_Get_time_units, &
            getVarType = BMI_Get_var_type, &
            getVarUnits = BMI_Get_var_units, &
            getVarRank = BMI_Get_var_rank, &
            getGridType = BMI_Get_grid_type, &
            getGridShape = BMI_Get_grid_shape, &
            getGridSpacing = BMI_Get_grid_spacing, &
            getGridOrigin = BMI_Get_grid_origin, &
            getDouble = BMI_Get_double, &
            getDoubleAt = BMI_Get_double_at_indices, &
            setDouble = BMI_Set_double, &
            setDoubleAt = BMI_Set_double_at_indices, &
            getInputVarNames = BMI_Get_input_var_names, &
            getOutputVarNames = BMI_Get_output_var_names, &
            getComponentName = BMI_Get_component_name, &
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
