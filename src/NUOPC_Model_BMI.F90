module NUOPC_Model_BMI

    !-----------------------------------------------------------------------------
    ! MODEL Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model, &
        model_routine_SS => SetServices ! Override NUOPC_Model SetServices
    use esmfBmiAdapter, & ! ESMF BMI Adapter is a component class of NUOPC Model BMI
        PrintBmiInfo => BMIAdapter_PrintComponentInfo, &
        PrintBmiCurrentTime => BMIAdapter_PrintCurrentTime, &
        PrintBmiFieldData => BMIAdapter_PrintFieldData
  
    implicit none
  
    private
  
    public &
        SetModel, &
        SetServices, &
        routine_Run, &
        PrintBmiInfo, &
        PrintBmiCurrentTime, &
        PrintBmiFieldData

    public &
        label_AdvanceClock, &
        label_CheckImport, &
        label_DataInitialize, &
        label_SetClock, &
        label_SetRunClock

    character(:),allocatable        :: modelConfigFile
    !character(:),allocatable        :: invarnames(:)
    !character(:),allocatable        :: outvarnames(:)

!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------

    subroutine SetModel(configFile, initialize,finalize,update, getStartTime, getEndTime, &
        getCurrentTime, getTimeStep, getTimeUnits, getVarType, getVarUnits, getVarRank, &
        getGridType, getGridShape, getGridSpacing, getGridOrigin, getGridCoord, getReal, &
        getReal2D, getReal3D, getDoubleAt, setDouble, setDoubleAt, getInputVarNames, getOutputVarNames, &
        getComponentName, rc)

        character(*),intent(in) :: configFile
        procedure(bmiInitialize) :: initialize
        procedure(bmiUpdate) :: update
        procedure(bmiFinalize) :: finalize
        procedure(bmiGetTimeStep) :: getTimeStep
        procedure(bmiGetTimeUnits) :: getTimeUnits
        procedure(bmiGetVarUnits) :: getVarUnits
        procedure(bmiGetGridShape) :: getGridShape
        procedure(bmiGetGridSpacing) :: getGridSpacing
        procedure(bmiGetReal),optional :: getReal
        procedure(bmiGetReal2D),optional :: getReal2D
        procedure(bmiGetReal3D),optional :: getReal3D
        procedure(bmiGetComponentName) :: getComponentName
        procedure(bmiGetStartTime),optional :: getStartTime
        procedure(bmiGetEndTime),optional :: getEndTime
        procedure(bmiGetCurrentTime),optional :: getCurrentTime
        procedure(bmiGetVarType),optional :: getVarType
        procedure(bmiGetVarRank),optional :: getVarRank
        procedure(bmiGetGridType),optional :: getGridType
        procedure(bmiGetGridOrigin),optional :: getGridOrigin
        procedure(bmiGetGridCoord),optional  :: getGridCoord
        procedure(bmiGetDoubleAt),optional :: getDoubleAt
        procedure(bmiSetDouble),optional :: setDouble
        procedure(bmiSetDoubleAt),optional :: setDoubleAt
        procedure(bmiGetInputVarNames),optional :: getInputVarNames
        procedure(bmiGetOutputVarNames),optional :: getOutputVarNames
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        modelConfigFile = configFile

        call BMIAdapter_SetModel( &
            initialize = initialize, &
            finalize = finalize, &
            update = update, &
            getStartTime = getStartTime, &
            getEndTime = getEndTime, &
            getCurrentTime = getCurrentTime, &
            getTimeStep = getTimeStep, &
            getTimeUnits = getTimeUnits, &
            getVarType = getVarType, &
            getVarUnits = getVarUnits, &
            getVarRank = getVarRank, &
            getGridType = getGridType, &
            getGridShape = getGridShape, &
            getGridSpacing = getGridSpacing, &
            getGridOrigin = getGridOrigin, &
            getReal = getReal, &
            getReal2D = getReal2D, &
            getReal3D = getReal3D, &
            getDoubleAt = getDoubleAt, &
            setDouble = setDouble, &
            setDoubleAt = setDoubleAt, &
            getInputVarNames = getInputVarNames, &
            getOutputVarNames = getOutputVarNames, &
            getComponentName = getComponentName, &
            rc = rc)
    end subroutine SetModel
    
    !-----------------------------------------------------------------------------
    subroutine SetServices(gcomp,rc)
        type(ESMF_GridComp)   :: gcomp
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        call model_routine_SS(gcomp,rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

         ! set entry point for methods that require specific implementation
        call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! attach specializing method(s)
        call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, &
            specRoutine=ModelAdvance, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSpecialize(gcomp,specLabel=label_Finalize, &
            specRoutine=ModelFinalize, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine


    subroutine InitializeP1(model, importState, exportState, clock, rc)
        type(ESMF_GridComp)     :: model
        type(ESMF_State)        :: importState, exportState
        type(ESMF_Clock)        :: clock, compclock
        type(ESMF_TimeInterval) :: stabilityTimeStep
        integer, intent(out)    :: rc
        type(ESMF_VM) :: vm
        integer ::  mpic, ierr
        integer :: commsize

        rc = ESMF_SUCCESS

        call BMIAdapter_Initialize(trim(adjustl(modelConfigFile)),rc) ! Initialize BMI Model
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Initialize BMI Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        compclock = BMIAdapter_ESMFClockCreate(clock, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSetClock(model, externalClock=compclock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call fieldDictionaryAddImportFields(rc=rc)
        if(ESMF_LogFoundError(rcToCheck=rc,msg="Add Import Fields to Dictionary Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

        call fieldDictionaryAddExportFields(rc=rc)
        if(ESMF_LogFoundError(rcToCheck=rc,msg="Add Export Fields to Dictionary Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

        call stateAdvertiseImportFields(importState,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Advertise Input Fields Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call stateAdvertiseExportFields(exportState,rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Advertise Output Fields Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    !-----------------------------------------------------------------------------

    subroutine InitializeP2(model, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: gridIn
        type(ESMF_Grid)         :: gridOut
        character(len=22),allocatable :: invarnames(:),outvarnames(:)

        rc = ESMF_SUCCESS

        ! create a Grid object for Fields
        call BMIAdapter_ImportFieldListGet(invarnames, rc)
        if (rc .ne. ESMF_SUCCESS) return  ! bail out
        call BMIAdapter_ExportFieldListGet(outvarnames, rc)
        if (rc .ne. ESMF_SUCCESS) return  ! bail out

        ! Add some logic for more robust grid creation here

        gridOut = BMIAdapter_ESMFGridCreate(outvarnames(1),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Grid Create error.", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        gridIn = gridOut ! for now out same as in

        call stateRealizeImportFields(importState,gridIn,rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Realize Input Fields Error", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call stateRealizeExportFields(exportState,gridOut,rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Realize Output Fields Error", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    !-----------------------------------------------------------------------------

    subroutine ModelAdvance(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_State)              :: importState, exportState

        rc = ESMF_SUCCESS

        call BMIAdapter_Update(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMI Update Error", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! query the Component for its clock, importState and exportState
        call ESMF_GridCompGet(model, clock=clock, importState=importState, &
            exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_ClockPrintStopTime(clock, &
            "------>Advancing MODEL to stop time: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    !-----------------------------------------------------------------------------

    subroutine ModelFinalize(model,rc)
        type(ESMF_Gridcomp) :: model
        integer,intent(out) :: rc

        rc = ESMF_SUCCESS

        call BMIAdapter_Finalize(rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="finalize failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    !========================================
    ! Add All Fields To Dictionary
    ! Iterate over all fields in input and
    ! output variable arrays.
    !========================================


    subroutine fieldDictionaryAddImportFields(rc)
        implicit none
        integer, intent(out) :: rc

        character(len=22),allocatable    :: invarnames(:)
        character(len=:),allocatable    :: units ! Deffered length for units string
        character(len=10)    :: dict_units
        logical                                     :: in_dictionary
        integer :: i

        rc = ESMF_SUCCESS

        call BMIAdapter_ImportFieldListGet(invarnames,rc)
        if (rc .ne. ESMF_SUCCESS) return

        do i=1,SIZE(invarnames)

            units = BMIAdapter_FieldUnitsGet (invarnames(i),rc)

            in_dictionary = NUOPC_FieldDictionaryHasEntry(trim(invarnames(i)), rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            if (in_dictionary) then
                call NUOPC_FieldDictionaryGetEntry(trim(invarnames(i)), canonicalUnits=dict_units, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
                if (dict_units .ne. units) then
                    rc=ESMF_RC_NOT_VALID
                    if (ESMF_LogFoundError(rcToCheck=rc, msg="Field units don't match!", &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                end if
            else
                call NUOPC_FieldDictionaryAddEntry(trim(invarnames(i)), canonicalUnits=trim(units), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg="Error adding to dictionary: " // trim(invarnames(i)), &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out

                call ESMF_LogWrite("model field added to dictionary <" // trim(invarnames(i)) // ":" // trim(units) // ">", ESMF_LOGMSG_INFO, rc=rc)
                if (rc .ne. ESMF_SUCCESS) return
            end if
        end do

    end subroutine fieldDictionaryAddImportFields

    subroutine fieldDictionaryAddExportFields(rc)
        implicit none
        integer, intent(out) :: rc

        character(len=22),allocatable    :: outvarnames(:)
        character(len=:),allocatable    :: units ! Deffered length for units string
        character(len=10)    :: dict_units
        logical                                     :: in_dictionary
        integer :: i

        rc = ESMF_SUCCESS

        call BMIAdapter_ExportFieldListGet(outvarnames, rc)
        if (rc .ne. ESMF_SUCCESS) return

        do i=1,size(outvarnames)

            units = BMIAdapter_FieldUnitsGet (outvarnames(i),rc)

            in_dictionary = NUOPC_FieldDictionaryHasEntry(trim(outvarnames(i)), rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            if (in_dictionary) then
                call NUOPC_FieldDictionaryGetEntry(trim(outvarnames(i)), canonicalUnits=dict_units, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
                if (dict_units .ne. units) then
                    rc=ESMF_RC_NOT_VALID
                    if (ESMF_LogFoundError(rcToCheck=rc, msg="Field units don't match!", &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                end if
            else
                call NUOPC_FieldDictionaryAddEntry(trim(outvarnames(i)), canonicalUnits=trim(units), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg="Error adding to dictionary: " // trim(outvarnames(i)), &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out

                call ESMF_LogWrite("model field added to dictionary <" // trim(outvarnames(i)) // ":" // trim(units) // ">", ESMF_LOGMSG_INFO, rc=rc)
                if (rc .ne. ESMF_SUCCESS) return
            end if
        end do

    end subroutine fieldDictionaryAddExportFields

    !========================================
    ! Advertise all input fields to referenced state variable
    !========================================

    subroutine stateAdvertiseImportFields(importState,rc)
        type(ESMF_State)        :: importState
        integer, intent(out)    :: rc
        character(len=22),allocatable    :: invarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        ! Get import variable names
        call BMIAdapter_ImportFieldListGet(invarnames, rc)
        if (rc .ne. ESMF_SUCCESS) return  ! bail out

        do i=1,SIZE(invarnames)
            call NUOPC_StateAdvertiseField(importState, &
                StandardName=trim(invarnames(i)), name=trim(invarnames(i)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Error advertising: " // trim(invarnames(i)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call ESMF_LogWrite("Model import field advertised <" // trim(invarnames(i)) // ">", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        end do
    end subroutine stateAdvertiseImportFields

    !========================================
    ! Advertise all output fields ot referenced state variable
    !========================================

    subroutine stateAdvertiseExportFields(exportState,rc)
        type(ESMF_State)        :: exportState
        integer, intent(out)    :: rc
        character(len=22),allocatable    ::  outvarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        ! Get export variable names
        call BMIAdapter_ExportFieldListGet(outvarnames, rc)
        if (rc .ne. ESMF_SUCCESS) return  ! bail out

        do i=1,SIZE(outvarnames)
            call NUOPC_StateAdvertiseField(exportState, &
                StandardName=trim(outvarnames(i)), name=trim(outvarnames(i)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Error advertising: " // trim(outvarnames(i)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call ESMF_LogWrite("Model export field advertised <" // trim(outvarnames(i)) // ">", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        end do

    end subroutine stateAdvertiseExportFields

    !========================================
    !  Realize all input fields to import
    !========================================

    subroutine stateRealizeImportFields(importState,gridIn,rc)
        type(ESMF_State)     :: importState
        type(ESMF_Grid)         :: gridIn
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Field)        :: field
        character(len=22),allocatable    :: invarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        ! Get import variables
        call BMIAdapter_ImportFieldListGet(invarnames, rc)
        if (rc .ne. ESMF_SUCCESS) return  ! bail out

        do i=1,size(invarnames)
            field = BMIAdapter_ESMFFieldCreate(gridIn,trim(invarnames(i)),rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Input Field Create Error: " // trim(invarnames(i)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call ESMF_LogWrite("Model import field created <" // trim(invarnames(i)) // ">", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return

            call NUOPC_StateRealizeField(importState, field=field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Input Field Realize Error: " // trim(invarnames(i)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call ESMF_LogWrite("Model import field realized <" // trim(invarnames(i)) // ">", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        end do

        call ESMF_LogWrite("All model import fields created and realized.", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

    end subroutine stateRealizeImportFields

    !========================================
    ! Realize all output fields to export
    !========================================
    subroutine stateRealizeExportFields(exportState,gridOut,rc)
        type(ESMF_State)     :: exportState
        integer, intent(out) :: rc
        type(ESMF_Grid) :: gridOut

        ! local variables
        type(ESMF_Field)        :: field
        character(len=22),allocatable    ::  outvarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        ! Get export variable names
        call BMIAdapter_ExportFieldListGet(outvarnames, rc)
        if (rc .ne. ESMF_SUCCESS) return  ! bail out

        ! 26.6.9 ESMF_FieldCreate - Create a Field from Grid and Fortran array pointer

        do i=1,size(outvarnames)
            field = BMIAdapter_ESMFFieldCreate(grid=gridOut,name=outvarnames(i),rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Output Field Create Error: " // trim(outvarnames(i)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call ESMF_LogWrite("Model export field created <" // trim(outvarnames(i)) // ">", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return

            call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Output Field Realize Error: " // trim(outvarnames(i)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call ESMF_LogWrite("Model export field realized <" // trim(outvarnames(i)) // ">", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return

        end do

        call ESMF_LogWrite("All model export fields created and realized.", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

    end subroutine stateRealizeExportFields

end module NUOPC_Model_BMI

