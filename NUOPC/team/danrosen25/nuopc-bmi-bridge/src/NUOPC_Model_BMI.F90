module NUOPC_Model_BMI

    !-----------------------------------------------------------------------------
    ! MODEL Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model, &
        model_routine_SS => SetServices ! Override NUOPC_Model SetServices
    use esmfBmiAdapter ! ESMF BMI Adapter is a component class of NUOPC Model BMI
  
    implicit none
  
    private
  
    public &
        SetModel, &
        SetServices, &
        routine_Run

    public &
        label_AdvanceClock, &
        label_CheckImport, &
        label_DataInitialize, &
        label_SetClock, &
        label_SetRunClock

    character(:),allocatable :: modelConfigFile

!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------

    subroutine SetModel(configFile, initialize,finalize,update, getStartTime, getEndTime, getCurrentTime, getTimeStep, getTimeUnits, getVarType, getVarUnits, getVarRank, getGridType, getGridShape, getGridSpacing, getGridOrigin, getDouble, getDoubleAt, setDouble, setDoubleAt, getInputVarNames, getOutputVarNames, getComponentName, rc)
        character(*),intent(in) :: configFile
        procedure(bmiInitialize) :: initialize
        procedure(bmiUpdate) :: update
        procedure(bmiFinalize) :: finalize
        procedure(bmiGetStartTime) :: getStartTime
        procedure(bmiGetEndTime) :: getEndTime
        procedure(bmiGetCurrentTime) :: getCurrentTime
        procedure(bmiGetTimeStep) :: getTimeStep
        procedure(bmiGetTimeUnits) :: getTimeUnits
        procedure(bmiGetVarType) :: getVarType
        procedure(bmiGetVarUnits) :: getVarUnits
        procedure(bmiGetVarRank) :: getVarRank
        procedure(bmiGetGridType) :: getGridType
        procedure(bmiGetGridShape) :: getGridShape
        procedure(bmiGetGridSpacing) :: getGridSpacing
        procedure(bmiGetGridOrigin) :: getGridOrigin
        procedure(bmiGetDouble) :: getDouble
        procedure(bmiGetDoubleAt) :: getDoubleAt
        procedure(bmiSetDouble) :: setDouble
        procedure(bmiSetDoubleAt) :: setDoubleAt
        procedure(bmiGetInputVarNames) :: getInputVarNames
        procedure(bmiGetOutputVarNames) :: getOutputVarNames
        procedure(bmiGetComponentName) :: getComponentName
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
            getDouble = getDouble, &
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
            return  ! bail ou

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

        call fieldDictionaryAddAll(rc=rc)
        if(ESMF_LogFoundError(rcToCheck=rc,msg="Add Fields to Dictionary Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

#define WITHIMPORTFIELDS_OFF
#ifdef WITHIMPORTFIELDS
    call stateAdvertiseAllInputFields(importState,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg="Advertise Input Fields Failed", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

            call stateAdvertiseAllOutputFields(exportState,rc)
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

        rc = ESMF_SUCCESS

        ! create a Grid object for Fields
        gridIn = gridCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Grid Create error.", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        gridOut = gridIn ! for now out same as in

#ifdef WITHIMPORTFIELDS
  call stateRealizeInputFields(importState,gridIn,rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg="Realize Input Fields Error", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

            call stateRealizeOutputFields(exportState,gridOut,rc)
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

        call BMIAdapter_Update(rc)
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
        type(ESMF_State)    :: exportState
        type(ESMF_Field)    :: field

        rc = ESMF_SUCCESS

        ! query the Component for its exportState
        call ESMF_GridCompGet(model, exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_StateGet(exportState, BMIAdapter_ExportFieldAt(1,rc), field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_FieldPrint(field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    call BMIAdapter_Finalize(rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg="finalize failed", &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

end subroutine

!========================================
! Add individual field to NUOPC Dictionary
!========================================

subroutine fieldDictionaryAdd(var_name, rc)
    implicit none
    character(len=*), intent (in) :: var_name
    character(len=:),allocatable    :: units ! Deffered length for units string
    character(len=10)    :: dict_units
    integer                                     :: rc
    logical                                     :: in_dictionary

    rc = ESMF_SUCCESS

    units = BMIAdapter_FieldUnitsGet (var_name,rc)

    in_dictionary = NUOPC_FieldDictionaryHasEntry(trim(var_name), rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    if (in_dictionary) then
        call NUOPC_FieldDictionaryGetEntry(trim(var_name), canonicalUnits=dict_units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        print *,trim(var_name)," units: ",trim(dict_units)
        if (dict_units .ne. units) then
            rc=ESMF_RC_NOT_VALID
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Field units don't match!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if
    else
        call NUOPC_FieldDictionaryAddEntry(trim(var_name), canonicalUnits=trim(units), rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_LogWrite("model field added <" // trim(var_name) // ":" // trim(units) // ">", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    end if

end subroutine fieldDictionaryAdd

!========================================
! Add All Fields To Dictionary
! Iterate over all fields in input and
! output variable arrays.
!========================================

subroutine fieldDictionaryAddAll(rc)
    implicit none
    character(len=:),allocatable    :: invarnames(:), outvarnames(:)
    integer                                     :: i
    integer, intent(out)                        :: rc

    rc = ESMF_SUCCESS

    invarnames = BMIAdapter_ImportFieldListGet(rc)
    if (rc .ne. ESMF_SUCCESS) return

    do i=1,SIZE(invarnames)
        call FieldDictionaryAdd(invarnames(i),rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    end do

    outvarnames = BMIAdapter_ExportFieldListGet(rc)
    if (rc .ne. ESMF_SUCCESS) return

    do i=1,SIZE(outvarnames)
        call FieldDictionaryAdd(outvarnames(i),rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    end do
end subroutine fieldDictionaryAddAll

!========================================
! Advertise all input fields to referenced state variable
!========================================

subroutine stateAdvertiseAllInputFields(importState,rc)
    type(ESMF_State)        :: importState
    integer, intent(out)    :: rc
    character(len=:),pointer    :: invarnames(:)
    integer                                     :: i

    rc = ESMF_SUCCESS

    ! Get import variable names
    invarnames = BMIAdapter_ExportFieldListGet(rc)
    if (rc .ne. ESMF_SUCCESS) return  ! bail out

    do i=1,SIZE(invarnames)
        call NUOPC_StateAdvertiseField(importState, &
            StandardName=trim(invarnames(i)), name=trim(invarnames(i)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Error advertising: " // trim(invarnames(i)), &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end do
end subroutine stateAdvertiseAllInputFields

!========================================
! Advertise all output fields ot referenced state variable
!========================================

subroutine stateAdvertiseAllOutputFields(exportState,rc)
    type(ESMF_State)        :: exportState
    integer, intent(out)    :: rc
    character(len=:),allocatable    ::  outvarnames(:)
    integer                                     :: i

    rc = ESMF_SUCCESS

    ! Get export variable names
    outvarnames = BMIAdapter_ExportFieldListGet(rc)
    print *,outvarnames
    if (rc .ne. ESMF_SUCCESS) return  ! bail out

    do i=1,SIZE(outvarnames)
        call NUOPC_StateAdvertiseField(exportState, &
            StandardName=trim(outvarnames(i)), name=trim(outvarnames(i)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Error advertising: " // trim(outvarnames(i)), &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end do

end subroutine stateAdvertiseAllOutputFields

!========================================
!  Realize all input fields to import
!========================================

subroutine stateRealizeInputFields(importState,gridIn,rc)
    type(ESMF_State)     :: importState
    type(ESMF_Grid)         :: gridIn
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Field)        :: field
    character(len=:),allocatable    :: invarnames(:)
    integer                                     :: i

    rc = ESMF_SUCCESS

    ! Get import variables
    invarnames = BMIAdapter_ImportFieldListGet(rc)
    if (rc .ne. ESMF_SUCCESS) return  ! bail out

    do i=1,size(invarnames)
        field = BMIAdapter_ESMFFieldCreate(gridIn,trim(invarnames(i)),rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call NUOPC_StateRealizeField(importState, field=field, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    end do

end subroutine stateRealizeInputFields

!========================================
! Realize all output fields to export
!========================================
subroutine stateRealizeOutputFields(exportState,gridOut,rc)
    type(ESMF_State)     :: exportState
    integer, intent(out) :: rc
    type(ESMF_Grid) :: gridOut

    ! local variables
    type(ESMF_Field)        :: field
    character(len=:),allocatable    ::  outvarnames(:)
    integer                                     :: i

    rc = ESMF_SUCCESS

    ! Get export variable names
    outvarnames = BMIAdapter_ExportFieldListGet(rc)
    if (rc .ne. ESMF_SUCCESS) return  ! bail out

    ! 26.6.9 ESMF_FieldCreate - Create a Field from Grid and Fortran array pointer

    do i=1,size(outvarnames)
        field = BMIAdapter_ESMFFieldCreate(grid=gridOut,name=outvarnames(i),rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    end do
end subroutine stateRealizeOutputFields


!###########################
!# Section V:              #
!# Grid Adapter Procedures #
!###########################

function gridCreate(rc) result(return_grid)
    integer,intent(out) :: rc
    type(ESMF_Grid) :: return_grid

    ! local variables
    integer :: iterator1, iterator2
    character(len=:),allocatable :: invarnames(:),outvarnames(:)
    integer :: gridtype, gridrank

    rc = ESMF_SUCCESS

    invarnames = BMIAdapter_ImportFieldListGet(rc)
    if (rc .ne. ESMF_SUCCESS) return  ! bail out
    outvarnames = BMIAdapter_ExportFieldListGet(rc)
    if (rc .ne. ESMF_SUCCESS) return  ! bail out

    ! If input fields and outputs field do not share the same grid
    ! then FAILURE.  Logic to create multiple field grids is not yet implemented
    do iterator1=1,SIZE(invarnames)
        do iterator2=1,SIZE(outvarnames)
            if (.NOT.(BMIAdapter_GridComparison(invarnames(iterator1),outvarnames(iterator2),rc))) then
                rc = ESMF_RC_NOT_IMPL
                return ! If variables do not share the same grid then return
            end if
        end do
    end do

    return_grid = BMIAdapter_ESMFGridCreate(invarnames(1),rc)

end function gridCreate

end module NUOPC_Model_BMI

