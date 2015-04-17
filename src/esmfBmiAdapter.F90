


module esmfBmiAdapter

    use ESMF

    implicit none

    private

    public BMIAdapter_SetModel, &
        BMIAdapter_Initialize, &
        BMIAdapter_Update, &
        BMIAdapter_Finalize, &
        BMIAdapter_ESMFClockCreate, &
        BMIAdapter_ESMFFieldCreate, &
        BMIAdapter_ESMFGridCreate, &
        BMIAdapter_FieldUnitsGet, &
        BMIAdapter_ImportFieldListGet, &
        BMIAdapter_ExportFieldListGet, &
        BMIAdapter_ImportCount, &
        BMIAdapter_ExportCount, &
        BMIAdapter_ImportFieldAt, &
        BMIAdapter_ExportFieldAt, &
        BMIAdapter_GridComparison, &
        BMIAdapter_PrintComponentInfo, &
        BMIAdapter_PrintCurrentTime, &
        BMIAdapter_PrintFieldData

    public bmiInitialize, &
        bmiUpdate, &
        bmiFinalize, &
        bmiGetStartTime, &
        bmiGetEndTime, &
        bmiGetCurrentTime, &
        bmiGetTimeStep, &
        bmiGetTimeUnits, &
        bmiGetVarType, &
        bmiGetVarUnits, &
        bmiGetVarRank, &
        bmiGetGridType, &
        bmiGetGridShape, &
        bmiGetGridSpacing, &
        bmiGetGridOrigin, &
        bmiGetGridCoord, &
        bmiGetReal, &
        bmiGetReal2D, &
        bmiGetReal3D, &
        bmiGetDoubleAt, &
        bmiSetDouble, &
        bmiSetDoubleAt, &
        bmiGetInputVarNames, &
        bmiGetOutputVarNames, &
        bmiGetComponentName

    interface
        subroutine bmiInitialize(config_file)
            character(len=*),intent(in) ::  config_file
        end subroutine
        subroutine bmiUpdate(dt)
            real, optional, intent(in) :: dt
        end subroutine
        subroutine bmiFinalize()
        end subroutine
        subroutine bmiGetStartTime(start)
            real,intent(out) :: start
        end subroutine
        subroutine bmiGetEndTime(end)
            real, intent (out) :: end
        end subroutine
        subroutine bmiGetCurrentTime(time)
            real, intent (out) :: time
        end subroutine
        subroutine bmiGetTimeStep(dt)
            real, intent (out) :: dt
        end subroutine
        subroutine bmiGetTimeUnits(units)
            character (len=*), intent (out) :: units
        end subroutine
        subroutine bmiGetVarType(var_name, type)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: type
        end subroutine
        subroutine bmiGetVarUnits(var_name, units)
            character (len=*), intent (in) :: var_name
            character (len=*), intent (out) :: units
        end subroutine
        subroutine bmiGetVarRank(var_name, rank)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: rank
        end subroutine
        subroutine bmiGetGridType(var_name, type)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: type
        end subroutine
        subroutine bmiGetGridShape(var_name, shape)
            character (len=*), intent (in) :: var_name
            integer, dimension (:), intent (out) :: shape
        end subroutine
        subroutine bmiGetGridSpacing(var_name, spacing)
            character (len=*), intent (in) :: var_name
            real, dimension (:), intent (out) :: spacing
        end subroutine
        subroutine bmiGetGridOrigin(var_name, origin)
            character (len=*), intent (in) :: var_name
            real, dimension (:), intent (out) :: origin
        end subroutine
        subroutine bmiGetGridCoord(var_name,dimension,gridX)
            character (len=*), intent (in) :: var_name
            integer, intent(in) :: dimension
            real, dimension (:), intent (out) :: gridX
        end subroutine
        subroutine bmiGetReal(var_name, dest)
            character (len=*),intent(in) :: var_name
            real, pointer, intent(inout) :: dest(:)
        end subroutine
        subroutine bmiGetReal2D(var_name, dest)
            character (len=*), intent (in) :: var_name
            real, pointer, intent (inout) :: dest(:,:)
        end subroutine
        subroutine bmiGetReal3D(var_name, dest)
            character (len=*), intent (in) :: var_name
            real, pointer, intent (inout) :: dest (:,:,:)
        end subroutine
        subroutine bmiGetDoubleAt(var_name, dest, inds)
            character (len=*), intent (in) :: var_name
            real, pointer, intent (inout) :: dest(:)
            integer, intent (in) :: inds(:)
        end subroutine
        subroutine bmiSetDouble(var_name, src)
            character (len=*), intent (in) :: var_name
            real, intent (in) :: src (*)
        end subroutine
        subroutine bmiSetDoubleAt(var_name, inds, src)
            character (len=*), intent (in) :: var_name
            integer, intent (in) :: inds(:)
            real, intent (in) :: src (*)
        end subroutine
        subroutine bmiGetInputVarNames(names)
            character (*),pointer, intent (out) :: names(:)
        end subroutine
        subroutine bmiGetOutputVarNames(names)
            character (*),pointer, intent (out) :: names(:)
        end subroutine
        subroutine bmiGetComponentName(name)
            character (len=*),pointer, intent (out) :: name
        end subroutine
    end interface

    type :: ModelState
        private
        logical :: set = .false.
        logical :: initialized = .false.
        logical :: finalized = .false.
    end type ModelState

    procedure(bmiInitialize), pointer :: pBmiInitialize => null()
    procedure(bmiUpdate), pointer :: pBmiUpdate => null()
    procedure(bmiFinalize), pointer :: pBmiFinalize => null()
    procedure(bmiGetStartTime), pointer :: pBmiGetStartTime => null()
    procedure(bmiGetEndTime), pointer :: pBmiGetEndTime => null()
    procedure(bmiGetCurrentTime), pointer :: pBmiGetCurrentTime => null()
    procedure(bmiGetTimeStep), pointer :: pBmiGetTimeStep => null()
    procedure(bmiGetTimeUnits), pointer :: pBmiGetTimeUnits => null()
    procedure(bmiGetVarType), pointer :: pBmiGetVarType => null()
    procedure(bmiGetVarUnits), pointer :: pBmiGetVarUnits => null()
    procedure(bmiGetVarRank), pointer :: pBmiGetVarRank => null()
    procedure(bmiGetGridType), pointer :: pBmiGetGridType => null()
    procedure(bmiGetGridShape), pointer :: pBmiGetGridShape => null()
    procedure(bmiGetGridSpacing), pointer :: pBmiGetGridSpacing => null()
    procedure(bmiGetGridOrigin), pointer :: pBmiGetGridOrigin => null()
    procedure(bmiGetGridCoord), pointer :: pBmiGetGridCoord => null()
    procedure(bmiGetReal),pointer :: pBmiGetReal => null()
    procedure(bmiGetReal2D), pointer :: pBmiGetReal2D => null()
    procedure(bmiGetReal3D), pointer :: pBmiGetReal3D => null()
    procedure(bmiGetDoubleAt), pointer :: pBmiGetDoubleAt => null()
    procedure(bmiSetDouble), pointer :: pBmiSetDouble => null()
    procedure(bmiSetDoubleAt), pointer :: pBmiSetDoubleAt => null()
    procedure(bmiGetInputVarNames), pointer :: pBmiGetInputVarNames => null()
    procedure(bmiGetOutputVarNames), pointer :: pBmiGetOutputVarNames => null()
    procedure(bmiGetComponentName), pointer :: pBmiGetComponentName => null()


    integer, parameter :: BMI_VAR_TYPE_UNKNOWN = 0
    integer, parameter :: BMI_VAR_TYPE_CHAR = 1
    integer, parameter :: BMI_VAR_TYPE_UNSIGNED_CHAR = 2
    integer, parameter :: BMI_VAR_TYPE_INT = 3
    integer, parameter :: BMI_VAR_TYPE_LONG = 4
    integer, parameter :: BMI_VAR_TYPE_UNSIGNED_INT = 5
    integer, parameter :: BMI_VAR_TYPE_UNSIGNED_LONG = 6
    integer, parameter :: BMI_VAR_TYPE_FLOAT = 7
    integer, parameter :: BMI_VAR_TYPE_DOUBLE = 8
    integer, parameter :: BMI_VAR_TYPE_NUMBER = 9


    integer, parameter :: BMI_GRID_TYPE_UNKNOWN = 0
    integer, parameter :: BMI_GRID_TYPE_UNIFORM = 1
    integer, parameter :: BMI_GRID_TYPE_RECTILINEAR = 2
    integer, parameter :: BMI_GRID_TYPE_STRUCTURED = 3
    integer, parameter :: BMI_GRID_TYPE_UNSTRUCTURED = 4
    integer, parameter :: BMI_GRID_TYPE_NUMBER = 5

    integer, parameter :: BMI_MAXVARNAMESTR = 22
    integer, parameter :: BMI_MAXCOMPNAMESTR = 22
    integer, parameter :: BMI_MAXUNITSSTR = 22

    integer,parameter :: BMI_CHAR = 1
    integer,parameter :: BMI_UNSIGNED_CHAR = 1
    integer,parameter :: BMI_INT =  2
    integer,parameter :: BMI_LONG = 4
    integer,parameter :: BMI_UNSIGNED_INT = 2
    integer,parameter :: BMI_UNSIGNED_LONG = 4
    integer,parameter :: BMI_FLOAT = 4
    integer,parameter :: BMI_DOUBLE = 8

    type(ModelState) :: state

!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------

    !##################################
    !# Section I:                     #
    !# Model Setup Procedures         #
    !##################################

    subroutine BMIAdapter_SetModel(initialize,finalize,update, getStartTime, getEndTime, &
        getCurrentTime, getTimeStep, getTimeUnits, getVarType, getVarUnits, &
        getVarRank, getGridType, getGridShape, getGridSpacing, getGridOrigin, &
        getGridCoord, getReal, getReal2D, getReal3D, getDoubleAt, setDouble, &
        setDoubleAt, getInputVarNames, getOutputVarNames, getComponentName, rc)

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

        pBmiInitialize => initialize
        pBmiUpdate => update
        pBmiFinalize => finalize
        pBmiGetTimeStep => getTimeStep
        pBmiGetTimeUnits => getTimeUnits
        pBmiGetVarUnits => getVarUnits
        pBmiGetGridShape => getGridShape
        pBmiGetGridSpacing => getGridSpacing
        pBmiGetComponentName => getComponentName

        if(present(getReal)) then
            pBmiGetReal => getReal
        end if

        if(present(getReal2D)) then
            pBmiGetReal2D => getReal2D
        else
            pBmiGetReal2D => defaultGetReal2D
        end if
        if(present(getReal3D)) then
            pBmiGetReal3D => getReal3D
        else
            pBmiGetReal3D => defaultGetReal3D
        end if
        if(present(getStartTime)) then
            pBmiGetStartTime => getStartTime
        else
            pBmiGetStartTime => defaultGetStartTime
        end if
        if(present(getEndTime)) then
            pBmiGetEndTime => getEndTime
        else
            pBmiGetEndTime => defaultGetEndTime
        end if
        if(present(getCurrentTime)) then
            pBmiGetCurrentTime => getCurrentTime
        else
            pBmiGetCurrentTime => defaultGetCurrentTime
        end if
        if (present(getVarType)) then
            pBmiGetVarType => getVarType
        else
            pBmiGetVarType => defaultGetVarType
        end if
        if(present(getVarRank)) then
            pBmiGetVarRank => getVarRank
        else
            pBmiGetVarRank => defaultGetVarRank
        end if
        if(present(getGridType)) then
            pBmiGetGridType => getGridType
        else
            pBmiGetGridType => defaultGetGridType
        end if
        if(present(getGridOrigin)) then
            pBmiGetGridOrigin => getGridOrigin
        else
            pBmiGetGridOrigin => defaultGetGridOrigin
        end if
        if(present(getGridCoord)) then
            pBmiGetGridCoord => getGridCoord
        else
            pBmiGetGridCoord => defaultGetGridCoord
        end if
        if(present(getDoubleAt)) then
            pBmiGetDoubleAt => getDoubleAt
        else
            pBmiGetDoubleAt => defaultGetDoubleAt
        end if
        if(present(setDouble)) then
            pBmiSetDouble => setDouble
        else
            pBmiSetDouble => defaultSetDouble
        end if
        if(present(setDoubleAt)) then
            pBmiSetDoubleAt => setDoubleAt
        else
            pBmiSetDoubleAt => defaultSetDoubleAt
        end if
        if(present(getInputVarNames)) then
            pBmiGetInputVarNames => getInputVarNames
        else
            pBmiGetInputVarNames => defaultGetInputVarNames
        end if
        if(present(getOutputVarNames)) then
            pBmiGetOutputVarNames => getOutputVarNames
        else
            pBmiGetOutputVarNames => defaultGetOutputVarNames
        end if

        if(associated(pBmiInitialize) .and. &
            associated(pBmiUpdate) .and. &
            associated(pBmiFinalize) .and. &
            associated(pBmiGetStartTime) .and. &
            associated(pBmiGetEndTime) .and. &
            associated(pBmiGetCurrentTime) .and. &
            associated(pBmiGetTimeStep) .and. &
            associated(pBmiGetTimeUnits) .and. &
            associated(pBmiGetVarType) .and. &
            associated(pBmiGetVarUnits) .and. &
            associated(pBmiGetVarRank) .and. &
            associated(pBmiGetGridType) .and. &
            associated(pBmiGetGridShape) .and. &
            associated(pBmiGetGridSpacing) .and. &
            associated(pBmiGetGridOrigin) .and. &
            associated(pBmiGetGridCoord) .and. &
            (associated(pBmiGetReal2D) .or. associated(pBmiGetReal3D)) .and. &
            associated(pBmiGetDoubleAt) .and. &
            associated(pBmiSetDouble) .and. &
            associated(pBmiSetDoubleAt) .and. &
            associated(pBmiGetInputVarNames) .and. &
            associated(pBmiGetOutputVarNames) .and. &
            associated(pBmiGetComponentName)) then

            state%set = .true.
            call BMIAdapter_LogWrite("Model set.", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                msg="Model set failed! Check external model.", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

    end subroutine BMIAdapter_SetModel

    !#########################################
    !# Section II:                           #
    !# Model control flow adapter procedures #
    !#########################################

    !========================================
    ! Initialize BMI model
    !========================================

    subroutine BMIAdapter_Initialize(file,rc)
        character(*),intent(in) :: file
        integer,intent(out) :: rc

        ! Local Variables
        character(BMI_MAXCOMPNAMESTR), pointer :: compname
        real :: timeStep, timeEnd

        rc = ESMF_SUCCESS

        if (state%set) then

            call pBmiInitialize(file)

            ! To be discussed: Initialization requirements
            call pBmiGetComponentName(compname)
            call pBmiGetTimeStep(timeStep)
            call pBmiGetEndTime(timeEnd)

            if( timeStep > 0 .and. timeEnd > 0) then
                state%initialized = .true.
                call BMIAdapter_LogWrite("Model initialized <" // trim(compname) //">", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (rc .ne. ESMF_SUCCESS) return
            else
                rc = ESMF_RC_OBJ_INIT
                if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                    msg="Model initialization failed <" // trim(compname) // &
                    ">! Check model configuration.", &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
            end if
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                msg="Cannot initialize model before model is set!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

    end subroutine BMIAdapter_Initialize

    !========================================
    ! Update BMI model
    !========================================

    subroutine BMIAdapter_Update(dt, rc)
        real, optional, intent(in) :: dt
        integer, intent(out) :: rc
        character(BMI_MAXCOMPNAMESTR),pointer :: compname

        rc = ESMF_SUCCESS

        if (state%initialized) then
            if(.not. state%finalized) then
                call pBmiUpdate(dt) ! Update BMI
                call pBmiGetComponentName(compname)
                call BMIAdapter_LogWrite("Model updated <" // trim(compname) //">", &
                    ESMF_LOGMSG_INFO, rc=rc)
                if (rc .ne. ESMF_SUCCESS) return
            else
                rc = ESMF_RC_OBJ_INIT
                if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                    msg="Cannot update model after finalization!", &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
            end if
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                msg="Cannot update model before model is initialized!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if
    end subroutine BMIAdapter_Update

    !========================================
    ! Finalize BMI model
    !========================================

    subroutine BMIAdapter_Finalize(rc)
        integer, intent(out) :: rc
        character(BMI_MAXCOMPNAMESTR),pointer :: compname

        rc = ESMF_SUCCESS

        if(state%initialized) then
            call pBmiFinalize() ! Finalize BMI
            call pBmiGetComponentName(compname)
            state%finalized = .true.
            call BMIAdapter_LogWrite("model finalized <" // trim(compname) //">", &
                ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                msg="Cannot finalize model before model is initialized!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

    end subroutine BMIAdapter_Finalize

    !#################
    !# Variable Info #
    !#################

    function BMIAdapter_ImportCount(rc) result(num)
        integer,intent(out) :: rc
        integer :: num
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetInputVarNames(varlist)
        num=size(varlist)
    end function BMIAdapter_ImportCount

    function BMIAdapter_ExportCount(rc) result(num)
        integer,intent(out) :: rc
        integer :: num
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetOutputVarNames(varlist)
        num=size(varlist)
    end function BMIAdapter_ExportCount

    function BMIAdapter_ImportFieldAt(id,rc) result(field)
        integer,intent(in) :: id
        integer,intent(out) :: rc
        character(len=BMI_MAXVARNAMESTR)    :: field
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetInputVarNames(varlist)
        field=varlist(id)
    end function BMIAdapter_ImportFieldAt

    function BMIAdapter_ExportFieldAt(id,rc) result(field)
        integer,intent(in) :: id
        integer,intent(out) :: rc
        character(len=BMI_MAXVARNAMESTR)    :: field
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetOutputVarNames(varlist)
        field=varlist(id)
    end function BMIAdapter_ExportFieldAt

    subroutine BMIAdapter_ImportFieldListGet(names,rc)
        integer, intent(out) :: rc
        character(len=*),dimension(:),allocatable,intent(out) :: names
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetInputVarNames(varlist)
        allocate(names(size(varlist)))
        names=varlist
    end subroutine BMIAdapter_ImportFieldListGet

    subroutine BMIAdapter_ExportFieldListGet(names,rc)
        integer, intent(out) :: rc
        character(len=*),dimension(:),allocatable,intent(out)    :: names
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetOutputVarNames(varlist)
        allocate(names(size(varlist)))
        names=varlist
    end subroutine BMIAdapter_ExportFieldListGet

    function BMIAdapter_FieldUnitsGet(field,rc) result(units)
        integer, intent(out) :: rc
        character(*),intent(in) :: field
        character(len=BMI_MAXUNITSSTR) :: units

        rc = ESMF_SUCCESS
        call pBmiGetVarUnits(field,units)
    end function BMIAdapter_FieldUnitsGet

    !######################################
    !# Create ESMF objects based on model #
    !######################################

    !=================================
    ! Create ESMF Clock based on model
    !=================================

    function BMIAdapter_ESMFClockCreate(refClock, rc) result(clock)
        ! reference clock. usually the system clock
        type(ESMF_Clock),intent(in)  :: refClock
        ! instantiate a clock
        type(ESMF_Clock) :: clock

        ! instantiate time_step, start and stop times
        type(ESMF_TimeInterval) :: timeStep,startDelay,endReference
        type(ESMF_Time) :: refStartTime, startTime, stopTime

        ! local variables for Get methods
        real :: step,start,end
        character(len=10) :: units

        ! return code
        integer :: rc

        rc = ESMF_SUCCESS

        call pBmiGetTimeStep(step)
        call pBmiGetStartTime(start)
        call pBmiGetEndTime(end)
        call pBmiGetTimeUnits(units)

          ! Put warning in for truncation
        if (units .eq. 's') then
            call ESMF_TimeIntervalSet(timeStep, s = INT(step), rc=rc)
            call ESMF_TimeIntervalSet(startDelay, s = INT(start),rc=rc)
            call ESMF_TimeIntervalSet(endReference, s = INT(end),rc=rc)
        else if(units .eq. 'm') then
            call ESMF_TimeIntervalSet(timeStep, m = INT(step), rc=rc)
            call ESMF_TimeIntervalSet(startDelay, m = INT(start),rc=rc)
            call ESMF_TimeIntervalSet(endReference, m = INT(end),rc=rc)
        else
            rc = ESMF_RC_ARG_VALUE
        end if

        call ESMF_ClockGet(refClock,startTime=refStartTime,rc=rc)

        startTime = refStartTime + startDelay
        stopTime = refStartTime + endReference

        ! initialize the clock with the above values
        clock = ESMF_ClockCreate(timeStep, startTime=startTime, stopTime=stopTime, &
            name="bmiclock1", rc=rc)

    end function BMIAdapter_ESMFClockCreate

    !==========================================
    ! Create ESMF Field based on model variable
    !==========================================

    function BMIAdapter_ESMFFieldCreate(grid,name,rc) result(field)
        type(ESMF_Field) :: field
        type(ESMF_Grid),intent(in) :: grid
        character(*),intent(in) :: name
        integer,intent(out) :: rc

        integer :: rank

        rc = ESMF_SUCCESS

        call pBmiGetVarRank(name,rank)
        field = BMIAdapter_ESMFFieldCreateRank(grid,name,rank,rc)

    end function BMIAdapter_ESMFFieldCreate

    function BMIAdapter_ESMFFieldCreateRank(grid,name,rank,rc) result(field)
        type(ESMF_Field) :: field
        type(ESMF_Grid),intent(in) :: grid
        character(*),intent(in) :: name
        integer,intent(in) :: rank
        integer,intent(out) :: rc
        real,pointer :: bmiflat (:)
        real,pointer :: bmireshape_2D(:,:)
        real,pointer :: bmireshape_3D(:,:,:)
        real,pointer :: arrayportion_1D (:)
        real,pointer :: arrayportion_2D (:,:)
        real,pointer :: arrayportion_3D (:,:,:)
        integer, dimension (rank) :: gridshape
        integer, dimension(rank) :: gec
<<<<<<< HEAD
        type(ESMF_VM) :: vm
        integer :: lpe, ldecount
=======
>>>>>>> a46016f488954e03c673f22e3fa605a42af15fbd

        rc = ESMF_SUCCESS

        call pBmiGetGridShape (name, gridshape)

<<<<<<< HEAD
        ! Get current VM and pet number
        call ESMF_VMGetCurrent(vm, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_VMGet(vm, localPet=lpe, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_GridGet(grid, localDECount=ldecount, rc=rc)
        if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        print *,"Local DE Count: ",ldecount," for PET: ",lpe

        call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, localDE = 0, &
            exclusiveCount=gec, rc=rc)
        if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        print *,"Local Exclusive Count: ",gec," for PET: ",lpe

=======
        call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, localDE = 0, &
            exclusiveCount=gec, rc=rc)
        if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
>>>>>>> a46016f488954e03c673f22e3fa605a42af15fbd

        if(rank == 1) then
            call pBmiGetReal(name,bmiflat)
            arrayportion_1D => bmiflat(1:gec(1))
            field = ESMF_FieldCreate(grid, farrayptr=arrayportion_1D,name=name, rc=rc)
        else if(rank == 2) then
            call pBmiGetReal(name,bmiflat)
            bmireshape_2D (1:gridshape(1), 1:gridshape(2)) => bmiflat
            arrayportion_2D => bmireshape_2D(1:gec(1),1:gec(2))
            field = ESMF_FieldCreate(grid, farrayptr=arrayportion_2D,name=name, rc=rc)
        else if(rank == 3) then
            call pBmiGetReal(name,bmiflat)
            bmireshape_3D (1:gridshape(1), 1:gridshape(2), 1:gridshape(3)) => bmiflat
            arrayportion_3D => bmireshape_3D(1:gec(1),1:gec(2),1:gec(3))
            field = ESMF_FieldCreate(grid, farrayptr=arrayportion_3D,name=name, rc=rc)
        else
            rc = ESMF_RC_NOT_IMPL
            if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                msg="Field rank out of bounds.", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

    end function BMIAdapter_ESMFFieldCreateRank

    !========================================
    ! Create grid based on variable
    !========================================

    function BMIAdapter_ESMFGridCreate(varname,rc) result(return_grid)
        type(ESMF_Grid) :: return_grid
        integer, intent(out) :: rc
        character(*),intent(in) ::varname

        ! Local Variables

        integer :: rank

        rc = ESMF_SUCCESS
        call pBmiGetVarRank(varname,rank)
        return_grid = gridCreateNoDecomp(varname,rank,rc)
    end function BMIAdapter_ESMFGridCreate

    function minIndex(varname,rank)
        integer, intent(in) :: rank
        character(*),intent(in) ::varname
        integer :: minIndex(1:rank)
        real :: gridorigin(1:rank)

        call pBmiGetGridOrigin (varname, gridorigin)

        if(rank .eq. 1) then
            minIndex = (/gridorigin(1)/)
        else if(rank .eq. 2) then
            minIndex = (/gridorigin(1),gridorigin(2)/)
        else if(rank .eq. 3) then
            minIndex = (/gridorigin(1),gridorigin(2),gridorigin(3)/)
        else
            minIndex = 0
        end if

    end function minIndex

    function maxIndex(varname,rank)
        integer, intent(in) :: rank
        character(*),intent(in) ::varname
        integer :: maxIndex(1:rank)
        integer :: gridshape(1:rank)
        real :: gridorigin(1:rank)

        call pBmiGetGridOrigin (varname, gridorigin)
        call pBmiGetGridShape (varname,gridshape)

        if(rank .eq. 1) then
            maxIndex = (/(gridorigin(1)+gridshape(1)-1)/)
        else if(rank .eq. 2) then
            maxIndex = (/(gridorigin(1)+gridshape(1)-1), &
                (gridorigin(2)+gridshape(2)-1)/)
        else if(rank .eq. 3) then
            maxIndex = (/(gridorigin(1)+gridshape(1)-1), &
                (gridorigin(2)+gridshape(2)-1), &
                (gridorigin(3)+gridshape(3)-1)/)
        else
            maxIndex = 0
        end if

    end function maxIndex

    function noDecomp(varname,rank)
        integer, intent(in) :: rank
        character(*),intent(in) ::varname
        integer :: noDecomp(1:rank)

        if(rank .eq. 1) then
            noDecomp = (/1/)
        else if(rank .eq. 2) then
            noDecomp = (/1,1/)
        else if(rank .eq. 3) then
            noDecomp = (/1,1,1/)
        else
            noDecomp = 0
        end if

    end function noDecomp

    subroutine setGridCoord2D(grid,varname,dimension,rc)
        type(ESMF_Grid),intent(inout) :: grid
        character(*), intent(in) :: varname
        integer, intent(in) :: dimension
        integer, intent(out) :: rc
        integer :: lbnd(1:2), ubnd(1:2), i,j
        real (ESMF_KIND_R4), pointer :: coordESMF(:,:)
        real,allocatable :: coordBMI(:)

        rc = ESMF_SUCCESS

        call pBmiGetGridCoord(varname, dimension, coordBMI)
        call ESMF_GridGetCoord(grid, coordDim=dimension, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            computationalLBound=lbnd, computationalUBound=ubnd, &
            farrayPtr=coordESMF, rc=rc)

        do j=lbnd(2),ubnd(2)
            do i=lbnd(1),ubnd(1)
                coordESMF(i,j)=coordBMI(i*j)
            end do
        end do

    end subroutine setGridCoord2D

    subroutine setGridCoord3D(grid,varname,dimension,rc)
        type(ESMF_Grid),intent(inout) :: grid
        character(*), intent(in) :: varname
        integer, intent(in) :: dimension
        integer, intent(out) :: rc
        integer :: lbnd(1:3), ubnd(1:3), i,j,k
        real (ESMF_KIND_R4), pointer :: coordESMF(:,:,:)
        real,allocatable :: coordBMI(:)

        rc = ESMF_SUCCESS

        call pBmiGetGridCoord(varname, dimension, coordBMI)
        call ESMF_GridGetCoord(grid, coordDim=dimension, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            computationalLBound=lbnd, computationalUBound=ubnd, &
            farrayPtr=coordESMF, rc=rc)

        do k=lbnd(3),ubnd(3)
            do j=lbnd(2),ubnd(2)
                do i=lbnd(1),ubnd(1)
                    coordESMF(i,j,k)=coordBMI(i*j*k)
                end do
            end do
        end do

    end subroutine setGridCoord3D

    subroutine setGridCoord1D(grid,varname,dimension,rc)
        type(ESMF_Grid),intent(inout) :: grid
        character(*), intent(in) :: varname
        integer, intent(in) :: dimension
        integer, intent(out) :: rc
        integer :: lbnd(1:1), ubnd(1:1), i
        real (ESMF_KIND_R4), pointer :: coordESMF(:)
        real,allocatable :: coordBMI(:)

        rc = ESMF_SUCCESS

        call pBmiGetGridCoord(varname, dimension, coordBMI)

        call ESMF_GridGetCoord(grid, coordDim=dimension, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            computationalLBound=lbnd, computationalUBound=ubnd, &
            farrayPtr=coordESMF, rc=rc)

        do i=lbnd(1),ubnd(1)
            coordESMF(i) = coordBMI(i)
        enddo

    end subroutine setGridCoord1D

    function gridCreateNoDecomp(varname,rank,rc) result(return_grid)
        type(ESMF_Grid) :: return_grid
        integer, intent(out) :: rc
        integer, intent(in) :: rank
        character(*),intent(in) ::varname
        integer :: i

        ! local variables
        integer :: gridtype

        rc = ESMF_SUCCESS

        call pBmiGetGridType (varname,gridtype)

        if(rank .lt. 1) then
            rc = ESMF_RC_OBJ_CREATE
            if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                msg="Cannot create grid with rank less than one! <"//trim(varname)//">", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        else if(rank .gt. 3) then
            rc = ESMF_RC_OBJ_CREATE
            if (BMIAdapter_LogFoundError(rcToCheck=rc, &
                msg="Cannot create grid with rank greater than three!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

        select case (gridtype)
            case(BMI_GRID_TYPE_UNIFORM)
                ! ESMF_GridCreateNoPeriDim - Create a Grid with no periodic dim and a regular distribution
                ! Temporarily no decomp

                return_grid = ESMF_GridCreateNoPeriDim( &
                    minIndex=minIndex(varname,rank), &
                    maxIndex=maxIndex(varname,rank), &
                    regDecomp=noDecomp(varname,rank), &
                    name=varname, rc=rc)
            case(BMI_GRID_TYPE_RECTILINEAR)
                if(rank .eq. 1) then
                    return_grid=ESMF_GridCreateNoPeriDim( &
                        minIndex=minIndex(varname,rank), &
                        maxIndex=maxIndex(varname,rank), &
                        regDecomp=noDecomp(varname,rank),  &
                        coordDep1=(/1/), &
                        name=varname,  rc=rc)
                else if(rank .eq. 2) then
                    return_grid=ESMF_GridCreateNoPeriDim( &
                        minIndex=minIndex(varname,rank), &
                        maxIndex=maxIndex(varname,rank), &
                        regDecomp=noDecomp(varname,rank),  &
                        coordDep1=(/1/), &
                        coordDep2=(/2/), &
                        name=varname,  rc=rc)
                else if(rank .eq. 3) then
                    return_grid=ESMF_GridCreateNoPeriDim( &
                        minIndex=minIndex(varname,rank), &
                        maxIndex=maxIndex(varname,rank), &
                        regDecomp=noDecomp(varname,rank),  &
                        coordDep1=(/1/), &
                        coordDep2=(/2/), &
                        coordDep3=(/3/), &
                        name=varname,  rc=rc)
                end if
                call ESMF_GridAddCoord(return_grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

                do i=1,rank
                    call setGridCoord1D(return_grid,varname,i,rc)
                end do

            case(BMI_GRID_TYPE_STRUCTURED)
                if(rank .eq. 1) then
                    return_grid=ESMF_GridCreateNoPeriDim( &
                        minIndex=minIndex(varname,rank), &
                        maxIndex=maxIndex(varname,rank), &
                        regDecomp=noDecomp(varname,rank),  &
                        coordDep1=(/1/), &
                        name=varname,  rc=rc)
                    call ESMF_GridAddCoord(return_grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

                    do i=1,rank
                        call setGridCoord1D(return_grid,varname,i,rc)
                    end do
                else if(rank .eq. 2) then
                    return_grid=ESMF_GridCreateNoPeriDim( &
                        minIndex=minIndex(varname,rank), &
                        maxIndex=maxIndex(varname,rank), &
                        regDecomp=noDecomp(varname,rank),  &
                        coordDep1=(/1,2/), &
                        coordDep2=(/1,2/), &
                        name=varname,  rc=rc)
                    call ESMF_GridAddCoord(return_grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

                    do i=1,rank
                        call setGridCoord2D(return_grid,varname,i,rc)
                    end do
                else if(rank .eq. 3) then
                    return_grid=ESMF_GridCreateNoPeriDim( &
                        minIndex=minIndex(varname,rank), &
                        maxIndex=maxIndex(varname,rank), &
                        regDecomp=noDecomp(varname,rank),  &
                        coordDep1=(/1,2,3/), &
                        coordDep2=(/1,2,3/), &
                        coordDep3=(/1,2,3/), &
                        name=varname,  rc=rc)
                    call ESMF_GridAddCoord(return_grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

                    do i=1,rank
                        call setGridCoord1D(return_grid,varname,i,rc)
                    end do
                end if

            case(BMI_GRID_TYPE_UNSTRUCTURED)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_NUMBER)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_UNKNOWN)
                rc = ESMF_RC_NOT_IMPL
                return
            case default
                rc = ESMF_RC_NOT_IMPL
                return
        end select

        if (rc .ne. ESMF_SUCCESS) return
        call BMIAdapter_LogWrite("Grid created: " // trim(varname), ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

    END FUNCTION gridCreateNoDecomp

    !========================================
    ! Grid Comparison Function for BMI variables
    !========================================

    function BMIAdapter_GridComparison(var_name_1,var_name_2,rc) result(equivalent)
        character (BMI_MAXVARNAMESTR), intent(in) :: var_name_1,var_name_2
        logical :: equivalent
        integer :: rank_1, rank_2
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS
        equivalent = .true.

        call pBmiGetVarRank(var_name_1,rank_1)
        call pBmiGetVarRank(var_name_2,rank_2)

        if(rank_1 .ne. rank_2) then
            equivalent = .false.
            return
        end if

        equivalent = BMIAdapter_GridComparisonRank(var_name_1,var_name_2,rank_1,rank_2,rc)

    end function BMIAdapter_GridComparison

    function BMIAdapter_GridComparisonRank(var_name_1,var_name_2,rank_1,rank_2,rc) result (equivalent)
        character (BMI_MAXVARNAMESTR), intent(in) :: var_name_1,var_name_2
        integer, intent (in) :: rank_1, rank_2
        integer, intent(out) :: rc

        real :: spacing_1(1:rank_1), spacing_2(1:rank_2)
        real :: origin_1(1:rank_1), origin_2(1:rank_2)
        integer :: shape_1(1:rank_1), shape_2(1:rank_2)
        integer :: gridtype_1, gridtype_2
        logical :: equivalent

        rc = ESMF_SUCCESS
        equivalent = .true.

        call pBmiGetGridOrigin (var_name_1,origin_1)
        call pBmiGetGridOrigin (var_name_2, origin_2)

        if(origin_1(1) .ne. origin_2 (1) .or. origin_1(2) .ne. origin_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridSpacing (var_name_1,spacing_1)
        call pBmiGetGridSpacing (var_name_2, spacing_2)

        if(spacing_1(1) .ne. spacing_2 (1) .or. spacing_1(2) .ne. spacing_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridShape (var_name_1,shape_1)
        call pBmiGetGridShape (var_name_2,shape_2)

        if(shape_1(1) .ne. shape_2 (1) .or. shape_1(2) .ne. shape_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridType(var_name_2,gridtype_1)
        call pBmiGetGridType(var_name_1,gridtype_2)

        if(gridtype_1 .ne. gridtype_2) then
            equivalent = .false.
            return
        end if

    end function BMIAdapter_GridComparisonRank

    !###########################
    !# Section nnnn            #
    !# Standard Out Procedures #
    !###########################

    !=========================
    !=Print Model Information=
    !=========================

    subroutine BMIAdapter_PrintComponentInfo()
        implicit none
        character(len=BMI_MAXVARNAMESTR), pointer        :: invarnames(:)
        character(len=BMI_MAXVARNAMESTR), pointer     :: outvarnames(:)
        character(len=BMI_MAXCOMPNAMESTR),pointer    :: compname
        character(len=10)                               ::tunits
        real                            :: start
        real                            :: end
        real                            :: step
        integer :: i
        integer :: rank

        call pBmiGetInputVarNames(invarnames)
        call pBmiGetOutputVarNames(outvarnames)
        call pBmiGetComponentName(compname)
        call pBmiGetTimeStep(step)
        call pBmiGetStartTime(start)
        call pBmiGetEndTime(end)
        call pBmiGetTimeUnits(tunits)

        print *, "BMI Component Info"
        print *, "     Step: ",step
        print *, "     Start: ",start
        print *, "     End: ",end
        print *, "     Time Units: ",tunits
        print *, "     # Input Variables: ",SIZE(invarnames)
        print *, "     Input Variable Names: ",invarnames
        print *, "     # Output Variables: ",SIZE(outvarnames)
        print *, "     Output Variable Names: ",outvarnames
        print *, "     Component Name: ",compname

        do i=1,SIZE(invarnames)
            print *,"In Variable Info: ",invarnames(i)
            call pBmiGetVarRank(invarnames(i),rank)
            call BMIAdapter_PrintVarInfoRank(invarnames(i),rank)
        end do

        do i=1,SIZE(outvarnames)
            print *,"Out Variable Info: ",outvarnames(i)
            call pBmiGetVarRank(outvarnames(i),rank)
            call BMIAdapter_PrintVarInfoRank(outvarnames(i),rank)
        end do

    end subroutine

    !========================================
    ! Print all available info for a sigle
    ! variable
    !========================================

    subroutine BMIAdapter_PrintVarInfoRank(var_name,var_rank)
        implicit none
        character (len=BMI_MAXVARNAMESTR), intent (in) :: var_name
        integer, intent(in) :: var_rank
        integer             :: type
        character(len=10)   :: units, dict_units ! Assumed length for units string
        integer             :: gtype
        integer,dimension(1:var_rank)     :: gshape  ! Assumed shape for grid shape array
        real, dimension(1:var_rank)       :: gspacing, gorigin ! Assumed shape for grid spacing array

        call pBmiGetVarType ( var_name, type)
        call pBmiGetVarUnits ( var_name, units)
        call pBmiGetGridType ( var_name, gtype)
        call pBmiGetGridShape ( var_name, gshape)
        call pBmiGetGridSpacing ( var_name, gspacing)
        call pBmiGetGridOrigin ( var_name, gorigin)

        print *, "     Type: ", type
        print *, "     Units: ", units
        print *, "     Rank: ", var_rank
        print *, "     Grid Type: ", gtype
        print *, "     Grid Shape: ", gshape
        print *, "     Grid Spacing: ", gspacing
        print *, "     Grid Origin: ", gorigin

    end subroutine

        !========================================
        ! Print Field Data
        !========================================

    subroutine BMIAdapter_PrintFieldData(name)
        character(*),intent(in) :: name
        integer :: rank

        call pBmiGetVarRank(name,rank)
        call BMIAdapter_PrintFieldDataRank(name,rank)

    end subroutine

    subroutine BMIAdapter_PrintFieldDataRank(name, rank)
        character(*),intent(in) :: name
        integer,intent(in) :: rank
        real,pointer :: bmiflat (:)
        real,pointer :: bmireshape_2D (:,:)
        real,pointer :: bmireshape_3D (:,:,:)
        integer, dimension (rank) :: gridshape

        call pBmiGetGridShape (name, gridshape)

        if(rank == 1) then
            call pBmiGetReal(name,bmiflat)
            print *,"Field Data: ",trim(name)
            print *,bmiflat
        else if(rank == 2) then
            call pBmiGetReal(name,bmiflat)
            bmireshape_2D (1:gridshape(1), 1:gridshape(2)) => bmiflat
            print *,"Field Data: ",trim(name)
            print *,bmireshape_2D
        else if(rank == 3) then
            call pBmiGetReal(name,bmiflat)
            bmireshape_3D (1:gridshape(1), 1:gridshape(2), 1:gridshape(3)) => bmiflat
            print *,"Field Data: ",trim(name)
            print *,bmireshape_3D
        else
            print *,"Field Data: ",trim(name)
            print *,"<Error: Rank out of range. Cannot print field data.>"
        end if
    end subroutine

    !========================================
    ! Print BMI model current time
    !========================================

    SUBROUTINE BMIAdapter_PrintCurrentTime()
        real                          :: bmi_time

        call pBmiGetCurrentTime(bmi_time)
        print *,"BMI Current Time: ", bmi_time

    END SUBROUTINE

    !############################
    !# BMI Adapter Log Wrappers #
    !############################

    logical function BMIAdapter_LogFoundError(rcToCheck,msg,line,file,method,rcToReturn,log)
        integer,          intent(in),    optional :: rcToCheck
        character(len=*), intent(in),    optional :: msg
        integer,          intent(in),    optional :: line
        character(len=*), intent(in),    optional :: file
        character(len=*), intent(in),    optional :: method
        integer,          intent(inout), optional :: rcToReturn
        type(ESMF_Log),   intent(inout), optional :: log

        ! Local Variable
        character(len=*),parameter :: errPrefix = "BMI Adapter ERROR: "

        BMIAdapter_LogFoundError = ESMF_LogFoundError(rcToCheck=rcToCheck, &
            msg = errPrefix // msg, line=line, file=file, method=method, &
            rcToReturn=rcToReturn, log=log)

    end function BMIAdapter_LogFoundError

    recursive subroutine BMIAdapter_LogWrite(msg, logmsgFlag, line, file, method, log, rc)
        character(len=*),      intent(in)             :: msg
        type(ESMF_LogMsg_Flag),intent(in),optional    :: logmsgFlag
        integer,               intent(in),   optional :: line
        character(len=*),      intent(in),   optional :: file
        character(len=*),      intent(in),   optional :: method
        type(ESMF_Log),        intent(inout),optional :: log
        integer,               intent(out),  optional :: rc

        ! Local Variable
        character(len=*),parameter :: msgPrefix = "BMI Adapter MSG: "

        call ESMF_LogWrite(msg=msgPrefix // msg,logmsgFlag=logmsgFlag, &
            line=line,file=file,method=method,log=log,rc=rc)

    end subroutine BMIAdapter_LogWrite

        !########################################
    !# Dummy routines for when not provided #
    !########################################

    subroutine defaultGetStartTime (start)
        implicit none
        real,intent(out) :: start
        start = 0.
    end subroutine

    subroutine defaultGetEndTime (end)
        implicit none
        real, intent (out) :: end
        end = 60.
    end subroutine

    subroutine defaultGetCurrentTime (time)
        implicit none
        real, intent (out) :: time
        time = 0.
    end subroutine

    subroutine defaultGetVarType (var_name, type)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (out) :: type
        type = BMI_VAR_TYPE_DOUBLE
    end subroutine

    subroutine defaultGetVarRank (var_name, rank)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (out) :: rank
        rank = 2
    end subroutine

    subroutine defaultGetGridType (var_name, type)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (out) :: type
        type = BMI_GRID_TYPE_UNIFORM
    end subroutine

    subroutine defaultGetGridOrigin (var_name, origin)
        implicit none
        character (len=*), intent (in) :: var_name
        real, dimension (:), intent (out) :: origin
        integer :: rank

        call pBmiGetVarRank(var_name,rank)
        call defaultgetGridOriginRank(var_name,rank,origin)

    end subroutine

    subroutine defaultGetGridOriginRank(var_name, rank, origin)
        implicit none
        integer, intent(in) :: rank
        character (len=*), intent(in) :: var_name
        real, dimension(rank), intent(out) :: origin
        integer :: i

        do i=1,rank
            origin(i) = 0.
        end do
    end subroutine

    subroutine defaultGetGridCoord(var_name,dimension,gridX)
        character (len=*), intent (in) :: var_name
        integer, intent(in) :: dimension
        real, dimension (:), intent (out) :: gridX
        gridX(1)=1.
        gridX(2)=2.
        gridX(3)=3.
    end subroutine

    subroutine defaultGetDoubleAt (var_name, dest, inds)
        implicit none
        character (len=*), intent (in) :: var_name
        real, pointer, intent (inout) :: dest(:)
        integer, intent (in) :: inds(:)
    end subroutine

    subroutine defaultSetdouble (var_name, src)
        implicit none
        character (len=*), intent (in) :: var_name
        real, intent (in) :: src (*)
    end subroutine

    subroutine defaultSetDoubleAt (var_name, inds, src)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (in) :: inds(:)
        real, intent (in) :: src (*)
    end subroutine

    subroutine defaultGetInputVarNames (names)
        implicit none
        character (*),pointer, intent (out) :: names(:)

        character (len=BMI_MAXVARNAMESTR), target, &
            dimension (0) :: input_items

        names => input_items

    end subroutine

    subroutine defaultGetOutputVarNames (names)
        implicit none
        character (*),pointer, intent (out) :: names(:)
        character (len=BMI_MAXVARNAMESTR), target, &
            dimension (0) :: output_items

        names => output_items
    end subroutine

    subroutine defaultGetReal2D (var_name,dest)
        implicit none
        character (len=*), intent (in) :: var_name
        real, pointer, intent (inout) :: dest(:,:)
        real, target, dimension(0,0) :: empty

        dest => empty
    end subroutine

    subroutine defaultGetReal3D (var_name,dest)
        implicit none
        character (len=*), intent (in) :: var_name
        real, pointer, intent (inout) :: dest(:,:,:)
        real, target, dimension(0,0,0) :: empty

        dest => empty
    end subroutine

end module esmfBmiAdapter
