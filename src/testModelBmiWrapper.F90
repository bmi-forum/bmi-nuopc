module BmiDefinitions
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

end module BmiDefinitions


subroutine BMI_Initialize (config_file)
    use TestModel_class, only: modelInitialize => initialize
    implicit none
    character(*), intent(in) :: config_file

    call modelInitialize(config_file)

end subroutine BMI_Initialize

subroutine BMI_Finalize ()
    use TestModel_class, only: modelFinalize => finalize
    implicit none

    call modelFinalize()

end subroutine BMI_Finalize

subroutine BMI_Update ()
    use TestModel_class, only: modelUpdate => update
    implicit none
    ! end declaration section

    call modelUpdate()

end subroutine BMI_Update

subroutine BMI_Update_until (t)
    use TestModel_class, only: modelUpdateUntil => updateUntil
    implicit none
    real, intent(in) :: t

    call modelUpdateUntil(t)

end subroutine BMI_Update_until

subroutine BMI_Get_start_time (start)
    use TestModel_class, only: modelGetStartTime => getStartTime
    implicit none
    real,intent(out) :: start

    call modelGetStartTime(start)

end subroutine BMI_Get_start_time

subroutine BMI_Get_end_time (end)
    use TestModel_class, only: modelGetEndTime => getEndTime
    implicit none
    real, intent (out) :: end

    call modelGetEndTime(end)

end subroutine BMI_Get_end_time

subroutine BMI_Get_current_time (time)
    use TestModel_class, only: modelGetCurrentTime => getCurrentTime
    implicit none
    real, intent (out) :: time

    call modelGetCurrentTime(time)

end subroutine BMI_Get_current_time

subroutine BMI_Get_time_step (dt)
    use TestModel_class, only: modelGetTimeStep => getTimeStep
    implicit none
    real, intent (out) :: dt

    call modelGetTimeStep(dt)

end subroutine BMI_Get_time_step

subroutine BMI_Get_time_units (units)
    use TestModel_class, only: modelGetTimeUnits => getTimeUnits
    implicit none
    character (len=*), intent (out) :: units

    call modelGetTimeUnits(units)

end subroutine BMI_Get_time_units

subroutine BMI_Get_var_type (var_name, type)
    use TestModel_class, only: modelGetVarType => getVarType
    implicit none
    character (len=*), intent (in) :: var_name
    integer, intent (out) :: type

    call modelGetVarType(var_name, type)

end subroutine BMI_Get_var_type

subroutine BMI_Get_var_units (var_name, units)
    use BmiDefinitions
    use TestModel_class, only: modelGetVarUnits => getVarUnits
    implicit none
    character (len=*), intent (in) :: var_name
    character (len=*), intent (out) :: units

    call modelGetVarUnits(var_name,units)

end subroutine BMI_Get_var_units

subroutine BMI_Get_var_rank (var_name, rank)
    use TestModel_class, only: modelGetVarRank => getVarRank
    implicit none
    character (len=*), intent (in) :: var_name
    integer, intent (out) :: rank

    call modelGetVarRank(var_name,rank)

end subroutine BMI_Get_var_rank

subroutine BMI_Get_grid_type (var_name, type)
    use BmiDefinitions
    use TestModel_class, only: modelGetGridType => getGridType
    implicit none
    character (len=*), intent (in) :: var_name
    integer, intent (out) :: type

    call modelGetGridType(var_name, type)

end subroutine BMI_Get_grid_type

subroutine BMI_Get_grid_shape (var_name, shape)
    use TestModel_class, only: modelGetGridShape => getGridShape
    implicit none
    character (len=*), intent (in) :: var_name
    integer, dimension (:), intent (out) :: shape

    call modelGetGridShape(var_name, shape)

end subroutine BMI_Get_grid_shape

subroutine BMI_Get_grid_spacing (var_name, spacing)
    use TestModel_class, only: modelGetGridSpacing => getGridSpacing
    implicit none
    character (len=*), intent (in) :: var_name
    real, dimension (:), intent (out) :: spacing

    call modelGetGridSpacing(var_name, spacing)

end subroutine BMI_Get_grid_spacing

subroutine BMI_Get_grid_origin (var_name, origin)
    use TestModel_class, only: modelGetGridOrigin => getGridOrigin
    implicit none
    character (len=*), intent (in) :: var_name
    real, dimension (:), intent (out) :: origin

    call modelGetGridOrigin(var_name, origin)

end subroutine BMI_Get_grid_origin

subroutine BMI_Get_double (var_name, dest)
    use TestModel_class, only: modelGetDouble => getDouble
    implicit none
    character (len=*), intent (in) :: var_name
    real, pointer, intent (inout) :: dest(:)

    call modelGetDouble(var_name, dest)

end subroutine BMI_Get_double


subroutine BMI_Get_double_at_indices (var_name, dest, inds)
    use TestModel_class, only: modelGetDoubleAtIndices => getDoubleAtIndices
    implicit none
    character (len=*), intent (in) :: var_name
    real, pointer, intent (inout) :: dest(:)
    integer, intent (in) :: inds(:)

    call modelGetDoubleAtIndices(var_name, dest, inds)

end subroutine BMI_Get_double_at_indices

subroutine BMI_Set_double (var_name, src)
    use TestModel_class, only: modelSetDouble => setDouble
    implicit none
    character (len=*), intent (in) :: var_name
    real, intent (in) :: src (*)

    call modelSetDouble(var_name, src)

end subroutine BMI_Set_double

subroutine BMI_Set_double_at_indices (var_name, inds, src)
    use TestModel_class, only: modelSetDoubleAtIndices => setDoubleAtIndices
    implicit none
    character (len=*), intent (in) :: var_name
    integer, intent (in) :: inds(:)
    real, intent (in) :: src (*)

    call modelSetDoubleAtIndices(var_name, inds, src)

end subroutine BMI_Set_double_at_indices

integer function BMI_Get_input_var_count()
    use TestModel_class, only: modelGetInputVarNames => getInputVarNames, modelVarNameLen => item_name_length
    implicit none

    ! local variables
    character (modelVarNameLen), pointer :: varnames(:)

    call modelGetInputVarNames(varnames)

    BMI_Get_input_var_count = size(varnames)

end function BMI_Get_input_var_count

integer function BMI_Get_output_var_count()
    use TestModel_class, only: modelGetOutputVarNames => getOutputVarNames, modelVarNameLen => item_name_length
    implicit none

    ! local variables
    character (modelVarNameLen), pointer :: varnames(:)

    call modelGetOutputVarNames(varnames)

    BMI_Get_output_var_count = size(varnames)

end function

subroutine BMI_Get_input_var_names (names)
    use TestModel_class, only: modelGetInputVarNames => getInputVarNames, modelVarNameLen => item_name_length
    implicit none
    character (*),pointer, intent (out) :: names(:)

    ! local variables
    character (modelVarNameLen), pointer :: varnames(:)

    call modelGetInputVarNames(varnames)

    !allocate(names(2))

    names => varnames

end subroutine BMI_Get_input_var_names

subroutine BMI_Get_output_var_names (names)
    use TestModel_class, only: modelGetOutputVarNames => getOutputVarNames, modelVarNameLen => item_name_length
    implicit none
    character (*),pointer, intent (out) :: names(:)

    ! local variables
    character(modelVarNameLen), pointer :: varnames(:)

    call modelGetOutputVarNames(varnames)

    names => varnames

end subroutine BMI_Get_output_var_names

subroutine BMI_Get_component_name (name)
    use BmiDefinitions
    use TestModel_class, only: modelGetComponentName => getComponentName, modelCompNameLen => component_name_length
    implicit none
    character (len=*),pointer, intent (out) :: name

!    ! Local Variable
!    character (modelCompNameLen), pointer :: compname

    call modelGetComponentName(name)

!    name => compname

end subroutine BMI_Get_component_name


