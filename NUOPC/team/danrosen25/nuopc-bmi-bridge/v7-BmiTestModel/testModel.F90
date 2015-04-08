module TestModel_class
    implicit none

    type :: TestModel
        private
        real :: dt
        real :: t
        real :: t_end

        integer :: n_x
        integer :: n_y

        real :: dx
        real :: dy

        real, pointer :: z(:,:)
        real, pointer :: z_temp(:,:)
    end type TestModel

    integer, parameter :: VAR_TYPE_DOUBLE = 8
    integer, parameter :: GRID_TYPE_UNIFORM = 1

    type(TestModel) :: self ! The compliant model configured by the configuration file or default model values

    integer, parameter :: component_name_length = 19
    character (component_name_length), target :: &
        component_name = "Fortran test model"

    ! start exchange item list
    !integer, parameter :: input_item_count = 1
    integer, parameter :: input_item_count = 2
    integer, parameter :: output_item_count = 1
    integer, parameter :: item_name_length = 22
    !      character (len=item_name_length), target, &
    !        dimension (input_item_count) :: &
    !        input_items = (/'surface_elevation    '/)
    character (len=item_name_length), target, &
        dimension (input_item_count) :: &
        input_items = (/'surface_elevation    ','surface_test         '/)

    character (len=item_name_length), target, &
        dimension (output_item_count) :: &
        output_items = (/'surface_elevation   '/)
          ! end exchange item list
contains
    subroutine initialize (config_file)
        implicit none

        character (len=*), intent (in) :: config_file
        ! end declaration section

        if (len (config_file)>0) then
            open (15, file=config_file)
            read (15, *) self%dt, self%t_end, self%n_x, self%n_y
            close (15)
        else
            self%dt = 5.
            self%t_end = 20.
            self%n_x = 10
            self%n_y = 20
        end if

        self%t = 0.
        self%dx = 1.
        self%dy = 1.

        allocate (self%z(self%n_x, self%n_y))
        allocate (self%z_temp(self%n_x, self%n_y))

        self%z = 0.
        self%z_temp = 0.

        call setBC (self%z)
        call setBC (self%z_temp)

    end subroutine

    subroutine setBC (z)
        implicit none
        real, dimension (:,:), intent (out) :: z

        integer :: i,j

        do i = 1, size(z,1)
            do j = 1, size(z,2)
                z(i,j) = i*j
            end do
        end do

    end subroutine


    subroutine finalize ()
        implicit none
        ! end declaration section

        deallocate (self%z)
        deallocate (self%z_temp)
    end subroutine

    subroutine update ()
        implicit none
        ! end declaration section

        integer :: i, j

        do i = 1, size(self%z,1)
            do j = 1, size(self%z,2)
                self%z(i,j) = self%z(i,j)+1
            end do
        end do

        self%t = self%t + self%dt

    end subroutine

    subroutine updateUntil (t)
        implicit none
        real, intent (in) :: t
        ! end declaration section

        integer :: n
        integer :: n_steps
        real :: saved_dt

        n_steps = (t-self%t)/self%dt

        do n = 1, n_steps
            call update ()
        end do

        if (t>self%t) then
            saved_dt = self%dt
            self%dt = t - self%t

            call update ()

            self%dt = saved_dt
        endif

    end subroutine

    subroutine getStartTime (start)
        implicit none
        real, intent (out) :: start
        ! end declaration BMI_Get_start_time

        start = 0.
    end subroutine

    subroutine getEndTime (end)
        implicit none
        real, intent (out) :: end
        ! end declaration BMI_Get_end_time

        end = self%t_end
    end subroutine

    subroutine getCurrentTime (time)
        implicit none
        real, intent (out) :: time
        ! end declaration BMI_Get_current_time

        time = self%t
    end subroutine

    subroutine getTimeStep (dt)
        implicit none
        real, intent (out) :: dt
        ! end declaration section

        dt = self%dt
    end subroutine

    subroutine getTimeUnits (units)
        implicit none
        character (len=*), intent (out) :: units
        ! end declaration section

        units = "s"
    end subroutine

    subroutine getVarType (var_name, type)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (out) :: type
        ! end declaration section

        type = VAR_TYPE_DOUBLE
    end subroutine

    subroutine getVarUnits (var_name, units)
        implicit none
        character (len=*), intent (in) :: var_name
        character (len=*), intent (out) :: units
        ! end declaration section

        units = "meter"
    end subroutine

    subroutine getVarRank (var_name, rank)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (out) :: rank
        ! end declaration section

        rank = 2
    end subroutine

    subroutine getGridType (var_name, type)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (out) :: type
        ! end declaration section

        type = GRID_TYPE_UNIFORM
    end subroutine

    subroutine getGridShape (var_name, shape)
        implicit none
        character (len=*), intent (in) :: var_name
        integer, dimension (:), intent (out) :: shape
        ! end declaration section

        shape(1) = self%n_x
        shape(2) = self%n_y
    end subroutine

    subroutine getGridSpacing (var_name, spacing)
        implicit none
        character (len=*), intent (in) :: var_name
        real, dimension (:), intent (out) :: spacing
        ! end declaration section

        spacing(1) = self%dx
        spacing(2) = self%dy
    end subroutine

    subroutine getGridOrigin (var_name, origin)
        implicit none
        character (len=*), intent (in) :: var_name
        real, dimension (:), intent (out) :: origin
        ! end declaration section

        origin(1) = 0.
        origin(2) = 0.
    end subroutine

    subroutine getDouble (var_name, dest) ! compiler standards for order of dimensions
        use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
        implicit none
        character (len=*), intent (in) :: var_name
        real, pointer, intent (inout) :: dest(:)
        ! end declaration section

        real, pointer :: src_as_1d (:)
        type (c_ptr) :: src

        select case (var_name)
            case ('surface_elevation')
                src = c_loc (self%z(1,1))
        end select

        if (associated (dest)) then
            call C_F_POINTER (src, src_as_1d, [self%n_x*self%n_y])
            dest = src_as_1d
        else
            call C_F_POINTER (src, dest, [self%n_x*self%n_y])
        endif

    end subroutine

    subroutine getDoubleCopy (var_name, dest)
        implicit none
        character (len=*), intent (in) :: var_name
        real, intent (out) :: dest (*)
        ! end declaration section

        select case (var_name)
            case ('surface_elevation')
                call copy_array (dest, self%z, self%n_x * self%n_y)
        end select

    end subroutine

    subroutine getDoubleAtIndices (var_name, dest, inds)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
        implicit none
        character (len=*), intent (in) :: var_name
        real, pointer, intent (inout) :: dest(:)
        integer, intent (in) :: inds(:)
        ! end declaration section

        real, pointer :: src_as_1d (:)
        type (c_ptr) :: src
        integer :: i

        select case (var_name)
            case ('surface_elevation')
                src = c_loc (self%z(1,1))
        end select

        call C_F_POINTER (src, src_as_1d, [self%n_x*self%n_y])

        do i = 1, size (inds)
            dest(i) = src_as_1d(inds(i))
        end do

    end subroutine

    subroutine setDouble (var_name, src)
        implicit none
        character (len=*), intent (in) :: var_name
        real, intent (in) :: src (*)
        ! end declaration section

        call copy_array (self%z, src, self%n_x * self%n_y)

    end subroutine

    subroutine setDoubleAtIndices (var_name, inds, src)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
        implicit none
        character (len=*), intent (in) :: var_name
        integer, intent (in) :: inds(:)
        real, intent (in) :: src (*)
        ! end declaration section

        real, pointer :: dest_as_1d (:)
        type (c_ptr) :: dest
        integer :: i

        select case (var_name)
            case ('surface_elevation')
                dest = c_loc (self%z(1,1))
        end select

        call C_F_POINTER (dest, dest_as_1d, [self%n_x*self%n_y])

        do i = 1, size (inds)
            dest_as_1d(inds(i)) = src(i)
        end do

    end subroutine

    subroutine copy_array (dest, src, n)
        real, intent (out) :: dest(n)
        real, intent (in) :: src(n)
        integer, intent (in) :: n
        dest = src
    end subroutine copy_array

    subroutine getInputVarNames (names)
        implicit none
        character (*), pointer, intent (out) :: names(:)
        ! end declaration section

        names => input_items
    end subroutine

    subroutine getOutputVarNames (names)
        implicit none
        character (*), pointer, intent (out) :: names(:)
        ! end declaration section

        names => output_items
    end subroutine

    subroutine getComponentName (name)
        implicit none
        character (len=*), pointer, intent (out) :: name
        ! end declaration section
        name => component_name
    end subroutine

end module
