

























   subroutine bmi_wrf_initialize( config_file)
       use module_BMI_WRF, only: initialize
       implicit none
       character(len=*) ::  config_file
       call initialize()
   end subroutine bmi_wrf_initialize

   subroutine bmi_wrf_update(dt)    

       use module_BMI_WRF, only: update
       implicit none
       real*8 :: dt
       call update(dt)
   end subroutine  bmi_wrf_update

   subroutine bmi_wrf_finalize()
       use module_BMI_WRF, only: finalize
        CALL finalize
   end subroutine bmi_wrf_finalize
  
   subroutine bmi_wrf_run_model(config_file)
       use module_BMI_WRF, only: run_model
       character(len=*) ::  config_file
       CALL run_model()
   end subroutine bmi_wrf_run_model 

   subroutine bmi_wrf_get_2d_real(var_name,pp)
       use module_BMI_WRF, only: get_2d_real, l_ide, l_jde
      implicit none
      character(len=*) ::  var_name

       real :: pp(1,1)

      call get_2d_real(var_name,pp)
   end subroutine  bmi_wrf_get_2d_real


   subroutine bmi_wrf_get_3d_real(var_name,pp)

      use module_BMI_WRF, only: get_3d_real
      implicit none
      character(len=*) ::var_name

      real:: pp(1,1,1)
      call get_3d_real(var_name,pp)
   end subroutine bmi_wrf_get_3d_real

    subroutine BMI_wrf_Get_time_step ( dt)
         use module_BMI_WRF, only: BMI_Get_time_step
            implicit none
            real, intent (out) :: dt
            call BMI_Get_time_step(dt)
    end subroutine BMI_wrf_Get_time_step

    subroutine BMI_wrf_Get_time_units ( units)
         use module_BMI_WRF, only: BMI_Get_time_units
            implicit none
            character (len=*), intent (out) :: units
            call BMI_Get_time_units ( units)
    end subroutine BMI_wrf_Get_time_units

    subroutine BMI_wrf_Get_var_units ( var_name, units)
         use module_BMI_WRF, only: BMI_Get_var_units
            implicit none
            character (len=*), intent (in) :: var_name
            character (len=*), intent (out) :: units
            call BMI_Get_var_units ( var_name, units)
    end subroutine BMI_wrf_Get_var_units

    subroutine BMI_wrf_Get_grid_shape ( var_name, shape)
         use module_BMI_WRF, only: BMI_Get_grid_shape
            implicit none
            character (len=*), intent (in) :: var_name

            integer, dimension (3), intent (out) :: shape
            call BMI_Get_grid_shape ( var_name, shape) 
    end subroutine BMI_wrf_Get_grid_shape

    subroutine BMI_wrf_Get_grid_spacing ( var_name, spacing)
         use module_BMI_WRF, only: BMI_Get_grid_spacing
            implicit none
            character (len=*), intent (in) :: var_name
            
            real, dimension (3), intent (out) :: spacing
            call BMI_Get_grid_spacing ( var_name, spacing)
    end subroutine BMI_wrf_Get_grid_spacing


     subroutine BMI_wrf_Get_component_name ( name)
            implicit none
            character (len=*), pointer, intent (out) :: name
            
            character (len=19), target :: component_name
            component_name = "WRF"
            name => component_name
     end subroutine BMI_wrf_Get_component_name
