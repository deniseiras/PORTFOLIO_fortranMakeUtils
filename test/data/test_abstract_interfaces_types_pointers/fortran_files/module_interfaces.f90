module interfaces

   #include "mpas_derived_types.inc" 

contains

   ! test with a pointer to a object (core_type) that points to a abstract interface
   ! (mpas_core_types.inc), which is implemented by atm_core.f90
   subroutine atm_setup_core(core)
      use core_type
      use atm_core, only : atm_core_init, atm_core_run, atm_core_finalize

      implicit none

      type (core_type), pointer :: core

      core % core_init => atm_core_init
      core % core_run => atm_core_run
      core % core_finalize => atm_core_finalize

      ! calls only core run
      call(core % core_run)

   end subroutine

   
   ! test with a pointer that points to a abstract interface
   subroutine main_interfaces
      use atm_core, only : atm_core_init

      implicit none
      procedure (mpas_core_init_function), pointer, nopass :: core_init => null()
      core_init => atm_core_init
      
      ! calls only core init
      call(core_init)

   end subroutine main_interfaces

end module interfaces
