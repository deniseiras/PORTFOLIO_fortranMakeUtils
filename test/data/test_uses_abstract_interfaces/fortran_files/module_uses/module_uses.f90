module uses

    use interfaces
    #include "include_interfaces_in_type.inc"

contains

    ! calls simple interface only via pointer
    subroutine uses_interface()
        procedure(SUB) SUB_FOR_POINTER
        procedure(SUB), pointer::P
        P => SUB_FOR_POINTER
        call P(5, 10)
    end subroutine uses_interface

   
    ! Calls interface methohd of type atm_core
    subroutine atm_setup_core(core)
        
        use atm_core, only : atm_core_run
  
        implicit none
 
        type (core_type), pointer :: core  
        core % core_run => atm_core_run

  
     end subroutine atm_setup_core
end module uses