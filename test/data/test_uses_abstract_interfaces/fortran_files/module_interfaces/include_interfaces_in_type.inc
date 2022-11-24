   abstract interface
      function mpas_core_run_function(domain) result(iErr)
         import domain_type
         type (domain_type), intent(inout) :: domain
         integer :: iErr
      end function
   end interface

   
   type core_type
      procedure (mpas_core_run_function), pointer, nopass :: core_run => null()
   end type core_type