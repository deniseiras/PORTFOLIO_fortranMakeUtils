   abstract interface
      function mpas_core_init_function(domain, timeStamp) result(iErr)
         import domain_type
         type (domain_type), intent(inout) :: domain
         character (len=*), intent(out) :: timeStamp
         integer :: iErr
      end function
   end interface

   abstract interface
      function mpas_core_run_function(domain) result(iErr)
         import domain_type
         type (domain_type), intent(inout) :: domain
         integer :: iErr
      end function
   end interface

   abstract interface
      function mpas_core_finalize_function(domain) result(iErr)
         import domain_type
         type (domain_type), intent(inout) :: domain
         integer :: iErr
      end function
   end interface


   type core_type
      type (domain_type), pointer :: domainlist => null()

      ! Core init, run, and finalize function pointers
      procedure (mpas_core_init_function), pointer, nopass :: core_init => null()
      procedure (mpas_core_run_function), pointer, nopass :: core_run => null()
      procedure (mpas_core_finalize_function), pointer, nopass :: core_finalize => null()

      ! core_type is a linked list
      type (core_type), pointer :: next => null()
   end type core_type