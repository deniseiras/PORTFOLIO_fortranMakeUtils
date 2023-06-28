   type domain_type
      ! Back pointer to core
      type (core_type), pointer :: core => null()
      ! Domain_type is a linked list
      type (domain_type), pointer :: next => null()
   end type domain_type
