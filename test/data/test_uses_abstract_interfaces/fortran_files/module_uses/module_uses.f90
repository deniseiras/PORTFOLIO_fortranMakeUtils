module uses

    use interfaces

contains

    ! calls only via pointer
    subroutine uses_interface()
        procedure(SUB) SUB_FOR_POINTER
        procedure(SUB), pointer::P
        P => SUB_FOR_POINTER
        call P(5, 10)
    end subroutine uses_interface

   
end module uses