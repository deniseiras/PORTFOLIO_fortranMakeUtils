module interfaces
    abstract interface
        subroutine SUB(X, Y)
            integer, intent(IN)::X
            integer, intent(IN)::Y
        end subroutine
    end interface

contains

    subroutine SUB1(X, Y)
        integer, intent(IN)::X
        integer, intent(IN)::Y
        print *, "The sum of X and Y is: ", X + Y
    end subroutine

    subroutine main_interfaces
        procedure(SUB) SUB_FOR_POINTER
        procedure(SUB), pointer::P
        P => SUB_FOR_POINTER
        call P(5, 10)

    end subroutine main_interfaces

end module interfaces
