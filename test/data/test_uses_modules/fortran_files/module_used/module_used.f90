module routines
   
contains

    subroutine sub1(X, Y)
        integer, intent(IN)::X
        integer, intent(IN)::Y
        print *, "The sum of X and Y is: ", X + Y
    end subroutine

    function func1(X, Y) result(res)
        integer, intent(IN)::X
        integer, intent(IN)::Y
        integer :: result
        result = X + Y
    end function func1

    ! calls 
    subroutine calls_routines
        integer :: sum
        call sub1(5, 10)
        sum = func1(5,10)
    end subroutine calls_routines

end module routines
