module uses

    use routines

contains

    ! calls 
    subroutine main_routines
        integer :: sum
        call sub1(5, 10)
        sum = func1(5,10)
        call calls_routines()
    end subroutine main_routines

    
end module uses