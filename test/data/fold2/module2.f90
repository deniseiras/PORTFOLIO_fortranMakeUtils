module module2

    use module1
contains

    subroutine recursiva()
        call recursiva()
        call mainused1(x,y)
    end subroutine recursiva

    
end module module2