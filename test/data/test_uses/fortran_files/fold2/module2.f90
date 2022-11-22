module module2

    use module1

contains

    recursive subroutine recursiva()
        call recursiva()
    end subroutine recursiva

    subroutine uses_module1()
        call mainused1(x,y)
        call interprocUsed2()
    end subroutine uses_module1
    
end module module2