module recursive

contains

    recursive subroutine recursiva()
        call recursiva()
        call recursive_inside()
        call non_recursive()
    end subroutine recursiva

    recursive subroutine recursive_inside()
        call recursive_inside()
        call non_recursive()
    end subroutine recursive_inside
    
    subroutine non_recursive()
        ! ignorethis
    end subroutine non_recursive

end module recursive