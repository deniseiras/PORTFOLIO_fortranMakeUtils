module includes 

#include "includes.inc"

contains

    subroutine SUB1(X)
        integer, intent(IN)::X
        print *, "SUB1 = ", X
    end subroutine

    subroutine main_includes
        call SUB1(1)
        call sub_included(2)
    end subroutine main_includes

end module includes
