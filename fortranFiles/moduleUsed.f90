module moduleUsed

    public :: integer modUsedInt

    interface getVariable
        module procedure getVariable2d
        module procedure getVariable3d
    end interface


contains

    subroutine moduleUsedSub()
        use moduleUsed2

    end subroutine moduleUsedSub


    subroutine getVariable3d(a)
        return var
    end subroutine getVariable3d


    subroutine getVariable2d(a)
        return var
    end subroutine getVariable2d


end moduleUsed