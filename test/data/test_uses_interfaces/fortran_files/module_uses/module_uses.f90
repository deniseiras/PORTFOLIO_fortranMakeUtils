module uses

    use interface

    interface SS
        subroutine SS1(IARG, JARG)
        end subroutine
        module procedure S1, S2, S3
    end interface

    contains

    subroutine SS1(IARG, JARG)
        IARG = 2
        JARG = 3
    end subroutine

    subroutine sub_uses
        call SS(3.14)              ! Calls subroutine S1 from M
        call SS(1, 2)             ! Calls subroutine SS1
    end subroutine sub_uses

end module uses

