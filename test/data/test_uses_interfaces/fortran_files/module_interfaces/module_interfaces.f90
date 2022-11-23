module interface
contains
    subroutine S1(IARG)
        IARG = 1
    end subroutine S1
    subroutine S2(RARG)
        RARG = 1.1
    end subroutine S2
    subroutine S3(LARG)
        logical LARG
        LARG = .true.
    end subroutine S3

    end module interface
