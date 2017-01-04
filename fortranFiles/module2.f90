module module2

    use moduleused

contains

subroutine used1(a,b)

end subroutine used1


subroutine used2()

    call getVariable(a)

end subroutine used2


subroutine notused2()
    asdsad
end subroutine notused2


function funcUsed1(s,g)

end function funcUsed1


subroutine main()

    x = funcMainUsed1(g,6)
    y = funcUsed1(3)
    call used2()

    !  call interprocUsed2(kk)
    !call interprocUsed2(kk)

end subroutine main












function funcNotUsed1(s,g)
    kkkk
end function funcNotUsed1

subroutine notused1()
    asdsad
end subroutine notused1

end module2