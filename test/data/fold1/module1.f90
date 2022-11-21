module module1

    use moduleUsed


interface interprocNotUsed
    module procedure interprocNotUsed_1, \
    interprocNotUsed_2
end interface

interface interprocUsed
    module procedure interprocUsed_1
    module procedure interprocUsed_2
end interface

interface interprocUsed2
    module procedure interprocUsed2_1, interprocUsed2_2 \
    interprocUsed2_3
end interface


subroutine mainused1(x,r)
    call mainused1(a,s)
end subroutine mainused1






FUNCTION funcMainUsed1(s,g)
    kkkk
end FUNCTION funcMainUsed1


subroutine mainnotused1()
    asdsad
end subroutine mainnotused1


function funcMainNotUsed1(s,g)
    kkkkk
end function funcMainNotUsed1


subroutine interprocUsed_1(d,t)
    ssss
end subroutune INTERprocUsed_1

subroutine interprocUsed_2(d,t)
    ssss
end subroutune interprocUsed_2

subroutine interprocNotUsed_1(d,t)
    ffff
end subroutune interprocNotUsed_1

subroutine interprocNotUsed_2(d,t)
    ffff
end subroutune interprocNotUsed_2

subroutine interprocUsed2_1(d,t)
    ffff
end subroutune interprocUsed2_1

subroutine interprocUsed2_2(d,t)
    ffff
end subroutune interprocUsed2_2

subroutine interprocUsed2_3(d,t)
    ffff
end subroutune interprocUsed2_3

end module1