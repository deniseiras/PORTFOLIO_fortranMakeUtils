module module1

contains 
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


recursive subroutine mainused1(x,r)
    call mainused1(a,s)
    call interprocUsed()
end subroutine mainused1


subroutine mainnotused1()
    ignorethis
end subroutine mainnotused1


FUNCTION funcMainUsed1(s,g)
    ignorethis
end FUNCTION funcMainUsed1


function funcMainNotUsed1(s,g)
    ignorethis
end function funcMainNotUsed1


subroutine interprocUsed_1(d,t)
    ignorethis
end subroutune INTERprocUsed_1

subroutine interprocUsed_2(d,t)
    ignorethis
end subroutune interprocUsed_2

subroutine interprocNotUsed_1(d,t)
    ignorethis
end subroutune interprocNotUsed_1

subroutine interprocNotUsed_2(d,t)
    ignorethis
end subroutune interprocNotUsed_2

subroutine interprocUsed2_1(d,t)
    ignorethis
end subroutune interprocUsed2_1

subroutine interprocUsed2_2(d,t)
    ignorethis
end subroutune interprocUsed2_2

subroutine interprocUsed2_3(d,t)
    ignorethis
end subroutune interprocUsed2_3

end module1