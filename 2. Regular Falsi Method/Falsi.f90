! Using False position method approximate the root of f(x)=cos(x)-x
program False_position
    implicit none

    real::a,b,c,tol,fa,fb,fc
    integer::i,max_iter

    write(*,*)"Enter a,b,tolerance,max iteration: "
    read(*,*)a,b,tol,max_iter
    write(*,*)

    fa=f(a)
    fb=f(b)
    if(fa*fb>0)stop 'Root do not exist'

    write(*,*)
    write(*,"(a10,5a20)")'Iteration','a','b','c','f(c)','tolerance'
    write(*,*)repeat('-',110)

    do i=1,max_iter
        c=(a*fb-b*fa)/(fb-fa)
        fc=f(c)
        write (*, "(i10,5f20.8)") i, a, b, c, fc, abs(c-b)
        if(fc==0.or.abs(c-b)<tol) exit
        if(fa*fc>0) then
            a=c
            fa=fc
        else
            b=c
            fb=fc
        end if

    end do
    write (*, *) repeat("-", 110)

    write(*,*)'Approximate ans is: ',c

    contains
        real function f(x)
            real, intent(in)::x
            f = cos(x) - x
    end function f
end program False_position
