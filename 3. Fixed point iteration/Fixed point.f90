! Using Fixed point iteration method approximate the root of f(x)=cos(x)-x
program Fixed_point_iteration
    implicit none

    real::tol,a0,a
    integer::i,max_iter

    write (*, *) "Enter initial a0, tolerance, max iteration: "
    read (*, *) a0, tol, max_iter

    write(*,*)
    write(*,"(a10,3a20)")"Iteration",'a0','a',"Tolerance"
    write(*,*)repeat("-",70)

    do i=1,max_iter
        a=f(a0)
        write(*,"(i10,3f20.8)")i,a0,a,abs(a-a0)
        if(abs(a-a0)<tol) exit
        a0=a
    end do


    write (*, *) repeat("-", 70)
    write (*, *) "Approximate root at x =", a


    contains
        real function f(x)
            real,intent(in)::x

            f=cos(x)

        end function f

end program Fixed_point_iteration
