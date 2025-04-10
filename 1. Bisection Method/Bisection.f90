program Bisection_method
   implicit none

   real::a,b,c,fa,fb,fc,tol
   integer::i,max_iter

   write(*,*)'Enter a,b,tolerance,max iteration: '
   read(*,*)a,b,tol,max_iter
   write(*,*)

   fa=f(a)
   fb=f(b)
   if(fa*fb>0.0)stop 'Root do not exist.'

   write(*,"(a10,5a20)")"Iteration","a","b",'c','f(c)','Tolerance'
   write(*,*)repeat("-",110)

   do i=1,max_iter
    c=a+(b-a)/2.0
    fc=f(c)

    write(*,"(i10,5f20.8)")i,a,b,c,fc,abs(b-a)/2

    if(fc==0.or.abs(b-a)/2.0<tol) exit
    if(fa*fc>0) then
        a=c
        fa=fc
    else
        b=c

    end if

   end do
   write(*,*)
   write(*,*)"The root is: ",c

    contains
        real function f(x)
            real,intent(in)::x

            f=cos(x)-x
        end function


end program Bisection_method
