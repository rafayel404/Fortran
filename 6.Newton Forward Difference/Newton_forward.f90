program Newton_forward_difference

    implicit none

    integer::n,i,j
    real::p,h,u,term,result,x(10),y(10),table(10,10)

    write(*,*)"Enter the number of data points : "
    read(*,*)n
    write(*,*)"Enter the data points x,y : "
    read(*,*) (x(i),y(i), i=1,n)

    do i=1,n
        table(i,1)=y(i)
    end do

    do j=2,n
        do i=1,n-j+1
            table(i,j)=table(i+1,j-1)-table(i,j-1)
        end do
    end do

    write(*,*)'The forward difference table is : '
    write(*,*) repeat("-",10*n)

    do i=1,n
        do j=1,n-i+1
            write(*,"(f10.4)",advance='no') table(i,j)
        end do
            write(*, *)
    end do

    write(*,*) repeat("-",10*n)

    write(*, *) "Enter the interpolation point:"
    read (*, *) p

    h=x(2)-x(1)
    u=(p-x(1))/h
    term=1.0
    result=table(1,1)

    do i =2,n
        term=term*(u-(i-2))
        result=result+(term*table(1,i))/factorial(i-1)
    end do

    write (*, *) "Interpolated value at x =", p, "is", result


    contains
        real function factorial(n)
            integer,intent(in)::n
            integer::i

            factorial=1
            do i=1,n
                factorial=factorial*i
            end do
        end function factorial

end program Newton_forward_difference
