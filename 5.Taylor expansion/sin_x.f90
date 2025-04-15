program Taylor_polynomial_sinx

    implicit none
    real::x,sum,k
    integer::n,i,count=0,sign=1

    write (*, *) "Enter x, Degree n:"
    read (*, * ) x, n

    sum = 0.0

    do i=1,100

        if ((mod(i,2)/=0))then
            if(mod(sign,2)==0)then
               k=((-1)**i)*x**i

            else
                k=((-1)**(i+1))*x**i
            end if

            sum=sum+(k/factorial(i))
            count=count+1
            if(count==n)exit
            sign=sign+1

        end if

    end do

    write (*, *) "Taylor polynomial Sin(x) at x =", x, 'is', sum

   contains
        integer function factorial(n)
        integer,intent(in)::n
        integer::i
        factorial=1
        do i=1,n
            factorial=factorial*i
        end do
    end function factorial

end program Taylor_polynomial_sinx
