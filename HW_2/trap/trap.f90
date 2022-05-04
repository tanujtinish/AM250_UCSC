REAL FUNCTION integral(a, b, N)

    IMPLICIT NONE
    INTEGER :: i, N
    REAL :: delta
    REAL :: integral, a, b
    REAL :: x1, x2

    delta = (b - a) / N

    do i = 1, N, 1
        x1= a + delta * ( i-1 )
        x2= a + delta * i
        integral = integral + ( delta / 2 ) * ( x1**2 + x2**2 )
        ! integral = integral + ( delta / 2 ) * ( SIN(x1) + SIN(x2) )
    end do

END FUNCTION integral


program trap
    
    implicit none
    real, external :: integral
    integer :: i, multiply=1, N
    REAL :: a, b, sum, swap, pi
    
    pi = acos( -1.0 )
    ! write (*,*) "The upper limit of the integration is: "
    ! write(*,*) pi
    ! b=pi
    
    write (*,*) "Please input the upper limit of the integration: "
    read (*,*) b
    write (*,*) "Please input the lower limit of the integration: "
    read (*,*) a
    
    write (*,*) "Please input the number of intervals N for the integration: "
    read (*,*) N
    
    write (*,*) NEW_LINE('!')
    write (*,*) "Given inputs values are: "
    write (*,*) "a-",a 
    write (*,*) "b-",b
    write (*,*) "N-",N

    if(b < a) then
        swap=b
        b=a
        a=swap
        multiply=-1
    end if

    sum = integral(a, b, N)
    sum = sum * multiply
    write (*,*) NEW_LINE('!')
    write (*,*) "Integral is:",sum
    write (*,*) NEW_LINE('!')

end program trap
    