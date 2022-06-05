module utility_func

    contains

    subroutine generate_input_square_array(inputSquareArray, N)
        
        implicit none
        integer :: i, j, N
        integer, ALLOCATABLE :: inputSquareArray(:,:)
        REAL :: ran_num

        allocate(inputSquareArray(N,N))
        do j = 1, N
            do i = 1, N
                ran_num = irand() / irand()
                if (ran_num .GE. 1) then
                    inputSquareArray(i, j) = 1
                else
                    inputSquareArray(i, j) = 0
                end if
            end do
        end do

    end subroutine generate_input_square_array

    subroutine generate_input_padded_square_array(inputSquareArray, paddedInputSquareArray, N)
        
        implicit none
        integer :: i, j, N
        integer :: inputSquareArray(:,:)
        integer, ALLOCATABLE :: paddedInputSquareArray(:,:)
        
        allocate( paddedInputSquareArray(0:N+1, 0:N+1) )

        do j = 1, N
            do i = 1, N
                paddedInputSquareArray(i, j)= inputSquareArray(i, j)
            end do
        end do

        do j = 1, N
            paddedInputSquareArray(N+1, j)= inputSquareArray(1, j)
            paddedInputSquareArray(0, j)= inputSquareArray(N, j)
        end do
        
        do i = 1, N
            paddedInputSquareArray(i, 0)= inputSquareArray(i, N)
            paddedInputSquareArray(i, N+1)= inputSquareArray(i, 1)
        end do

        paddedInputSquareArray(0,0)= inputSquareArray(N,N)
        paddedInputSquareArray(N+1,N+1)= inputSquareArray(1,1)
        paddedInputSquareArray(0,N+1)= inputSquareArray(N,1)
        paddedInputSquareArray(N+1,0)= inputSquareArray(1,N)

    end subroutine generate_input_padded_square_array

    subroutine print_square_array(arr)

        implicit none
        integer :: arr(:,:)
        integer :: i, N
        
        N= size(arr, 1)
        do i = 1, N
            WRITE (*,*) arr(i, :)
        end do

    end subroutine print_square_array

    subroutine generate_output_square_array(paddedInputSquareArray, outputSquareArray, N)

        implicit none
        integer :: paddedInputSquareArray(:,:)
        integer, ALLOCATABLE:: outputSquareArray(:,:)
        integer :: i, j, i2, j2, N, count

        allocate(outputSquareArray(N,N))

        do j2 = 1, N
            do i2 = 1, N
                count = 0 
                
                i=i2+1
                j=j2+1

                if(paddedInputSquareArray(i+1,j+1)==1) then
                    count=count+1
                end if

                if(paddedInputSquareArray(i+1,j)==1) then
                    count=count+1
                end if

                if(paddedInputSquareArray(i,j+1)==1) then
                    count=count+1
                end if

                if(paddedInputSquareArray(i-1,j-1)==1) then
                    count=count+1
                end if

                if(paddedInputSquareArray(i-1,j)==1) then
                    count=count+1
                end if

                if(paddedInputSquareArray(i,j-1)==1) then
                    count=count+1
                end if

                if(paddedInputSquareArray(i+1,j-1)==1) then
                    count=count+1
                end if

                if(paddedInputSquareArray(i-1,j+1)==1) then
                    count=count+1
                end if

                if(count==3) then
                    outputSquareArray(i2,j2)=1
                else
                    outputSquareArray(i2,j2)=0
                end if
            end do       
        end do

    end subroutine generate_output_square_array

end module utility_func

program ones
    use utility_func
    use, intrinsic :: ISO_C_BINDING
    implicit none
    
    real(kind=8), dimension(6) :: timming_array
    integer :: timming_array_index
    
    real(kind=8) :: total_time1,total_time2,total_time3,total_time, sqrt_p
    real(kind=8) :: ts,tw,tc, N,p

    tc=0.000000131284970928
    ts=0.007997999
    tw=0.0005823312
    p=4
    sqrt_p= sqrt(real(p,8))

    write(*, *) ts,tw,tc

    timming_array = (/ 20, 100, 500, 1000, 3000, 6000/)
    
    timming_array_index = 1
    do while (timming_array_index .le. 6)
        total_time=0
        
        N=timming_array(timming_array_index)

        total_time = real(tc*((N*N)/(P))) 
        total_time = total_time + real(4*ts)
        total_time = total_time + real(4*tw)*real( (N/sqrt_p) +1 )

        timming_array_index=timming_array_index+1

        write(*, *) 'For N: ',N
        write(*, *) 'Total time taken: ',total_time
        write (*,*) "------------------------------------------------------------------"

    end do
    
    

end program ones