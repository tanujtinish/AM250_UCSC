program matrix_multiplication
    implicit none
    ! Specify the number of threads that are to be used during execution
    integer:: num_threads = 8
    ! Specify the matrix dimensions
    ! mxn are the dimensions of Matrix A
    ! nxo are the dimensions of Matrix B
    integer:: m,n,o
    ! i,j & k are used for matrix indexing
    integer:: i,j, k
    ! Seed is used to generate random values to populate the matrices
    integer:: seed
    ! base is used to specify the upperlimit of the values of the random 
    ! variables generated. 
    integer, parameter:: base  = 1000
    ! Constants to initialise the seed value for generating random values
    integer:: seed_const_A = 789643
    integer:: seed_const_B = 578968
    ! Gives the system time at that instant
    real*8 :: omp_get_wtime
    ! variable that stores the system time 
    real*8 ::  seconds
    ! Declare a matrix to store the values of Matrix A        
    integer , allocatable, dimension ( :, : ) :: MAT_A
    ! Declare a matrix to store the values of Matrix B  
    integer , allocatable, dimension ( :, : ) :: MAT_B
    ! Declare a matrix to store the values of the 
    ! product obtained by multiplying Matrices A & B  
    ! using do loops 
    integer , allocatable, dimension ( :, : ) :: MAT_C
    ! Declare a matrix to store the values of the 
    ! product obtained by multiplying Matrices A & B  
    ! using MatMul call 
    integer , allocatable, dimension ( :, : ) :: MAT_D

    ! Read the values of the dimensions of the matrices
    print *,"Enter the value of m (Number of Rows in A): " 
    read *, m
    print *, "Enter the value of n (Number of Rows in B and Columns in A): "
    read *, n
    print * , "Enter the value of o (Number of Columns in B): "
    read *, o

    ! Allocate the dimensions of the mastrices accordingly
    allocate ( MAT_A(1:m,1:n) )
    allocate ( MAT_B(1:n,1:o) )
    allocate ( MAT_C(1:m,1:o) )
    allocate ( MAT_D(1:m,1:o) )

    ! Initializing the seed value to generate random values
    seed = seed_const_A + time()
    call srand(seed)
    ! Generating the Matrix A
    print*, "Generating Matrix A...."
    print*, " "
    ! Iterating through Rows
    do i = 1, m
        ! Iterting through Columns
        do j = 1, n
            ! Assigning random values at each index
            MAT_A(i,j) = Floor(rand()*base)
            ! Printing the values of the Matrix at each index
            write(*, fmt="(1x,a,i0)", advance="no") " ", MAT_A(i,j)
        end do
        print *, " "
    end do

    ! Initializing the seed value to generate random values
    seed = seed_const_B + time()
    call srand(seed)
    ! Generating the Matrix B
    print*, "Generating Matrix B...."
    print*, " "
    ! Iterating through Rows
    do i = 1, n
        ! Iterting through Columns
        do j = 1, o
            ! Assigning random values at each index
            MAT_B(i,j) = Floor(rand()*base)
            ! Printing the values of the Matrix at each index
            write(*, fmt="(1x,a,i0)", advance="no") " ", MAT_B(i,j)
        end do
        print *, " "
    end do

    ! We not proceed to perform the matrix multiplication 
    ! using do loops to do and OpenMP PARALLEL DO 
    print *, 'Matrix Multiplication using do loops and OpenMP PARALLEL DO...' 
    call calc_mult_parallal ( MAT_A, MAT_B, MAT_C, m, n, o, seconds, omp_get_wtime)
    print *, " "
       print *, " Matrix C after Multiplication is: "
       ! Iterating through Rows
       do i = 1, m
            ! Iterting through Columns
            do j = 1, o
                ! Printing the values of the Matrix at each index
                write(*, fmt="(1x,a,i0)", advance="no") " ", MAT_C(i,j)
            end do
            print *, " "
       end do

    ! We not proceed to perform the matrix multiplication 
    ! using Fortran MATMUL and OpenMP workshare 
    print *, 'Matrix Multiplication using Fortran MATMUL and OpenMP workshare...' 
    call calc_matmul_workshare ( MAT_A, MAT_B, MAT_D, m, n, o, seconds, omp_get_wtime)
    print *, " "
    print *, " Matrix D after Multiplication is: "
    ! Iterating through Rows
    do i = 1, m
         ! Iterting through Columns
         do j = 1, o
             ! Printing the values of the Matrix at each index
             write(*, fmt="(1x,a,i0)", advance="no") " ", MAT_D(i,j)
         end do
         print *, " "
    end do

    ! Deallocate the memory assigned to Matrix A
    deallocate(MAT_A)
    ! Deallocate the memory assigned to Matrix B
    deallocate(MAT_B)
    ! Deallocate the memory assigned to Matrix C
    deallocate(MAT_C)
    ! Deallocate the memory assigned to Matrix D
    deallocate(MAT_D)

end program matrix_multiplication

subroutine calc_mult_parallal ( MAT_A, MAT_B, MAT_C, m, n, o, seconds, omp_get_wtime )
    implicit none
    ! Specify the matrix dimensions
    ! mxn are the dimensions of Matrix A
    ! nxo are the dimensions of Matrix B
    integer:: m,n,o
    ! i,j & k are used for matrix indexing
    integer:: i,j, k
    ! Gives the system time at that instant
    real*8 :: omp_get_wtime
    ! variable that stores the system time 
    real*8 ::  seconds
    ! Declare a matrix to store the values of Matrix A 
    integer, dimension (1:m, 1:n)::MAT_A
    ! Declare a matrix to store the values of Matrix A
    integer, dimension (1:n, 1:o)::MAT_B
    ! Declare a matrix to store the values of the 
    ! product obtained by multiplying Matrices A & B  
    ! using do loops
    integer, dimension (1:m, 1:o)::MAT_C
    print *,'hey'
    seconds = omp_get_wtime( )
    print *,'hey'
    !$omp parallel &
    !$omp shared ( m, n,o, MAT_A, MAT_B, MAT_C ) &
    !$omp private ( i, j, k )
    !$omp do
    ! Store the starting time
    ! Iterate through the rows of A
    do i=1,m
        ! Iterate through the columns of B
        do j=1,o
            ! Initialize the value at the given index 
            ! of Matrix C with Zero.
            MAT_C(i,j) = 0
                ! Calcluate the value at the given 
                ! index of Matrix C.
                do k=1,n
                    MAT_C(i,j) = MAT_C(i,j) + MAT_A(i,k)* MAT_B(k,j)
                end do
        end do
    end do
    !$omp end do
    !$omp end parallel
    ! Calculate the end time
    seconds = omp_get_wtime( ) - seconds
    print *, 'Matrix multiplication using do loops and OpenMP PARALLEL DO  took time of ', seconds, "seconds"
    
    return
end

subroutine calc_matmul_workshare ( MAT_A, MAT_B, MAT_D, m, n, o, seconds, omp_get_wtime )
    implicit none
    ! Specify the matrix dimensions
    ! mxn are the dimensions of Matrix A
    ! nxo are the dimensions of Matrix B
    integer:: m,n,o
    ! i,j & k are used for matrix indexing
    integer:: i,j, k
    ! Gives the system time at that instant
    real*8 :: omp_get_wtime
    ! variable that stores the system time 
    real*8 ::  seconds
    ! Declare a matrix to store the values of Matrix A 
    integer, dimension (1:m, 1:n)::MAT_A
    ! Declare a matrix to store the values of Matrix A
    integer, dimension (1:n, 1:o)::MAT_B
    ! Declare a matrix to store the values of the 
    ! product obtained by multiplying Matrices A & B  
    ! using MATMUL workshare
    integer, dimension (1:m, 1:o)::MAT_D
    seconds = omp_get_wtime( )
    !$omp parallel 
    !$omp workshare
    ! Store the starting time
    MAT_D = matmul (MAT_A,MAT_B)
    !$omp end workshare
    !$omp end parallel
    ! Calculate the end time
    seconds = omp_get_wtime( ) - seconds
    print *, 'Matrix multiplication using Fortran MATMUL and OpenMP workshare took time of ', seconds, 'seconds'

    return
end