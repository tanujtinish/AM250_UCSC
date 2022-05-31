PROGRAM matrix_op

    implicit none
    DOUBLE PRECISION t1, t2, omp_get_wtime 
    INTEGER I, J, K, R1, C1R2, C2, MIN_I, MIN_J, MIN_ELEM
    INTEGER, ALLOCATABLE, target :: A(:,:)
    INTEGER, ALLOCATABLE :: B(:,:)
    INTEGER, ALLOCATABLE :: C(:,:)
    REAL ran_num, random_number

    write (*,*) "Please input the rows in matrix A: "
    read (*,*) R1
    write (*,*) "Please input the (columns in matrix A=rows in matrix B): "
    read (*,*) C1R2
    write (*,*) "Please input the columns in matrix B: "
    read (*,*) C2

    write (*,*) "------------------------------------------------------------------"
    print *,'Dimentions of matriz A are: (',R1,',',C1R2,')'
    print *,'Dimentions of matriz B are: (',C1R2,',',C2,')'
    write (*,*) "------------------------------------------------------------------"

    allocate(A(R1,C1R2))
    allocate(B(C1R2,C2))
    allocate(C(R1,C2))

    !Initialize matrix A
    DO I=1, R1
        DO J=1, C1R2
            ran_num = rand()
            A(J, I) = 1+FLOOR(100*ran_num)
        ENDDO
    ENDDO

    !Initialize matrix B
    DO I=1, C1R2
        DO J=1, C2
            ran_num = rand()
            B(J, I) = 1+FLOOR(100*ran_num)
        ENDDO
    ENDDO

    t1 = omp_get_wtime()
    !$omp parallel shared(A, B, C) private(I, J, K)
    !$omp do schedule(static)
    do I=1,R1
        do J=1,C2
            do K=1,C1R2
                C(I,J)=C(I,J)+A(I,K)*B(K,J)
            ENDDO
        ENDDO
    ENDDO
    !$omp end do
    !$omp end parallel
    t2 = omp_get_wtime()
    write (*,*) "------------------------------------------------------------------"
    print *,'Time taken by OpenMP PARALLEL DO for matrix Mult is: ', t2 - t1, ' s'
    write (*,*) "------------------------------------------------------------------"

    t1 = omp_get_wtime()
    !$OMP PARALLEL WORKSHARE
    C = matmul(A, B)
    !$OMP END PARALLEL WORKSHARE
    t2 = omp_get_wtime()
    write (*,*) "------------------------------------------------------------------"
    print *,'Time taken by OpenMP workshare for matrix Mult is: ', t2 - t1, ' s'
    write (*,*) "------------------------------------------------------------------"

    MIN_ELEM=2147483647
    do I=1,R1
        do J=1,C2
            IF(C(I,J) <= MIN_ELEM) THEN
                MIN_ELEM=C(I,J)
                MIN_I=I
                MIN_J=J
            END IF
        ENDDO
    ENDDO
    write (*,*) "------------------------------------------------------------------"
    print *,'MIN_ELEMENT IS: ', MIN_ELEM, ' at position ','(',MIN_I,',',MIN_I,')'
    write (*,*) "------------------------------------------------------------------"

stop
END PROGRAM matrix_op