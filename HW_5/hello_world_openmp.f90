PROGRAM hello_world

    implicit none
    INTEGER NTHREADS, TID, OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS 

    !$OMP PARALLEL PRIVATE(NTHREADS, TID)
    TID= OMP_GET_THREAD_NUM()
    PRINT *, 'Hello World from thread = ', TID

    if(TID .EQ. 0) THEN
        NTHREADS = OMP_GET_NUM_THREADS()
        PRINT *, 'Number of threads = ', NTHREADS
    END if

    !$OMP END PARALLEL

stop
END PROGRAM hello_world
