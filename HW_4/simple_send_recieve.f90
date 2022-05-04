program simple_send_recieve
    
use mpi
use, intrinsic :: ISO_C_BINDING
implicit none

integer ierr, pid, numprocs, status(mpi_status_size)

integer sid, rid, i

integer, dimension(10), target :: array= (/1,1,1,1,1,1,1,1,1,1/)

sid = 1
rid = 2

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

if (pid .eq. sid) then
    print *, "Processor", sid, " is sending data to processor ", rid
    write (*,*) NEW_LINE('!')
    array= (/1,2,3,4,5,6,7,8,9,10/)
    call mpi_send(array, 10, MPI_INT, rid, 222, MPI_COMM_WORLD, ierr)
end if

if (pid .eq. rid) then
    print *, "Processor", rid, " is recieving data from processor ", sid
    write (*,*) NEW_LINE('!')

    print *, "Processor", rid
    print *, "Currently, my array is:"
    print *, array(:)
    write (*,*) NEW_LINE('!')

    call mpi_recv(array, 10, MPI_INT, sid, 222, MPI_COMM_WORLD, status, ierr)

    print *, "Processor", rid
    print *, "After recieving data, my array is:"
    print *, array(:)
    write (*,*) NEW_LINE('!')
end if

call MPI_FINALIZE(ierr)

stop
end program simple_send_recieve