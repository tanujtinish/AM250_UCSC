program ring

use mpi
implicit none

integer numprocs, ierr, request, status(mpi_status_size)  

integer pid, target_pid, source_pid, left_or_right

integer message

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

if (pid .eq. 0) then

    print *, "Should I pass Left or Right? Enter '0' for Left or '1' for right: "
    read(*,*) left_or_right

end if

call mpi_barrier(mpi_comm_world, ierr)
call mpi_bcast(left_or_right, 1, mpi_int, 0, mpi_comm_world, ierr)

if (left_or_right .eq. 1) then
    target_pid = MOD(pid + 1, numprocs)
    source_pid = MOD(pid - 1+ numprocs, numprocs)
else
    target_pid = MOD(pid + 1, numprocs)
    source_pid = MOD(pid - 1+ numprocs, numprocs)
end if

message=pid
call mpi_isend(message, 27, MPI_CHAR, target_pid, 222, MPI_COMM_WORLD, request, ierr)

call mpi_recv(message, 27, MPI_CHAR, source_pid, 222, MPI_COMM_WORLD, status, ierr)
print *, "I, Processor ", pid, " recieved message: ", message," from processor ",source_pid

call MPI_FINALIZE(ierr)

stop
end program ring