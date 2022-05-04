program hello_world

use mpi
implicit none

integer ierr, pid, numprocs

call MPI_INIT(ierr)

call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

print *, "Hello! (From processor #", pid, " out of ", numprocs, ")"

call MPI_FINALIZE(ierr)
stop

end program hello_world