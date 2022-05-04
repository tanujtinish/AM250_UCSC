program ping_pong

use mpi
implicit none

integer ierr, pid, numprocs, status(mpi_status_size)

integer ping_node, pong_node

character*25 message

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

ping_node = 0
pong_node = 1

if (pid .eq. ping_node) then
    
    message="This is message from Ping"
    call mpi_send(message, 25, MPI_CHAR, pong_node, 222, MPI_COMM_WORLD, ierr)
    print *, "I Ping just sent a message to pong!"
    
    call mpi_recv(message, 25, MPI_CHAR, pong_node, 222, MPI_COMM_WORLD, status, ierr)
    print *, "I Ping just got a message: '", message(1:25), "'"
end if

if (pid .eq. pong_node) then

    call mpi_recv(message, 25, MPI_CHAR, ping_node, 222, MPI_COMM_WORLD, status, ierr)
    print *, "I Pong just got a message: '", message(1:25), "'"


    message="This is message from Pong"
    call mpi_send(message, 25, MPI_CHAR, ping_node, 222, MPI_COMM_WORLD, ierr)
    print *, "I Pong just sent a message to ping!"


end if

call MPI_FINALIZE(ierr)

stop
end program ping_pong