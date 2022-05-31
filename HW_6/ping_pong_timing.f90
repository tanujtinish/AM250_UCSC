program ping_pong_timing

use mpi
implicit none

integer ierr, pid, numprocs, status(mpi_status_size), data_length, i, iter, num_of_iter
integer ping_node, pong_node

integer ping_send, pong_send, ping_recv, pong_recv, temp
integer pong_status(mpi_status_size), ping_status(mpi_status_size)

real(kind=8) time_average, total_time
real(kind=8) start, end, startup, endup

character, dimension(:), allocatable :: message

call cpu_time(startup)
call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)


ping_node = 0
pong_node = 1
total_time=0

num_of_iter = 1000
iter = 1

if (pid .eq. ping_node) then

    print *, "Please input data length: "
    read(*,*) data_length

end if
call mpi_barrier(mpi_comm_world, ierr)
call mpi_bcast(data_length, 1, mpi_int, 0, mpi_comm_world, ierr)
allocate(message(data_length))

if (data_length.gt.0) then

    do while (iter .le. num_of_iter)
        if (pid .eq. ping_node) then
            
            call cpu_time(start)

            i = 1
            do while (i .le. data_length)
                message(i) = "a"
                i = i + 1
            end do
            call mpi_isend(message, data_length, MPI_CHAR, pong_node, 222, MPI_COMM_WORLD, ping_send, ierr)
            call mpi_wait(ping_send, ping_status, ierr)
            ! print *, "Iteration ",iter,": I Ping just sent a message to pong!"

            ! call mpi_recv(end, 1, mpi_double, pong_node, 222, mpi_comm_world, temp, ierr)

            call mpi_irecv(message, data_length, MPI_CHAR, pong_node, 222, MPI_COMM_WORLD, status, ping_recv, ierr)
            call mpi_wait(ping_recv, ping_status, ierr)
            ! print *, "Iteration ",iter,": I Ping just got a message: '", message(1:data_length), "'"
                    
            ! total_time = total_time + end - start

            iter = iter + 1
        end if

        if (pid .eq. pong_node) then

            call mpi_irecv(message, data_length, MPI_CHAR, ping_node, 222, MPI_COMM_WORLD, status, pong_recv, ierr)
            call mpi_wait(pong_recv, pong_status, ierr)
            ! print *, "Iteration ",iter,": I Pong just got a message: '", message(1:data_length), "'"
            
            ! call cpu_time(end)
            ! call mpi_send(end, 1, mpi_double, ping_node, 222, mpi_comm_world, ierr)

            i = 1
            do while (i .le. data_length)
                message(i) = "b"
                i = i + 1
            end do
            call mpi_isend(message, data_length, MPI_CHAR, ping_node, 222, MPI_COMM_WORLD, pong_send, ierr)
            call mpi_wait(pong_send, pong_status, ierr)
            ! print *, "Iteration ",iter,": I Pong just sent a message to ping!"
            
            iter = iter + 1
        end if

    end do

    if (pid .eq. ping_node) then
        ! time_average = (total_time/num_of_iter) * 1.0_4
        call cpu_time(endup)
        ! time_average = (total_time/num_of_iter) * 1.0_4
        print *, "Message_length(L) is: ", data_length
        print *, "Total_Time_taken(Tw*L + Ts) is: ", endup - startup    
    end if
else
    if (pid .eq. ping_node) then
        call cpu_time(endup)
        print *, "Message_length(L) is: ", data_length
        print *, "Total_Time_taken(Tw*L + Ts) is: ", endup - startup    
    end if
end if

call MPI_FINALIZE(ierr)

stop
end program ping_pong_timing