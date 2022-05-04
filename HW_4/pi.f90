program pi

use mpi
implicit none

integer pid, numprocs, ierr, status(mpi_status_size)

integer i, iterations, seed, sign
integer hits_per_processor, reduced_total_hits
real x_coord, y_coord, radius, point_distance, pi_value

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

iterations = 5000000
hits_per_processor = 0
radius = 1.0

seed = pid
call srand(seed)

do i = 1, iterations

    if (rand() .gt. 0.5) then
        sign = 1.0
    else 
        sign = -1.0
    end if
    x_coord = sign * rand()

    if (rand() .gt. 0.5) then
        sign = 1.0
    else 
        sign = -1.0
    end if
    y_coord = radius * sign * rand()

    point_distance = sqrt(x_coord ** 2 + y_coord ** 2)
    if (point_distance .le. radius) then
        hits_per_processor = hits_per_processor + 1
    end if

end do

print *, "Processor ", pid, " got ", hits_per_processor, " hits in the circle!"
call MPI_REDUCE(hits_per_processor, reduced_total_hits, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

if (pid .eq. 0) then
    pi_value = 4.0 * (reduced_total_hits / real(numprocs * iterations))
    print *, "Approximation of Pi: ", pi_value
end if

call MPI_FINALIZE(ierr)

stop
end program pi