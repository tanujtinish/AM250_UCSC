

program game_of_life

    use utility_func
    use mpi
    use, intrinsic :: ISO_C_BINDING

    implicit none

    integer :: request, rstatus(mpi_status_size), ierr, tag
    integer :: pid, mpi_nodes

    integer :: grid_size, num_procs, num_of_iterations, config
    integer, dimension(2) :: num_proc_dim
    
    integer, ALLOCATABLE, target :: processor_properties(:,:)
    type(t_pair) :: processor_local_properties

    integer :: i, j
    INTEGER, ALLOCATABLE, target :: global_grid(:,:)
    INTEGER, ALLOCATABLE, target :: local_grid(:,:)

    real(kind=8) :: start, end

    integer :: timming_array_index
    integer, dimension(6) :: timming_array
    timming_array = (/  20, 100, 500, 1000, 3000, 6000/)

    call mpi_init(ierr)
    call mpi_comm_rank(mpi_comm_world, pid, ierr)
    call mpi_comm_size(mpi_comm_world, mpi_nodes, ierr)


    do timming_array_index=1,6

        grid_size=timming_array(timming_array_index)
        num_procs=4
        num_of_iterations=80
        config=1
        allocate(processor_properties(4, num_procs))


        if(mpi_nodes .lt. num_procs+1) then
            write(*,*) "Number of nodes used in mpi cant be less than the parallel processes/tasks"
            call mpi_finalize(ierr)
            stop
        end if

        if((num_procs .gt. grid_size*grid_size)) then
            num_procs = num_procs -  grid_size*grid_size
        end if

        call calculate_processors_per_row_column(num_procs, num_proc_dim)


        if(pid .ne. 0  .AND.  pid .le. num_procs) then

            call calculate_local_grid_position(pid, num_proc_dim, processor_local_properties)
            call calculate_local_grid_size(pid, grid_size, num_proc_dim, processor_local_properties)
            call calculate_local_grid_start_position(pid, processor_local_properties)
            call calculate_neighbour_nodes(pid, num_proc_dim, processor_local_properties)

            call send_properties_to_pid0(processor_local_properties)

        else if(pid .eq. 0) then
            call recieve_properties_to_pid0(num_procs ,processor_properties)
        end if


        call mpi_barrier(mpi_comm_world, ierr)


        if(pid .eq. 0) then
            call initialize_grid(global_grid, grid_size, config)

            ! write (*,*) "Grid initially"
            ! call print_grid(global_grid)
            ! write (*,*) "------------------------------------------------------------------"
            
            call send_initial_grid_to_nodes(global_grid, processor_properties)

        else if(pid .ne. 0  .AND.  pid .le. num_procs) then

            allocate(local_grid(processor_local_properties%local_grid_size(1)+2, processor_local_properties%local_grid_size(2)+2))
            call recieve_initial_grid_to_nodes(local_grid)
        end if


        call mpi_barrier(mpi_comm_world, ierr)

        if(pid .eq. 0) then
            call cpu_time(start)
        end if

        do i=1,num_of_iterations
            if(pid .ne. 0  .AND.  pid .le. num_procs) then
                call communicate_columns_to_from_neigbours(local_grid, processor_local_properties)
                call communicate_rows_to_from_neigbours(local_grid, processor_local_properties)
                call communicate_diagnols_to_from_neigbours(local_grid, processor_local_properties)
                
                call update_local_grid_game_of_life(local_grid, processor_local_properties)

                call send_updated_grid_to_pid0(local_grid)

            else if(pid .eq. 0) then
                call recieve_updated_grid_to_pid0(global_grid, processor_properties)
                
                ! if(mod(i,20) .eq. 0) then
                !     write (*,*) "Grid After iteration: ",i
                !     call print_grid(global_grid)
                !     write (*,*) "------------------------------------------------------------------"
                ! end if

            end if
            if(pid .eq. 0) then
                call cpu_time(end)
            end if

            call mpi_barrier(mpi_comm_world, ierr)


        end do

        if(pid .eq. 0) then
            write(*, *) 'For N: ',grid_size
            write(*, *) 'For P: ',num_procs
            write(*, *) 'Total time taken: ',end-start
            write(*, *) "------------------------------------------------------------------"
        end if

        if(pid .ne. 0  .AND.  pid .le. num_procs) then
            deallocate(local_grid)
        else if(pid .eq. 0) then
            deallocate(global_grid)
        end if
        deallocate(processor_properties)

    end do

    call mpi_finalize(ierr)
    stop
end program game_of_life