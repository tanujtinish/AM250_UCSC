module utility_func

    use mpi
    use, intrinsic :: ISO_C_BINDING
    
    type :: t_pair
        sequence
        integer, dimension(2) :: sizes, local_grid_size, local_grid_start_position, local_grid_position
        integer, dimension(2) :: grid_size_1st_row_col
        integer, dimension(8) :: neighbour_node
    end type

    contains

    subroutine print_grid(grid)

        ! prints 2d grid. Visualization function for game of life

        implicit none
        integer :: grid(:,:)
        integer :: i, j, rows, cols
        
        rows= size(grid, 1)
        cols= size(grid, 2)
        do i = 1, rows
            do j = 1, cols
                if(grid(i, j) .eq. 1) then
                    write(*, fmt="(1x,a,i0)", advance="no") "1"
                else
                    write(*, fmt="(1x,a,i0)", advance="no") "_"
                end if
            end do
            write(*,*)
        end do
        write(*,*)

    end subroutine print_grid
    
    
    subroutine initialize_grid(grid, N, config)
        
        ! Creates initial 2D matric for game of life. Called in node of rank 0

        implicit none
        integer :: i, j, N, config
        integer, ALLOCATABLE :: grid(:,:)
        REAL :: ran_num

        allocate(grid(N,N))

        if(config .eq. 1) then
            do j = 1, N
                do i = 1, N
                    grid(i, j) = 0
                end do
            end do

            grid(1, 3) = 1
            grid(2, 3) = 1
            grid(3, 3) = 1
            grid(3, 2) = 1
            grid(2, 1) = 1

        else
            do j = 1, N
                do i = 1, N
                    ran_num = irand() / irand()
                    if (ran_num .GE. 1) then
                        grid(i, j) = 1
                    else
                        grid(i, j) = 0
                    end if
                end do
            end do
        end if

    end subroutine initialize_grid


    subroutine calculate_processors_per_row_column(num_procs, num_proc_dim)

        ! Calculates the number of processors assigned to a single row and a single column of 2D grid
        ! Basically factorize num_procs into x and y such that num_procs=x*y and |x-y| is minimum for best load balancing

        implicit none
        integer :: grid_size, num_procs
        integer, dimension(2) :: num_proc_dim

        integer :: i
        integer :: square_root_num_procs

        square_root_num_procs=sqrt(real(num_procs,8))

        loop: do i=square_root_num_procs,1,-1
            if(mod(num_procs,i) .eq. 0) then
                num_proc_dim(1) = i
                num_proc_dim(2) = num_procs/i
                exit loop
            end if
        end do loop

    end subroutine calculate_processors_per_row_column


    subroutine calculate_local_grid_position(pid, num_proc_dim, processor_local_properties)
        
        ! Function calculates relative position of processor on the grid in terms of row/col. Like processor 1 will have position 1,1. Processor P will have position sqrt(P),sqrt(P) 
        
        implicit none
        integer :: pid 
        integer, dimension(2) :: num_proc_dim

        type(t_pair) :: processor_local_properties

        processor_local_properties%local_grid_position(1)= mod(pid-1,num_proc_dim(1))+1
        processor_local_properties%local_grid_position(2)= ((pid-1)/num_proc_dim(1))+1

    end subroutine calculate_local_grid_position


    subroutine calculate_local_grid_size(pid, grid_size, num_proc_dim, processor_local_properties)
        
        ! Function calculates the size of the 2D subgrid assigned to processor number n

        implicit none
        integer :: grid_size, pid
        integer, dimension(2) :: num_proc_dim

        type(t_pair) :: processor_local_properties

        if(processor_local_properties%local_grid_position(1) .eq. 1) then
            processor_local_properties%local_grid_size(1)=grid_size / num_proc_dim(1) + mod(grid_size, num_proc_dim(1))
        else
            processor_local_properties%grid_size_1st_row_col(1)=grid_size / num_proc_dim(1) + mod(grid_size, num_proc_dim(1))
            processor_local_properties%local_grid_size(1)=grid_size / num_proc_dim(1) 
        end if

        if(processor_local_properties%local_grid_position(2) .eq. 1) then
            processor_local_properties%local_grid_size(2)=grid_size / num_proc_dim(2) + mod(grid_size, num_proc_dim(2))
        else
            processor_local_properties%grid_size_1st_row_col(2)=grid_size / num_proc_dim(2) + mod(grid_size, num_proc_dim(2))
            processor_local_properties%local_grid_size(2)=grid_size / num_proc_dim(2)
        end if
        

    end subroutine calculate_local_grid_size


    subroutine calculate_local_grid_start_position(pid, processor_local_properties)
        
        ! Function calculates the indexes of first cell of the 2D subgrid assigned to processor number n.

        implicit none
        integer :: grid_size, pid

        type(t_pair) :: processor_local_properties

        integer :: loc_grid_size_row1, loc_grid_size_col1
        integer :: loc_grid_size_rowi, loc_grid_size_coli 
        integer :: loc_grid_pos_row, loc_grid_pos_col
                
        if(processor_local_properties%local_grid_position(1) .ge. 2) then
            loc_grid_size_row1 =processor_local_properties%grid_size_1st_row_col(1)
            loc_grid_size_rowi =processor_local_properties%local_grid_size(1)
            loc_grid_pos_row= processor_local_properties%local_grid_position(1)

            processor_local_properties%local_grid_start_position(1)= loc_grid_size_row1+loc_grid_size_rowi*(loc_grid_pos_row-2)+1
        else
            processor_local_properties%local_grid_start_position(1)=1
        end if

        if(processor_local_properties%local_grid_position(2) .ge. 2) then
            loc_grid_size_col1  =processor_local_properties%grid_size_1st_row_col(2)
            loc_grid_size_coli =processor_local_properties%local_grid_size(2)
            loc_grid_pos_col =processor_local_properties%local_grid_position(2)
            processor_local_properties%local_grid_start_position(2)= loc_grid_size_col1+loc_grid_size_coli*(loc_grid_pos_col-2)+1
        else
            processor_local_properties%local_grid_start_position(2)=1
        end if

    end subroutine calculate_local_grid_start_position


    subroutine calculate_neighbour_nodes(pid, num_proc_dim, processor_local_properties)
        
        ! Function calculates all the 8 neigbour subgrids for a given subgrid
        
        implicit none
        integer :: pid
        integer, dimension(2) :: num_proc_dim

        type(t_pair) :: processor_local_properties

        integer :: neighbour_node_row, neighbour_node_col


        neighbour_node_row = processor_local_properties%local_grid_position(1)
        neighbour_node_col = processor_local_properties%local_grid_position(2) -1
        if(neighbour_node_col .eq. 0) then 
            neighbour_node_col = num_proc_dim(2)
        end if
        processor_local_properties%neighbour_node(1)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row
        

        neighbour_node_row = processor_local_properties%local_grid_position(1) -1
        if(neighbour_node_row .eq. 0) then 
            neighbour_node_row = num_proc_dim(1)
        end if
        neighbour_node_col = processor_local_properties%local_grid_position(2) -1
        if(neighbour_node_col .eq. 0) then 
            neighbour_node_col = num_proc_dim(2)
        end if
        processor_local_properties%neighbour_node(2)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row
        

        neighbour_node_row = processor_local_properties%local_grid_position(1) - 1
        if(neighbour_node_row .eq. 0) then 
            neighbour_node_row = num_proc_dim(1)
        end if
        neighbour_node_col = processor_local_properties%local_grid_position(2)
        processor_local_properties%neighbour_node(3)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row


        neighbour_node_row = processor_local_properties%local_grid_position(1) -1
        if(neighbour_node_row .eq. 0) then 
            neighbour_node_row = num_proc_dim(1)
        end if
        neighbour_node_col = processor_local_properties%local_grid_position(2) +1
        if(neighbour_node_col .eq. num_proc_dim(2)+1) then 
            neighbour_node_col = 1
        end if
        processor_local_properties%neighbour_node(4)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row
        

        neighbour_node_row = processor_local_properties%local_grid_position(1)
        neighbour_node_col = processor_local_properties%local_grid_position(2) + 1
        if(neighbour_node_col .eq. num_proc_dim(2)+1) then 
            neighbour_node_col = 1
        end if
        processor_local_properties%neighbour_node(5)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row


        neighbour_node_row = processor_local_properties%local_grid_position(1) + 1
        if(neighbour_node_row .eq. num_proc_dim(1)+1) then 
            neighbour_node_row = 1
        end if
        neighbour_node_col = processor_local_properties%local_grid_position(2) + 1
        if(neighbour_node_col .eq. num_proc_dim(2)+1) then 
            neighbour_node_col = 1
        end if
        processor_local_properties%neighbour_node(6)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row


        neighbour_node_row = processor_local_properties%local_grid_position(1) + 1
        if(neighbour_node_row .eq. num_proc_dim(1)+1) then 
            neighbour_node_row = 1
        end if
        neighbour_node_col = processor_local_properties%local_grid_position(2)
        processor_local_properties%neighbour_node(7)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row


        neighbour_node_row = processor_local_properties%local_grid_position(1) + 1
        if(neighbour_node_row .eq. num_proc_dim(1)+1) then 
            neighbour_node_row = 1
        end if
        neighbour_node_col = processor_local_properties%local_grid_position(2) -1
        if(neighbour_node_col .eq. 0) then 
            neighbour_node_col = num_proc_dim(2)
        end if
        processor_local_properties%neighbour_node(8)= (neighbour_node_col - 1) * num_proc_dim(1) + neighbour_node_row


    end subroutine calculate_neighbour_nodes


    subroutine send_initial_grid_to_nodes(global_grid, processor_properties)
        
        ! Initially, the game data will be built on Node with rank 0. This node will have to scatter corresponding subgrids data to all the other nodes.
        ! This function sends data from root node to all other nodes

        implicit none
        integer :: request, status(mpi_status_size)
        integer :: newtype_subarray, ierr, tag

        integer :: global_grid(:,:)
        integer :: processor_properties(:,:)

        integer, dimension(2) :: sizes, local_grid_size, local_grid_start
        integer :: height, width

        integer :: i, j, num_procs
        integer :: start_pos_row, start_pos_col, local_height, local_width

        num_procs= size(processor_properties, 2)
        
        height = size(global_grid, 1)
        width = size(global_grid, 2)

        do i=1,num_procs,1

            start_pos_row= processor_properties(3,i)
            start_pos_col= processor_properties(4,i)
            local_height= processor_properties(1,i)
            local_width= processor_properties(2,i)

            do j=2,local_width+1
                tag= j*8
                
                call mpi_isend(global_grid(start_pos_row : start_pos_row + local_height - 1, start_pos_col + j -2), &
                            local_height, MPI_INTEGER, i, &
                            tag, mpi_comm_world, request, ierr)
            end do

        end do  


    end subroutine send_initial_grid_to_nodes


    subroutine recieve_initial_grid_to_nodes(local_grid)
        
        ! Initially, the game data will be built on Node with rank 0. This node will have to scatter corresponding subgrids data to all the other nodes.
        ! This function recieves data from root node into all other nodes
        
        implicit none
        integer :: request, status(mpi_status_size)
        integer :: newtype_subarray, ierr, tag

        integer :: local_grid(:,:)

        integer :: height, width

        integer :: i

        height = size(local_grid, 1)
        width = size(local_grid, 2)

        do i=2,width-1
            tag= i*8

            call mpi_recv(local_grid(2:height-1, i), &
                            height-2, MPI_INTEGER, 0, tag, mpi_comm_world, status, ierr)
           
        end do  

    end subroutine recieve_initial_grid_to_nodes


    subroutine send_properties_to_pid0(processor_local_properties)
        
        implicit none
        integer :: request, status(mpi_status_size)
        integer :: newtype_subarray, ierr
        integer :: grid_size, num_procs, num_of_iterations
        
        integer :: properties_to_send(4,1)
        integer, dimension(2) :: sizes, local_grid_size, local_grid_start

        type(t_pair) :: processor_local_properties

        properties_to_send(1,1)=processor_local_properties%local_grid_size(1)
        properties_to_send(2,1)=processor_local_properties%local_grid_size(2)
        properties_to_send(3,1)=processor_local_properties%local_grid_start_position(1)
        properties_to_send(4,1)=processor_local_properties%local_grid_start_position(2)

        call mpi_isend(properties_to_send, 4, MPI_INTEGER, 0, &
                        123, mpi_comm_world, request, ierr)

    end subroutine send_properties_to_pid0


    subroutine recieve_properties_to_pid0(num_procs ,processor_properties)        
        implicit none
        integer :: request, status(mpi_status_size)
        integer :: newtype_subarray, ierr
        integer :: grid_size, num_procs, num_of_iterations
        
        integer :: processor_properties(:,:)
        integer :: i

        do i=1,num_procs
            call mpi_recv(processor_properties(1:4,i), 4, MPI_INTEGER, i, &
                        123, mpi_comm_world, status, ierr)
        end do

        
    end subroutine recieve_properties_to_pid0


    subroutine communicate_columns_to_from_neigbours(local_grid, processor_local_properties)        
        
        ! Function communicates column information to and fro from 2 neigbours up and down
        
        implicit none
        integer :: request, status(mpi_status_size), ierr
        
        integer :: local_grid(:,:)

        type(t_pair) :: processor_local_properties

        integer :: height, width
        integer, dimension(:), allocatable :: col_to_send(:)

        height = size(local_grid, 1)
        width = size(local_grid, 2)
        allocate(col_to_send(height - 2))

        col_to_send = local_grid(2 : height - 1, 2)
        call mpi_isend(col_to_send, height - 2, MPI_INTEGER, processor_local_properties%neighbour_node(1), &
                        123, mpi_comm_world, request, ierr)

        call mpi_recv(local_grid(2 : height - 1, width), height-2, MPI_INTEGER, processor_local_properties%neighbour_node(5), &
                        123, mpi_comm_world, status, ierr)

        col_to_send = local_grid(2 : height - 1, width-1)
        call mpi_isend(col_to_send, height - 2, MPI_INTEGER, processor_local_properties%neighbour_node(5), &
                        123, mpi_comm_world, request, ierr)

        call mpi_recv(local_grid(2 : height - 1, 1), height-2, MPI_INTEGER, processor_local_properties%neighbour_node(1), &
                        123, mpi_comm_world, status, ierr)

        ! No need for the temporary column anymore
        deallocate(col_to_send)

    end subroutine communicate_columns_to_from_neigbours


    subroutine communicate_rows_to_from_neigbours(local_grid, processor_local_properties)        
        
        ! Function communicates row information to and fro from 2 neigbours left and right

        implicit none
        ! integer :: request, status(mpi_status_size), ierr
        
        integer :: request, status(mpi_status_size)
        integer :: newtype, ierr
        integer :: local_grid(:,:)

        type(t_pair) :: processor_local_properties

        integer :: height, width
        integer, dimension(:), allocatable :: col_to_send(:)

        height = size(local_grid, 1)
        width = size(local_grid, 2)

        call MPI_TYPE_VECTOR(width-2, 1, height, MPI_INTEGER, newtype,ierr)
        call MPI_TYPE_COMMIT(newtype,ierr)

        call mpi_isend(local_grid(2,2), 1, newtype, processor_local_properties%neighbour_node(3), &
                        123, mpi_comm_world, request, ierr)

        call mpi_recv(local_grid(height, 2:width-1), width-2, MPI_INTEGER, processor_local_properties%neighbour_node(7), &
                        123, mpi_comm_world, status, ierr)

        call mpi_isend(local_grid(height-1,2), 1, newtype, processor_local_properties%neighbour_node(7), &
                        123, mpi_comm_world, request, ierr)

        call mpi_recv(local_grid(1, 2:width-1), width-2, MPI_INTEGER, processor_local_properties%neighbour_node(3), &
                        123, mpi_comm_world, status, ierr)

    end subroutine communicate_rows_to_from_neigbours


    subroutine communicate_diagnols_to_from_neigbours(local_grid, processor_local_properties)
        
        ! Function communicates corner information to and fro from 4 neigbour on the diagnals
        
        implicit none
        integer :: request, status(mpi_status_size), ierr
        
        integer :: local_grid(:,:)

        type(t_pair) :: processor_local_properties

        integer :: height, width

        height = size(local_grid, 1)
        width = size(local_grid, 2)

        call mpi_isend(local_grid(2,2), 1, MPI_INTEGER, processor_local_properties%neighbour_node(2), &
                        123, mpi_comm_world, request, ierr)
        call mpi_recv(local_grid(height,width), 1, MPI_INTEGER, processor_local_properties%neighbour_node(6), &
                        123, mpi_comm_world, status, ierr)

        call mpi_isend(local_grid(2,width-1), 1, MPI_INTEGER, processor_local_properties%neighbour_node(4), &
                        123, mpi_comm_world, request, ierr)
        call mpi_recv(local_grid(height,1), 1, MPI_INTEGER, processor_local_properties%neighbour_node(8), &
                        123, mpi_comm_world, status, ierr)
        
        call mpi_isend(local_grid(height-1,width-1), 1, MPI_INTEGER, processor_local_properties%neighbour_node(6), &
                        123, mpi_comm_world, request, ierr)
        call mpi_recv(local_grid(1,1), 1, MPI_INTEGER, processor_local_properties%neighbour_node(2), &
                        123, mpi_comm_world, status, ierr)
        
        call mpi_isend(local_grid(height-1,2), 1, MPI_INTEGER, processor_local_properties%neighbour_node(8), &
                        123, mpi_comm_world, request, ierr)
        call mpi_recv(local_grid(1,width), 1, MPI_INTEGER, processor_local_properties%neighbour_node(4), &
                        123, mpi_comm_world, status, ierr)

    end subroutine communicate_diagnols_to_from_neigbours


    subroutine update_local_grid_game_of_life(local_grid, processor_local_properties)
        
        ! Function that plays game of life on a given 2D matrix

        implicit none
        integer :: local_grid(:,:)

        integer, ALLOCATABLE:: output_local_grid(:,:)

        type(t_pair) :: processor_local_properties

        integer :: height, width
        integer :: i, j, i2, j2, count
        height = size(local_grid, 1)
        width = size(local_grid, 2)

        allocate(output_local_grid(height,width))

        do j = 2, width-1
            do i = 2, height-1

                count=0

                if(local_grid(i+1,j+1) .eq. 1) then
                    count=count+1
                end if

                if(local_grid(i+1,j).eq. 1) then
                    count=count+1
                end if

                if(local_grid(i,j+1).eq. 1) then
                    count=count+1
                end if

                if(local_grid(i-1,j-1).eq. 1) then
                    count=count+1
                end if

                if(local_grid(i-1,j).eq. 1) then
                    count=count+1
                end if

                if(local_grid(i,j-1).eq. 1) then
                    count=count+1
                end if

                if(local_grid(i+1,j-1).eq. 1) then
                    count=count+1
                end if

                if(local_grid(i-1,j+1).eq. 1) then
                    count=count+1
                end if

                if(count .eq. 3) then
                    output_local_grid(i,j)=1
                else if(count .eq. 2) then
                    output_local_grid(i,j)=local_grid(i,j)
                else
                    output_local_grid(i,j)=0
                end if

            end do
        end do

        do j = 2, width-1
            do i = 2, height-1
                local_grid(i,j)=output_local_grid(i,j)
            end do
        end do
        
        deallocate(output_local_grid)

    end subroutine update_local_grid_game_of_life


    subroutine send_updated_grid_to_pid0(local_grid)
        
        ! After update of the game locally on the 2D subgrids, all subgrids will send the update back to with rank 0.
        ! This function sends updates from all the nodes to node 0
        
        implicit none
        integer :: request, status(mpi_status_size)
        integer :: newtype_subarray, ierr, tag

        integer :: local_grid(:,:)
        integer, dimension(:), allocatable :: col_to_send(:)

        integer, dimension(2) :: sizes, local_grid_size, local_grid_start
        integer :: height, width, i
        
        height = size(local_grid, 1)
        width = size(local_grid, 2)


        allocate(col_to_send(height - 2))

        do i=2,width-1
            tag= i*8

            col_to_send = local_grid(2 : height - 1, i)
            call mpi_isend(col_to_send, height - 2, MPI_INTEGER, 0, &
                            tag, mpi_comm_world, request, ierr)
        end do

        deallocate(col_to_send)

    end subroutine send_updated_grid_to_pid0


    subroutine recieve_updated_grid_to_pid0(global_grid, processor_properties)

        ! After update of the game locally on the 2D subgrids, all subgrids will send the update back to with rank 0.
        ! This function recieves updates from all the nodes into node 0

        implicit none
        integer :: request, status(mpi_status_size)
        integer :: newtype_subarray, ierr, tag

        integer :: global_grid(:,:)

        integer :: height, width

        integer :: processor_properties(:,:)
        integer :: i, j, num_procs

        integer :: total_elems, start_pos_row, start_pos_col, local_height, local_width

        num_procs= size(processor_properties, 2)

        height = size(global_grid, 1)
        width = size(global_grid, 2)


        do i=1,num_procs,1

            start_pos_row= processor_properties(3,i)
            start_pos_col= processor_properties(4,i)
            local_height= processor_properties(1,i)
            local_width= processor_properties(2,i)

            do j=2,local_width+1
                tag= j*8

                call mpi_recv(global_grid(start_pos_row : start_pos_row + local_height - 1, start_pos_col + j -2), &
                            local_height, MPI_INTEGER, i, tag, mpi_comm_world, status, ierr)
            end do

        end do    

    end subroutine recieve_updated_grid_to_pid0

end module utility_func