I have implemented the Giga version(level 6) that decomposes the matrix in both directions.

On grape, do the following to compile and run my code:

STEP 1: Make sure all the required libraries are loaded using commands:
1. module load rocks-openmpi
2. module load gcc-6.2.0

STEP 2: To change the 
A) Grid size: Edit game_of_life_mpi_mega_version.f90 and change variable grid_size
B) The number of processors: Edit game_of_life_mpi_mega_version.f90 and and change variable num_procs
C) The number of iterations: Edit game_of_life_mpi_mega_version.f90 and change variable num_of_iterations

    By default, values of these variables are:
    grid_size=20
    num_procs=4
    num_of_iterations=80


STEP 3: To compile my code, run the following commands in order
    mpif90 -o utility_func.o -c  utility_func.f90
    mpif90 -o game_of_life_mpi_giga_version.o  utility_func.o  game_of_life_mpi_giga_version.f90

STEP 4: To run my code with n processors, run the following command
    mpirun -np n game_of_life_mpi_giga_version.o




PS:
    File game_of_life_mpi_giga_version_timings.f90:
    Used for calculating execution times for game of life for different values of N

    File ones_for_tc_calculation.f90:
    Used for calculating Tc constant for the performance model




