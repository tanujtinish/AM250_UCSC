To run all the tasks, run following commands as explained:

1. module load rocks-openmpi
2. module load gcc-6.2.0

To run task 1(hello_world), Run following commands in order
    A. gfortran -fopenmp hello_world_openmp.f90 -o hello_world
    B. ./hello_world


To run task 2(Finds the minimum value in the C matrix ), Run following commands in order
    A. gfortran -fopenmp matrix_op_openmp.f90 -o matrix_op_openmp
    B. ./matrix_op_openmp
