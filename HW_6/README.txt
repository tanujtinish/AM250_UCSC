To run all the tasks, run following commands as explained:

1. module load rocks-openmpi
2. module load gcc-6.2.0

Run following commands for task 3(ping_pong) in order
    mpif90 -o ping_pong_timing_object_code ping_pong_timing.f90
    qsub -I -l nodes=2:ppn=1
    cd $PBS_O_WORKDIR
    mpirun -np 2 ping_pong_timing_object_code
    exit
    rm -f ping_pong_timing_object_code


