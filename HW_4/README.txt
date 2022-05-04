To run all the tasks, run following commands as explained:

1. module load rocks-openmpi
2. module load gcc-6.2.0

Command to run task 1(hello_world)
    mpif90 -o hello_world_object_code hello_world.f90
    qsub -I -l nodes=2:ppn=4
    cd $PBS_O_WORKDIR
    mpirun -np 8 hello_world_object_code
    exit
    rm -f hello_world_object_code


Command to run task 2(simple_send_recieve)
    mpif90 -o ssr_object_code simple_send_recieve.f90
    qsub -I -l nodes=2:ppn=4
    cd $PBS_O_WORKDIR
    mpirun -np 8 ssr_object_code
    exit
    rm -f ssr_object_code


Command to run task 3(ping_pong)
    mpif90 -o ping_pong_object_code ping_pong.f90
    qsub -I -l nodes=2:ppn=4
    cd $PBS_O_WORKDIR
    mpirun -np 8 ping_pong_object_code
    exit
    rm -f ping_pong_object_code

Command to run task 5(ring)
    mpif90 -o ring_object_code ring.f90
    qsub -I -l nodes=2:ppn=4
    cd $PBS_O_WORKDIR
    mpirun -np 8 ring_object_code
    exit
    rm -f ring_object_code

Command to run task 6(pi)
    mpif90 -o pi_object_code pi.f90
    qsub -I -l nodes=2:ppn=4
    cd $PBS_O_WORKDIR
    mpirun -np 8 pi_object_code
    exit
    rm -f pi_object_code