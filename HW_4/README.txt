To run all the tasks, run following commands as explained:

1. module load rocks-openmpi
2. module load gcc-6.2.0

Run following commands for task 1(hello_world) in order
    mpif90 -o hello_world_object_code hello_world.f90
    qsub -I -l nodes=2:ppn=4
    cd $PBS_O_WORKDIR
    mpirun -np 8 hello_world_object_code
    exit
    rm -f hello_world_object_code


Run following commands for task 2(simple_send_recieve) in order
    mpif90 -o ssr_object_code simple_send_recieve.f90
    qsub -I -l nodes=3:ppn=1
    cd $PBS_O_WORKDIR
    mpirun -np 3 ssr_object_code
    exit
    rm -f ssr_object_code


Run following commands for task 3(ping_pong) in order
    mpif90 -o ping_pong_object_code ping_pong.f90
    qsub -I -l nodes=2:ppn=1
    cd $PBS_O_WORKDIR
    mpirun -np 2 ping_pong_object_code
    exit
    rm -f ping_pong_object_code


Run following commands for task 5(ring) in order
    mpif90 -o ring_object_code ring.f90
    qsub -I -l nodes=2:ppn=4
    cd $PBS_O_WORKDIR
    mpirun -np 8 ring_object_code
    exit
    rm -f ring_object_code


Run following commands for task 6(pi) in order
    mpif90 -o pi_object_code pi.f90
    qsub -I -l nodes=8:ppn=1
    cd $PBS_O_WORKDIR
    mpirun -np 8 pi_object_code
    exit
    rm -f pi_object_code