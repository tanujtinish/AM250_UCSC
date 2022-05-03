#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N hello_mpi_grape_2
#PBS -o ./output_files
#PBS -e ./error_files
#PBS -l nodes=2:ppn=4
#PBS -l walltime=00:01:00

cd $PBS_O_WORKDIR
mpirun -np 8 hello_mpi_grape

