#!/bin/csh

#SBATCH -p Instruction
#SBATCH -J hello_omp_hb_1
#SBATCH -e ./error_files/hello_omp_hb_1_%j.err
#SBATCH -o ./output_files/hello_omp_hb_1_%j.out
#SBATCH -N 1
#SBATCH -c 4
#SBATCH -t 00:05:00

setenv OMP_NUM_THREADS 4
./hello_omp_hb