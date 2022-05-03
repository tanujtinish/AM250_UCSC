#!/bin/csh

#SBATCH -p Instruction
#SBATCH -J hello_omp_hb_2
#SBATCH -e ./error_files/hello_omp_hb_2_%j.err
#SBATCH -o ./output_files/hello_omp_hb_2_%j.out
#SBATCH -N 1
#SBATCH -c 8
#SBATCH -t 00:05:00

setenv OMP_NUM_THREADS 8
./hello_omp_hb