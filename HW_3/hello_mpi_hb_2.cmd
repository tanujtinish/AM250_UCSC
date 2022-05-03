#!/bin/bash

#SBATCH -p Instruction
#SBATCH -J hello_mpi_hb_2
#SBATCH -e ./error_files/hello_mpi_hb_2_%j.err
#SBATCH -o ./output_files/hello_mpi_hb_2_%j.out
#SBATCH --nodes 2
#SBATCH --ntasks-per-node 4
#SBATCH -t 00:05:00

mpirun -np 8 hello_mpi_hb
