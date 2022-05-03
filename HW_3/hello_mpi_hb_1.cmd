#!/bin/bash

#SBATCH -p Instruction
#SBATCH -J hello_mpi_hb_1
#SBATCH -e ./error_files/hello_mpi_hb_1_%j.err
#SBATCH -o ./output_files/hello_mpi_hb_1_%j.out
#SBATCH --nodes 1
#SBATCH --ntasks-per-node 4
#SBATCH -t 00:05:00

mpirun -np 4 hello_mpi_hb
