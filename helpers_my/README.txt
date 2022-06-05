mpif90 -o utility_func.o -c  utility_func.f90
mpif90 -o game_of_life_mpi_giga_version.o  utility_func.o  game_of_life_mpi_giga_version.f90
mpirun -np 5 game_of_life_mpi_giga_version.o

mpif90 -o utility_func.o -c  utility_func.f90
mpif90 -o game_of_life_mpi_giga_version_timings.o  utility_func.o  game_of_life_mpi_giga_version_timings.f90
mpirun -np 5 game_of_life_mpi_giga_version_timings.o

gfortran -o cal helpers/cal.f90
./cal

tar cvf Gupta.tar AM250_project_Gupta