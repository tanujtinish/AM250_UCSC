compiler=gfortran

run: compile
	./object_code
	make clean

compile:
	$(compiler) -o object_code ones.f90

clean:
	rm -f trapezoidal_rule.o function.o