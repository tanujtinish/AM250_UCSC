compiler=gfortran

run: compile
	./object_code
	make clean

compile:
	$(compiler) -o object_code ones.f90

.PHONY: clean
clean:
	rm -f object_code