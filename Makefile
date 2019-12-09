# /codes/multifile/Makefile5

OBJECTS = read_data.o write_to_screen.o ge_elim.o backward_solve.o linear_solve.o 
FLAGS = -Wall -Wextra -fdefault-real-8 -fdefault-double-8
.PHONY: clean

output.txt: main.exe
	./main.exe > output.txt

main.exe: $(OBJECTS)
	  gfortran $(OBJECTS) -o main.exe

%.o : %.f90
	gfortran -c $(FLAGS) $< 

clean:
	rm -f $(OBJECTS) main.exe *.mod
