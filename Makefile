# /codes/multifile/Makefile5

OBJECTS = read_data.o
FLAGS = -Wall -Wextra -fdefault-real-8 -fdefault-double-8
.PHONY: clean

output.txt: main.exe
	./main.exe > output.txt

main.exe: $(OBJECTS)
	  gfortran $(OBJECTS) -o main.exe

%.o : %.F90
	gfortran -c $(FLAGS) $< 

clean:
	rm -f $(OBJECTS) main.exe
