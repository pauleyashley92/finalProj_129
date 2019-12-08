# /codes/multifile/Makefile5

OBJECTS = read_data.o
.PHONY: clean

output.txt: main.exe
	./main.exe > output.txt

main.exe: $(OBJECTS)
	  gfortran $(OBJECTS) -o main.exe

%.o : %.F90
	gfortran -c $< 

clean:
	rm -f $(OBJECTS) main.exe
