  
OBJECTS = gaussian_elim_FixMe.F90
.PHONY: clean

output.txt: main.exe
	./main.exe > output.txt

main.exe: $(OBJECTS)
	gfortran $(OBJECTS) -o main.exe

%.o : %.f90
	gfortran -c $<

clean:
	rm -f $(OBJECTS) main.exe
