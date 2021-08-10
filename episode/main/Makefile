# variables
FC=gfortran
CFLAGS=-c -g -Og -Wall

# linking
a.out: main.o
	$(FC) main.o

# compiling
main.o: main.f90
	$(FC) $(CFLAGS) main.f90


# cleanup
clean:
	del *.exe *.o
# rm a.exe 運行時會有問題，待調整

# run
run:
	make
	./a.exe