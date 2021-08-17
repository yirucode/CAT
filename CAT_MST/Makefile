# variables
# Compiler settings - Can be customized.
CC = gfortran
CXXFLAGS =-c -g -Og -Wall

# linking
a.out: main.o 
	$(CC) main.o

# compiling
main.o: main.f90
	$(CC) $(CXXFLAGS) main.f90

# cleanup
clean:
	del *.exe *.o *.txt
# rm a.exe 運行時會有問題，待調整

# run
run:
	make
	./a.exe