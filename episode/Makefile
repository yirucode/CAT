# variables
# Compiler settings - Can be customized.
CC = gfortran
CXXFLAGS =-c -g -Og -Wall

# linking
a.out: helloVSCode.f90 getName.o
	$(CC) helloVSCode.f90 getName.o

# compiling
getName.o: getName.f90
	$(CC) $(CXXFLAGS) getName.f90

# cleanup
clean:
	del *.exe *.o
# rm a.exe 運行時會有問題，待調整

# run
run:
	make
	./a.exe