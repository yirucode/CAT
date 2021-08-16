# variables
# Compiler settings - Can be customized.
CC = gfortran
CXXFLAGS =-c -g -Og -Wall

# linking
a.out: main.f90 
	$(CC) main.f90

# compiling
# getName.o: getName.f90
# 	$(CC) $(CXXFLAGS) getName.f90

# cleanup
clean:
	del *.exe *.o *.txt
# rm a.exe 運行時會有問題，待調整

# run
run:
	make
	./a.exe