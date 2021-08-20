# variables
# Compiler settings - Can be customized.
CC = gfortran
CXXFLAGS =-c -g -Og -Wall

# linking
a.out: main.o func_information.o func_normalPDF.o func_probability.o subr_response.o
	$(CC) main.o func_information.o func_normalPDF.o func_probability.o subr_response.o

# compiling
main.o: main.f90
	$(CC) $(CXXFLAGS) main.f90

func_information.o: func_information.f90
	$(CC) $(CXXFLAGS) func_information.f90

func_normalPDF.o: func_normalPDF.f90
	$(CC) $(CXXFLAGS) func_normalPDF.f90

func_probability.o: func_probability.f90
	$(CC) $(CXXFLAGS) func_probability.f90

subr_response.o: subr_response.f90
	$(CC) $(CXXFLAGS) subr_response.f90

# cleanup
clean:
	del *.exe *.o *.txt
# rm a.exe 運行時會有問題，待調整

# run
run:
	make
	./a.exe