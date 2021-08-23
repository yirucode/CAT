# variables
# Compiler settings - Can be customized.
CC = gfortran
CXXFLAGS =-c -g -Og -Wall 
exe = a
obj = main.o

# linking 鏈結
a: $(obj)
	$(CC) -o $(exe) $(obj)

# compiling 編譯
%.o:%.f90
	$(CC) $(CXXFLAGS) $^ -o $@

# cleanup
# 偽目標
.PHONY : clean 
clean : 
	del -file *.exe *.o *.txt
# rm a.exe 運行時會有問題，待調整

# run
run:
	make
	./a.exe


# 代號說明：
# $^：代表目前的相依性項目
# $@：目前的目標項目名稱
# $*：代表目前的相依性項目，但不含副檔名。
# $?：代表需要重建（被修改）的相依性項目。