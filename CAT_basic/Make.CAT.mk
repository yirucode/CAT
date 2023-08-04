# variables
# Compiler settings - Can be customized.
# -O3: 當gcc優化器發現某些變量在某些方面是多餘時，會將其優化掉
# -O0: 在某些情況下會使用具有相同數值的變量，編譯時應禁用優化。
# -Og: 它只應用那些不影響可調試性的優化
CC = gfortran
CXXFLAGS =-c -g -O0 -Og -Wall 
exe = a
obj = main_OMST_cont_SH.o \
subr_maxvReal.o subr_maxvInt.o \
subr_minvInt.o subr_minvReal.o\
subr_sumReal.o subr_sumInt.o\
subr_aveReal.o subr_aveIntToReal.o \
subr_varReal.o subr_mseReal.o\
subr_aveReal_kind8.o subr_varReal_kind8.o\
subr_resp.o \
subr_itemUsedYN.o subr_itemUsedSum.o \
subr_itemUsedRate.o subr_itemPoolUsedRate.o\
subr_testOverlap.o \
subr_testOmega.o subr_testPsi.o\
func_deltaPsi.o\
subr_contentCount.o subr_contentTargetP.o \
func_probability.o func_information.o\
func_normalPDF.o \
func_combination.o \
subr_EAP.o \
subr_setPK.o \
module_ran_mod.o \



# 待驗證的代碼
# main_CAT.o
# main_CAT_contentControl.o
# main_CAT_Psi
# main_CAT_Psi_contentC
# main_CAT_Psi_contentC_full
# main_D_MST
# main_D_MST_Psi
# main_D_MST_Psi_full
# main_MST_Psi
# main_MST_moduleReCount
# main_MST_12designP
# main_OMST_contentControl
# main_OMST_cont_Psi
# main_OMST_cont_Psi_full
# main_MST_12design
# main_MST_1234design
# main_MST_infor
# main_MST_theta
# main_OMST_cont_SHO
# main_OMST_cont_SH
# main_Random_cont


# obj = subr_testOmega.o

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