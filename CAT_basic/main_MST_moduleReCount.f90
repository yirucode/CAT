program MST
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "ListCAT_module.txt"
    ! === MST set ===
    integer, parameter :: numStages = 2
    integer, parameter :: maxLevel = 3
    integer, parameter :: numModuleInLevel = 5
    integer, parameter :: maxModule = maxLevel*numModuleInLevel
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數
    ! === input data ===
    integer:: placeModule_choose(numStages, numTest) !被選擇的Module
    ! === output data ===
    real :: overlap
    ! === 迴圈用 ===
    integer :: i,j
    ! 輸入module
    open(100, file= dataPath, status="old") 
    read(100,*)  ! 跳過第一列
    do i=1,numTest
        read(100,*) (placeModule_choose(j,i),j=1,numStages) !三參數
    enddo
    close(100)
    ! 開始計算重疊
    call subr_testOverlap(placeModule_choose(2,:), numTest, 1, overlap)
    WRITE(*,*) overlap






end program MST