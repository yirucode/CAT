program MST
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_300.txt"
    ! === parameter ===
    integer,parameter :: row = 10000 !重複次數
    integer,parameter :: col = 300 !題庫數
    integer,parameter :: length = 40 !作答題長
    ! === item parameter ===
    real::a(col), b(col), c(col) !題庫試題參數
    ! === true theta ===
    real :: thetaTrue(row) = 1. !真實能力值
    real :: thetaTrueMean !真實能力值之平均
    real :: thetaBegin = 0.
    ! === function ===
    real, external :: information, probability, normalPDF
    ! === unknown data ===
    ! === 迴圈用 ===
    integer :: i,j
    integer :: try
    integer :: choose
    ! === 運算暫存用 ===
    real :: maxv !最大值 
    integer :: place
    real :: infor(col) !題庫各試題的訊息量
    integer :: usedPool(col, row) !被使用過的題目  
    real :: randv(length, row)
    ! === output data ===
    integer :: resp(length, row) !作答反應
    integer:: place_choose(length, row) !選題的試題位置
    real:: a_choose(length, row),b_choose(length, row), &
    c_choose(length, row) !選題的試題參數
    ! 試題使用率
    real:: usedRate(col)
    real:: usedRateMax
    real:: usedRateMean
    real:: usedRateVar
    ! 測驗重疊率參數
    real:: testOverlap
    ! 估計能力參數
    real::thetaHat(length, row)
    real::thetaHatMean !估計能力值的平均數
    real::thetaBias !估計能力值與真值的差之平均
    real::thetaHatVar !估計能力值的變異數
    real::thetaHatMSE !估計能力值的MSE
    ! item pool 的相關資料紀錄 ===
    real :: poolUsedRate
    ! === 存取時間 ===
    real (kind=8) t1 !開始時間
    real (kind=8) t2 !結束時間
    ! === output error ===
    integer :: ierror
    ! === 輸出資料格式設定 === 
    character(len = 20), parameter :: input = 'ListCAT_thetaHat.txt'
    character(len = 20), parameter :: dataINT = '(100I10)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataF = '(100F10.4)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataPool = '(500I10)' ! 隨著 pool item number 改變而改變
    ! === run code ===
    call cpu_time (t1) !開始計時
    ! 讀取資料：輸入試題參數
    open(100, file= dataPath, status="old") 
    do i=1,col
        read(100,*) a(i),b(i),c(i) !三參數
    enddo
    close(100)
    ! 開始模擬




    

    stop
end program MST

