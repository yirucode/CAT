program CAT
    implicit none
    ! === given data ====
    ! === parameter ===
    integer,parameter :: row = 10000 !重複次數
    integer,parameter :: col = 300 !題庫數
    integer,parameter :: length = 40 !作答題長
    ! === item parameter ===
    real::a(col), b(col), c(col) !題庫試題參數
    ! === unknown data ===
    ! === 運算暫存用 ===
    integer :: place !位置
    real :: maxv !最大值   
    integer :: usedPool(col) 
    ! === output data ===
    real :: infor(col) !題庫各試題的訊息量
    integer::place_choose(row,length) !選題的試題位置
    real::a_choose(row,length),b_choose(row,length),c_choose(row,length) !選題的試題參數
    ! === 迴圈用 ===
    integer :: i,j
    integer :: try
    integer :: choose
    ! === 存取時間 ===
    real (kind=8) t1 !開始時間
    real (kind=8) t2 !結束時間
    ! === run code ===




end program CAT