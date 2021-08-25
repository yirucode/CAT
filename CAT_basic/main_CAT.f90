program CAT
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_300.txt"
    ! === parameter ===
    integer,parameter :: row = 100 !重複次數
    integer,parameter :: col = 300 !題庫數
    integer,parameter :: length = 100 !作答題長
    ! === item parameter ===
    real::a(col), b(col), c(col) !題庫試題參數
    ! === true theta ===
    real :: thetaTrue(row) = 1. !真實能力值
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
    real :: infor(col) !題庫各試題的訊息量
    integer :: usedPool(col, row) !被使用過的題目  
    real :: randv(length, row)
    ! === output data ===
    integer :: resp(length, row) !作答反應
    integer:: place_choose(length, row) !選題的試題位置
    real:: a_choose(length, row),b_choose(length, row), &
    c_choose(length, row) !選題的試題參數
    ! 估計能力參數
    real::thetaHat(length, row)
    real::thetaHatVar !估計能力值的變異數
    real::thetaHatMean !估計能力值的平均數
    real::thetaHatMSE !估計能力值的MSE
    ! item pool 的相關資料紀錄 ===
    real :: PoolUsedRate
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
    do try = 1, row
        do  choose = 1, length
            ! 計算訊息量
            if ( choose == 1 ) then
                do i = 1, col
                    infor(i) = information(thetaBegin, a(i), b(i), c(i))
                enddo
            else
                do i = 1, col
                    if ( usedPool(i, try) == 0 ) then
                        infor(i) = information(thetaHat(choose-1, try), a(i), b(i), c(i))
                    else
                        infor(i) = 0
                    endif
                enddo
            endif
            call subr_maxvReal(infor, col, maxv, place_choose(choose, try)) ! 求出最大訊息量與其題庫ID(紀錄使用的試題題號)
            usedPool(place_choose(choose, try), try) = 1 !紀錄使用試題
            ! 紀錄使用的試題參數
            a_choose(choose, try) = a(place_choose(choose, try))
            b_choose(choose, try) = b(place_choose(choose, try))
            c_choose(choose, try) = c(place_choose(choose, try))
            ! 模擬作答反應
            call subr_resp(thetaTrue(try), &
            a_choose(choose, try),b_choose(choose, try),c_choose(choose, try),&
            resp(choose, try),randv(choose, try))
            ! EAP能力估計
            call subr_EAP(choose, &
            a_choose(1:choose, try),b_choose(1:choose, try),c_choose(1:choose, try),&
            resp(1:choose, try), thetaHat(choose, try))
        enddo
    end do
    call cpu_time (t2) !結束計時
    ! thetaHat 計算
    call subr_aveReal(thetaHat(length,:),row,thetaHatMean)
    call subr_varReal(thetaHat(length,:),row,thetaHatVar)
    call subr_mseReal(thetaHat(length,:),thetaTrue(:),row,thetaHatMSE)
    ! item pool 計算
    call subr_poolUsedRate(usedPool,row,col,PoolUsedRate)
    ! === 輸出資料 ===
    open(unit = 100 , file = 'ListCAT_summary.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A10,F10.5)') "time", t2-t1
    write(unit = 100, fmt = '(/,A)') "About thetaHat: "
    write(unit = 100, fmt = '(A10, F10.5)') "Mean = ", thetaHatMean
    write(unit = 100, fmt = '(A10, F10.5)') "Var = ", thetaHatVar
    write(unit = 100, fmt = '(A10, F10.5)') "MSE = ", thetaHatMSE
    write(unit = 100, fmt = '(/,A)') "About pool used: "
    write(unit = 100, fmt = '(A10, F10.5)') "Rate = ", PoolUsedRate
    close(100)
    open(unit = 100 , file = 'ListCAT_theta.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "thetaHat = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,row
        write(unit = 100, fmt = dataF) (thetaHat(j,i),j=1,length)
    end do
    close (100)
    open(unit = 100 , file = 'ListCAT_resp.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "response = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,row
        write(unit = 100, fmt = dataINT) (resp(j,i),j=1,length)
    end do
    close(100)
    open(unit = 100 , file = 'ListCAT_item.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "choose item = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,row
        write(unit = 100, fmt = dataINT) (place_choose(j,i),j=1,length)
    end do
    close(100)
    open(unit = 100 , file = 'ListCAT_poolUsed.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "pool used = "
    write(unit = 100, fmt = dataPool) (j, j=1,col)
    do i=1,row
        write(unit = 100, fmt = dataPool) (usedPool(j,i),j=1,col)
    end do
    close(100)


    !open(101,file="summery_CAT_thetaHat.txt",status="replace") !寫下能力估計
    ! open(102,file="summery_CAT_response.txt",status="replace") !寫下作答反應
    ! open(103,file="summery_CAT_item.txt",status="replace") !寫下所選的試題
    ! open(104,file="summery_CAT_m.txt",status="replace") !紀錄m
    

    ! write(102,1021) "ID",(i,i=1,length)
    ! write(103,1031) "ID",(i,i=1,length)
    ! write(104,1041) "ID",(i,i=1,col) !紀錄m

    ! do i=1,row
        ! write(unit = 100, fmt = dataF) (thetaHat(j,i),j=1,length)
        ! write(102,1022) i,(resp(j,i),j=1,length) !寫下作答反應
        ! write(103,1032) i,(place_choose(j,i),j=1,length) !寫下所選的試題
        ! write(104,1042) i,(usedPool(j,i),j=1,col)
    ! end do

    ! 1011 Format(A5,A10,50I10)
    ! 1021 Format(I5,50F10.4)
    ! 1031 Format(A5,50I10)
    ! 1041 Format(A5,500I10)
    
    ! 1012 Format(I5,50F10.4)
    ! 1022 Format(I5,50I3)
    ! 1032 Format(I5,50I10)
    ! 1042 Format(I5,500I10)
    stop
end program CAT

