program CAT
    implicit none
    ! === given data ====
    ! === parameter ===
    integer,parameter :: row = 500 !重複次數
    integer,parameter :: col = 300 !題庫數
    integer,parameter :: length = 40 !作答題長
    ! === item parameter ===
    real::a(col), b(col), c(col) !題庫試題參數
    ! === true theta ===
    real :: thetaTrue(row) = 1. !真實能力值
    real :: thetaBegin = 0.
    ! === function ===
    real, external :: information, probability, normalPDF
    ! === unknown data ===
    ! === 迴圈用 ===
    integer :: i
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
    ! === 存取時間 ===
    real (kind=8) t1 !開始時間
    real (kind=8) t2 !結束時間
    ! === run code ===
    call cpu_time (t1) !開始計時
    ! 讀取資料：輸入試題參數
    open(100,file="data/parameter_300.txt",status="old") 
    do i=1,col
        read(100,*) a(i),b(i),c(i) !三參數
    enddo
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
    !WRITE(*,*) (thetaHat(length, i),i=1,row)
    call subr_aveReal(thetaHat(length,:),row,thetaHatMean)
    WRITE(*,*) 'avg= ',thetaHatMean
    ! ! === 輸出資料
    ! !------------------
    ! open(101,file="summery_CAT_thetaHat.txt",status="replace") !寫下能力估計
    ! open(102,file="summery_CAT_response.txt",status="replace") !寫下作答反應
    ! open(103,file="summery_CAT_item.txt",status="replace") !寫下所選的試題
    ! open(104,file="summery_CAT_m.txt",status="replace") !紀錄m
    ! open(106,file="summery_CAT_item_choose.txt",status="replace")

    ! write(101,1011) "ID","true",(i,i=1,length)
    ! write(102,1021) "ID",(i,i=1,length)
    ! write(103,1031) "ID",(i,i=1,length)
    ! write(104,1041) "ID",(i,i=1,col) !紀錄m

    ! do i=1,row
    !     write(101,1012) i,thetaTrue(i),(thetaHat(i,j),j=1,length)
    !     write(102,1022) i,(resp(i,j),j=1,length) !寫下作答反應
    !     write(103,1032) i,(place_choose(i,j),j=1,length) !寫下所選的試題
    !     write(104,1042) i,(usedPool(i,j),j=1,col)
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

