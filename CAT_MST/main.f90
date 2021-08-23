Program CAT

    implicit none
    real (kind=8) t1 !開始時間
    real (kind=8) t2 !結束時間
    integer :: i,j
    integer :: try
    integer :: choose
    
    integer,parameter :: row=1000 !重複次數
    integer,parameter :: col=300 !題庫數
    integer,parameter :: length=40 !作答題長
    
    !------------------
    !選題使用參數
    real::a(col),b(col),c(col) !題庫試題參數
    real::a_used(col),b_used(col),c_used(col) !選題後的題庫試題參數
    real::infor(col) !題庫各試題的訊息量
    real::maxv !最大值
    integer::place !位置
    
    integer::resp(row,length) !作答反應
    real::randv(row,length) !亂數
    real::thetaHAT(row,length) !估計能力值
    
    integer::place_choose(row,length) !選題的試題位置
    real::a_choose(row,length),b_choose(row,length),c_choose(row,length) !選題的試題參數
    
    real,external:: probability,information,normalPDF !函數
    
    !------------------
    !EAP使用參數
    integer :: mat !根據估計間隔所切割的資料數
    real :: seq = 0.01 !估計間隔
    real,allocatable ::thetaCUT(:),PQmultiply(:),posterior(:) !切割能力值、機率乘積、後驗分配
    real,allocatable ::prob(:,:),pq(:,:)
    real::EAP_numerator !EAP分子
    real::EAP_denominator !EAP分母
    
    !------------------
    !計算最後總值所用參數
    real::thetaTRUE=1 !真實能力值
    real::thetaHatVariance !估計能力值的變異數
    real::thetaHATmean !估計能力值的平均數
    real::thetaHatMSE !估計能力值的MSE
    
    !------------------
    !EAP初始設定(放在參數設定的最後)
    mat = INT((3-(-3))/seq+1) +1
    !write(*,*) mat
    !allocate(matrixA(row,col,mat))
    allocate(thetaCUT(mat),PQmultiply(mat),posterior(mat)) !定義資料格數
    choose = 1
    allocate(prob(mat,choose),pq(mat,choose)) !重新定義的資料格數
    !deallocate(prob,pq) !取消定義的資料格數
    thetaCUT(1)=-3 !起始能力
    do i=2,mat
        thetaCUT(i)=thetaCUT(i-1)+seq !算出能力值
    enddo
    !write(*,*) thetaCUT
    
    !------------------
    !計時
    call cpu_time (t1) !開始計時
    
    
    !====================
    !======讀取資料======
    !====================
    
    !輸入試題參數
    open(113,file="data/parameter_300.txt",status="old") 
    do i=1,col
          read(113,*) a(i),b(i),c(i) !三參數
    enddo
    
    !try = 2
    do try=1,row
        do choose = 1,length
            
            if (choose == 1) then
                
                do i=1,col
                    a_used(i)=a(i)
                    b_used(i)=b(i)
                    c_used(i)=c(i)
                enddo		
                
                !------------------
                !選題
                !第一題
                !choose = 1	
                !計算題庫試題訊息量
                thetaHAT(try,choose)=0
                do i=1,col
                    infor(i)=information(thetaHAT(try,choose),a_used(i),b_used(i),c_used(i))
                enddo
                !write(*,*) infor
    
                !取題庫試題訊息量的最大值
                maxv=infor(1)
                do i=2,col !602
                    if(maxv<infor(i))then
                        maxv=infor(i)
                    else
                        maxv=maxv
                    end if
                end do
                !write(*,*) maxv
    
                !找出題庫試題相對位置
                do i=1,col
                    place=i
                    if(infor(i)==maxv) exit
                enddo
                place_choose(try,choose)=place
                !write(*,*) place_choose
    
                !存入選題
                a_choose(try,choose)=a_used(place)
                b_choose(try,choose)=b_used(place)
                c_choose(try,choose)=c_used(place)
    
                !------------------
                !移除已作答試題
                b_used(place)=1000
                !write(*,*) b_used
    
                !------------------
                !作答模擬
                call response(thetaTRUE,&
                a_choose(try,choose),b_choose(try,choose),c_choose(try,choose),&
                resp(try,choose),randv(try,choose))   
                !write(*,*) resp(try,choose)
    
                !------------------
                !EAP能力估計
                deallocate(prob,pq) !取消定義的資料格數
                allocate(prob(mat,choose),pq(mat,choose)) !重新定義的資料格數
    
                !計算作答反應機率
                do i=1,mat
                    j=choose
                    prob(i,j)=probability(thetaCUT(i),a_choose(try,j),b_choose(try,j),c_choose(try,j))
                    pq(i,j)=prob(i,j)**(resp(try,j))*(1-prob(i,j))**(1-resp(try,j))
                    !write(*,"(1X,20F10.4)") (pq(i,j),j=1,choose)  !隨題數變動
                enddo
    
                !計算作答反應機率乘機與倒數
                do i=1,mat
                    PQmultiply(i)=pq(i,1)
                !	do j=2,choose
                !		PQmultiply(i)=PQmultiply(i)*pq(i,j)
                !	enddo 
                    posterior(i)=PQmultiply(i)*normalPDF(thetaCUT(i))
                    !write(*,*) log(posterior(i))
                enddo
    
                EAP_numerator=posterior(1)*thetaCUT(1)
                EAP_denominator=posterior(1)
                do i=2,mat
                    EAP_numerator=EAP_numerator + posterior(i)*thetaCUT(i)
                    EAP_denominator=EAP_denominator + posterior(i)
                end do
                !根據最大值的位置取出能力估計值
                thetaHAT(try,choose)=EAP_numerator/EAP_denominator
                !write(*,*) (resp(try,i),i=1,choose)
                !write(*,*) thetaHAT(try,choose)
                    
            else
                !第二題以後
                !choose = 2~
    
                !計算題庫試題訊息量
                do i=1,col
                    infor(i)=information(thetaHAT(try,choose-1),a_used(i),b_used(i),c_used(i))
                enddo
                !write(*,*) b_used
                !write(*,*) infor
    
                do i=1,col
                    if(isnan(infor(i)))then
                        infor(i)=-1 !處理NaN，代換成數值-1
                    else
                        infor(i)=infor(i)
                    end if 
                end do
                !write(*,*) infor
    
                !取題庫試題訊息量的最大值
                maxv=infor(1)
                do i=2,col !602
                    if(maxv<infor(i))then
                        maxv=infor(i)
                    else
                        maxv=maxv
                    end if
                end do
                !write(*,*) maxv
    
                !找出題庫試題相對位置
                do i=1,col
                    place=i
                    if(infor(i)==maxv) exit
                enddo
                place_choose(try,choose)=place
                !write(*,*) place_choose
    
                !存入選題
                a_choose(try,choose)=a_used(place)
                b_choose(try,choose)=b_used(place)
                c_choose(try,choose)=c_used(place)
    
                !------------------
                !移除已作答試題
                b_used(place)=1000
    
                !------------------
                !作答模擬
                call response(thetaTRUE,&
                a_choose(try,choose),b_choose(try,choose),c_choose(try,choose),&
                resp(try,choose),randv(try,choose))   
                !write(*,*) resp(try,choose)
    
                !------------------
                !EAP能力估計
                deallocate(prob,pq) !取消定義的資料格數
                allocate(prob(mat,choose),pq(mat,choose)) !重新定義的資料格數
    
                !計算作答反應機率
                do i=1,mat
                    do j=1,choose
                        prob(i,j)=probability(thetaCUT(i),a_choose(try,j),b_choose(try,j),c_choose(try,j))
                        pq(i,j)=prob(i,j)**(resp(try,j))*(1-prob(i,j))**(1-resp(try,j))
                    enddo
                    !write(*,"(1X,20F10.4)") (pq(i,j),j=1,choose)  !隨題數變動
                enddo
    
                !計算作答反應機率乘機與倒數
                do i=1,mat
                    PQmultiply(i)=pq(i,1)
                    do j=2,choose
                        PQmultiply(i)=PQmultiply(i)*pq(i,j)
                    enddo 
                    posterior(i)=PQmultiply(i)*normalPDF(thetaCUT(i))
                    !write(*,*) log(posterior(i))
                enddo
    
                EAP_numerator=posterior(1)*thetaCUT(1)
                EAP_denominator=posterior(1)
                do i=2,mat
                    EAP_numerator=EAP_numerator + posterior(i)*thetaCUT(i)
                    EAP_denominator=EAP_denominator + posterior(i)
                end do
                !根據最大值的位置取出能力估計值
                thetaHAT(try,choose)=EAP_numerator/EAP_denominator
            
            end if 
        end do
        !write(*,*) (b_choose(try,i),i=1,length)
        !write(*,*) (randv(try,i),i=1,length)
        !write(*,*) (thetaHAT(try,i),i=1,length)
        !write(*,*) (resp(try,i),i=1,length)
    end do 
    
    
    !=======================
    !======寫入資料 01======
    !=======================
    
    !------------------
    !寫下能力估計
    open(114,file="summery_CAT_thetaHAT.txt",status="replace")
    write(114,"(A5,20I10)") "ID",(i,i=1,length)
    do i=1,row !10000
        write(114,"(I5,50F10.4)") i,(thetaHAT(i,j),j=1,length)
    end do
    
    !------------------
    !寫下作答反應
    open(115,file="summery_CAT_respones.txt",status="replace")
    write(115,"(A5,50I3)") "ID",(i,i=1,length)
    do i=1,row !10000
        write(115,"(I5,50I3)") i,(resp(i,j),j=1,length)
    end do
    
    !------------------
    !寫下所選的試題難度
    open(116,file="summery_CAT_item.txt",status="replace")
    write(116,"(A5,50I10)") "ID",(i,i=1,length)
    do i=1,row !10000
        write(116,"(I5,50I10)") i,(place_choose(i,j),j=1,length)
    end do
        
    
    !=======================!=======================
    !根據所估計的能力估計值進行計算
    !=======================!=======================
    
    !估計能力值的平均數
    thetaHATmean=thetaHAT(1,length)
    do i=2,row !10000
        thetaHATmean=thetaHATmean+thetaHAT(i,length)
    end do
    thetaHATmean=thetaHATmean/row
    !write(*,*) thetaHATmean
    
    !估計能力值的變異數
    thetaHatVariance=(thetaHAT(1,length)-thetaHATmean)**2
    do i=2,row !10000
        thetaHatVariance=thetaHatVariance+(thetaHAT(i,length)-thetaHATmean)**2
    end do
    thetaHatVariance=thetaHatVariance/row
    !write(*,"(F10.5)") thetaHatVariance
    
    !估計能力值的MSE
    thetaHatMSE=(thetaHAT(1,length)-thetaTRUE)**2
    do i=2,row !100
        thetaHatMSE=thetaHatMSE+(thetaHAT(i,length)-thetaTRUE)**2
    end do
    thetaHatMSE=thetaHatMSE/row
    !write(*,"(F10.5)") thetaHatMSE
    
    
    !=======================
    !======寫入資料 02======
    !=======================
    
    !------------------
    !停止計時
    call cpu_time (t2) !結束計時
    !write(*,"(A15,F15.5)") "Time=",t2-t1
    
    !------------------
    !寫入所需資料
    open(117,file="summery_CAT_other.txt",status="replace") 
    write (117,"(A15,I5,/,A15,I5,/,A15,I5,/)") 'item pool=',col,'length=',length,'try=',row ! / 為換行
    !write(117,"(6A15)") "Time","Mean","Bias","Variance","MSE"
    !write (117,"(5F15.5)")  t2-t1,thetaHATmean,thetaHATmean-thetaTRUE,thetaHatVariance,thetaHatMSE
    write(117,"(A15,F15.5)") "Time",t2-t1
    write(117,"(A15,F15.5)") "Mean",thetaHATmean
    write(117,"(A15,F15.5)") "Bias",thetaHATmean-thetaTRUE
    write(117,"(A15,F15.5)") "Variance",thetaHatVariance
    write(117,"(A15,F15.5)") "MSE",thetaHatMSE
    
    write(117,"(F15.5)") t2-t1
    write(117,"(F15.5)") thetaHATmean
    write(117,"(F15.5)") thetaHATmean-thetaTRUE
    write(117,"(F15.5)") thetaHatVariance
    write(117,"(F15.5)") thetaHatMSE
    
    end Program CAT
    