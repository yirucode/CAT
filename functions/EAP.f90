Program EAP

    implicit none
    real (kind=8) t1 !開始時間
    real (kind=8) t2 !結束時間
    integer::i,j,q !,place !會用到的迭代數值
    real,external:: probability,information,normalPDF !函數
    integer,parameter :: row=100 !重複次數
    integer,parameter :: col=20 !題數
    real::a(col),b(col),c(col)
    integer::u(row,col) !作答紀錄
    !real::maxv(row) !最大值
    real::thetaHAT(row) !能力估計值
    real::infor(row,col) !每題的訊息量計算
    real::inforSUM(row) !訊息量加總
    real::EAP_numerator !EAP分子
    real::EAP_denominator !EAP分母
    
    real::thetaTRUE=1 !真實能力值
    
    real::thetaHatVariance !估計能力值的變異數
    real::thetaHATmean !估計能力值的平均數
    real::thetaHatMSE !估計能力值的MSE
    
    integer :: mat !根據估計間隔所切割的資料數
    real :: seq = 0.01 !估計間隔
    real,allocatable ::theta(:),PQmultiply(:),posterior(:) !切割能力值、機率乘積、後驗分配
    real,allocatable ::prob(:,:),pq(:,:)
    
    call cpu_time(t1) !開始計時
    
    mat = INT((3-(-3))/seq+1)+1
    !write(*,*) mat
    !allocate(matrixA(row,col,mat))
    allocate(theta(mat),PQmultiply(mat),posterior(mat)) !定義資料格數
    allocate(prob(mat,col),pq(mat,col))
    !deallocate(theta,PQln,maxv) 取消定義的資料格數
    
    theta(1)=-3 !起始能力
    do i=2,mat
        theta(i)=theta(i-1)+seq !算出能力值
    enddo
    !write(*,*) theta
    
    !====================
    !======讀取資料======
    
    !輸入試題參數
    open(113,file="parameter.txt",status="old") 
    do i=1,col
          read(113,*) a(i),b(i),c(i) !三參數
    enddo
    
    !輸入作答反應
    open(111,file="response.txt",status="old") 
    do j=1,row
        read(111,"(20i1)") (u(j,i),i=1,col) !=====需更改讀取數目=====!
    enddo
    
    
    !====================
    !======計算資料======
    !====================
    
    !作答反應輸出 檢驗用
    !do i=1,row
    !	write(*,"(1X,20I2)") (u(i,j),j=1,col) !隨題數變動
    !end do
    
    do q=1,row !10000
    
        !計算作答反應機率
        do i=1,mat
            do j=1,col
                prob(i,j)=probability(theta(i),a(j),b(j),c(j))
                pq(i,j)=prob(i,j)**(u(q,j))*(1-prob(i,j))**(1-u(q,j))
            enddo
            !write(*,"(1X,20F10.4)") (pq(i,j),j=1,col)  !隨題數變動
        enddo
    
        !計算作答反應機率乘機與倒數
        do i=1,mat
            PQmultiply(i)=pq(i,1)
            do j=2,col
                PQmultiply(i)=PQmultiply(i)*pq(i,j)
            enddo 
            posterior(i)=PQmultiply(i)*normalPDF(theta(i))
            !write(*,*) log(posterior(i))
        enddo
        
        EAP_numerator=posterior(1)*theta(1)
        EAP_denominator=posterior(1)
        do i=2,mat
            EAP_numerator=EAP_numerator + posterior(i)*theta(i)
            EAP_denominator=EAP_denominator + posterior(i)
        end do
        
        !根據最大值的位置取出能力估計值
        thetaHAT(q)=EAP_numerator/EAP_denominator
        !write(*,"(I5,F10.6)") q,thetaHAT(q)
        
    end do
    
    !=======================
    !根據所估計的能力估計值進行計算
    !=======================
    
    !估計能力值的平均數
    thetaHATmean=thetaHAT(1)
    do i=2,row !100
        thetaHATmean=thetaHATmean+thetaHAT(i)
    end do
    thetaHATmean=thetaHATmean/row
    !write(*,*) thetaHATmean
    
    !估計能力值的變異數
    thetaHatVariance=(thetaHAT(1)-thetaHATmean)**2
    do i=2,row !100
        thetaHatVariance=thetaHatVariance+(thetaHAT(i)-thetaHATmean)**2
    end do
    thetaHatVariance=thetaHatVariance/row
    !write(*,"(F10.5)") thetaHatVariance
    
    !估計能力值的MSE
    thetaHatMSE=(thetaHAT(1)-thetaTRUE)**2
    do i=2,row !100
        thetaHatMSE=thetaHatMSE+(thetaHAT(i)-thetaTRUE)**2
    end do
    thetaHatMSE=thetaHatMSE/row
    !write(*,"(F10.5)") thetaHatMSE
    
    
    !====================
    !======寫入資料======
    !====================
    open(114,file="summery_EAP_thetaHAT.txt",status="replace")
    write(114,"(A5,A10)") "ID","thetaHAT"
    do q=1,row !10000
        write(114,"(I5,4F10.4)") q,thetaHAT(q)
    end do
    
    open(116,file="summery_EAP_thetaHAT_out.txt",status="replace")
    !write(116,"(A10)") "thetaHAT"
    do q=1,row !10000
        write(116,"(F10.4)") thetaHAT(q)
    end do
    
    call cpu_time (t2)
    open(115,file="summery_EAP_other.txt",status="replace") 
    write (115,"(A10,I5,/,A10,I5,/)") 'Item=',col,'try=',row ! / 為換行
    !write(115,"(6A15)") "Time","Mean","Bias","Variance","MSE"
    !write (115,"(5F15.5)")  t2-t1,thetaHATmean,thetaHATmean-thetaTRUE,thetaHatVariance,thetaHatMSE
    write(115,"(A15,F15.5)") "Time",t2-t1
    write(115,"(A15,F15.5)") "Mean",thetaHATmean
    write(115,"(A15,F15.5)") "Bias",thetaHATmean-thetaTRUE
    write(115,"(A15,F15.5)") "Variance",thetaHatVariance
    write(115,"(A15,F15.5)") "MSE",thetaHatMSE
    
    write(115,"(F15.5)") t2-t1
    write(115,"(F15.5)") thetaHATmean
    write(115,"(F15.5)") thetaHATmean-thetaTRUE
    write(115,"(F15.5)") thetaHatVariance
    write(115,"(F15.5)") thetaHatMSE
    
    end Program EAP 
    
    
    
    ! 	
    ! 3PL probability
    !
    function probability(theta,a,b,c) 
    implicit none
    real::probability
    real::theta,a,b,c
    probability=c+(1-c)/(1+exp(-1.7*a*(theta-b))) !學姊的D為1.71
    return
    end
    
    ! 	
    ! 3PL information
    !
    real function information(theta,a,b,c) 
    implicit none
    real::theta,a,b,c
    real::prob,probMARK
    prob=c+(1-c)/(1+exp(-1.7*a*(theta-b)))
    !probMARK=1.7*a*(1-prob)*(prob-c)/(1-c)
    !information=probMARK**2/(prob*(1-prob))
    information=((1.7**2 * a**2 *(1-prob))/prob)*((prob-c)**2 / (1-c)**2)
    return
    end function information
    
    ! 	
    ! Normal probability density function
    !
    real function normalPDF(x)
    implicit none
    real, parameter:: pi = 3.14159
    real:: x
    real:: mu = 0, sigma = 1  !平均數=0、標準差=1
    normalPDF = (1/((2*pi)**0.5 * sigma))* exp(-(x-mu)**2/(2*sigma**2))
    return
    end function normalPDF