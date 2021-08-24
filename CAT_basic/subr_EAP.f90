!------------------
!EAP能力估計
!------------------
subroutine EAP(length,a,b,c,resp,thetaHAT)
    implicit none
    ! === output data ===
    real, intent(out)::thetaHAT !估計能力值
    ! === input data ===
    integer, intent(in)::length ! 總人數；作答題長
    integer, intent(in)::resp(length) !作答反應
    real, intent(in)::a(length),b(length),c(length) !選題的試題參數
    ! === function ===
    real, external:: probability, information, normalPDF
    ! === local variable ===
    integer, save::i,j
    ! EAP使用參數
    integer, save :: mat !根據估計間隔所切割的資料數
    real, save :: seq = 0.1 !估計間隔
    real, allocatable, save ::thetaCUT(:),PQmultiply(:),posterior(:) !切割能力值、機率乘積、後驗分配
    real, allocatable, save ::prob(:,:),pq(:,:)
    real, save:: EAP_numerator !EAP分子
    real, save:: EAP_denominator !EAP分母
    ! === run code ===
    !EAP初始設定
    mat = INT((3-(-3))/seq+1) +1 
    !deallocate(prob,pq) !取消定義的資料格數
    allocate(prob(mat,length),pq(mat,length)) !重新定義的資料格數
    allocate(thetaCUT(mat),PQmultiply(mat),posterior(mat)) !定義資料格數
    thetaCUT(1)=-3 !起始能力
    do i=2,mat
        thetaCUT(i)=thetaCUT(i-1)+seq !算出能力值
    enddo
    ! write(*,*) (thetaCUT(i),i=1,mat)
    !計算作答反應機率
    do i=1,mat
        do j=1, length
            prob(i, j)=probability(thetaCUT(i),a(j),b(j),c(j))
            pq(i, j)=prob(i, j)**(resp(j))*(1-prob(i, j))**(1-resp(j)) !隨題數變動
        enddo
    enddo
    !計算作答反應機率乘機與倒數
    if(length==1)then
        do i=1,mat
            PQmultiply(i) = pq(i, 1)
            posterior(i) = PQmultiply(i)*normalPDF(thetaCUT(i))
        end do
    else
        do i=1,mat
            PQmultiply(i)=pq(i, 1)
            do j=2,length
                PQmultiply(i)=PQmultiply(i)*pq(i,j)
            enddo
            posterior(i)=PQmultiply(i)*normalPDF(thetaCUT(i))
        end do
    end if
    ! write(*,*) (log(posterior(i)),i=1,mat)
    EAP_numerator=posterior(1)*thetaCUT(1)
    EAP_denominator=posterior(1)
    do i=2,mat
        EAP_numerator=EAP_numerator + posterior(i)*thetaCUT(i)
        EAP_denominator=EAP_denominator + posterior(i)
    end do

    thetaHAT=EAP_numerator/EAP_denominator !計算出能力估計值
    return
end subroutine EAP


! ! === example ===
! program ex
!     implicit none
!     ! === 輸出格式 ===
!     9 Format(1X, A5, 40I5) ! 與length一同修改
!     10 Format(1X, A5, 40F5.2) ! 與length一同修改
!     ! === given data ===
!     integer, parameter :: row = 1 , length = 40
!     real :: a(row,length), b(row,length), c(row,length) !選題的試題參數
! 	! === output data ===
!     integer :: resp(row,length) !作答反應
!     real::thetaHAT(row) !估計能力值  
!     ! === changing data ===
!     integer :: i, k
!     integer :: try
!     real :: sumv, randv(length)
! 	! === unknown data ===
!     a = 1
!     call random_number(randv)
!     b(1,:) = randv(:)*3
!     c = 0
!     resp (row,:) = 0
!     WRITE(*,10) 'a=',(a(1,i),i=1,length)
!     WRITE(*,10) 'b=',(b(1,i),i=1,length) 
!     WRITE(*,10) 'c=',(c(1,i),i=1,length)    
!     WRITE(*,9) 'resp=',resp
!     sumv = sum( (/b(1,:)/) )
!     WRITE(*,*) 'sumB=',sumv
!     k = 5 ! 只針對前5題算EAP 
!     try = 1 ! 針對第一位受試者
!     call EAP(k,a(try,1:k),b(try,1:k),c(try,1:k),resp(try,1:k),thetaHAT(try))
!     ! call EAP(length,a,b,c,resp,thetaHAT(1))
!     WRITE(*,*) 'thetaHat=',thetaHAT
    
! end program ex
    

! !-----------------------     
! ! 3PL probability
! !----------------------- 
! function probability(theta,a,b,c) 
!     implicit none
!     ! === output data ===
!     real::probability
!     ! === input data ===
!     real, intent(in)::theta,a,b,c
!     ! === run code ===
!     probability=c+(1-c)/(1+exp(-1.7*a*(theta-b))) !學姊的D為1.71
!     return
! end


! !-----------------------     
! ! 3PL information
! !----------------------- 
! function information(theta,a,b,c) 
!     implicit none
!     ! === output data ===
!     real :: information
!     ! === local variable ===
!     real, save :: prob !,probMARK
!     ! === input data ===
!     real, intent(in) :: theta,a,b,c
!     ! === run code ===
!     prob=c+(1-c)/(1+exp(-1.7*a*(theta-b)))
!     !probMARK=1.7*a*(1-prob)*(prob-c)/(1-c)
!     !information=probMARK**2/(prob*(1-prob))
!     information=((1.7**2 * a**2 *(1-prob))/prob)*((prob-c)**2 / (1-c)**2)
!     return
    
! end function information


! !-----------------------     
! ! Normal probability density function
! !----------------------- 
! function normalPDF(x)
!     implicit none
!     ! === output ===
!     real:: normalPDF
!     ! === input data ===
!     real, intent(in):: x
!     ! === local variable ===
!     real, save:: pi = 3.14159
!     real, save:: mu = 0, sigma = 1  !平均數=0、標準差=1
!     ! === run code ===
!     normalPDF = (1/((2*pi)**0.5 * sigma))* exp(-(x-mu)**2/(2*sigma**2))
!     return
! end function normalPDF



