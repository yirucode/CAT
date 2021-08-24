!------------------
!EAP能力估計
!------------------
subroutine subr_EAP(length,a,b,c,resp,thetaHAT)
    implicit none
    ! === output data ===
    real, intent(out)::thetaHAT !估計能力值
    ! === input data ===
    integer, intent(in)::length ! 總人數；作答題長
    integer, intent(in)::resp(length) !作答反應
    real, intent(in)::a(length),b(length),c(length) !選題的試題參數
    ! === function ===
    real, external:: probability, normalPDF
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
    allocate(prob(length, mat), pq(length, mat)) !重新定義的資料格數
    allocate(thetaCUT(mat), PQmultiply(mat), posterior(mat)) !定義資料格數
    thetaCUT(1)=-3 !起始能力
    do i=2,mat
        thetaCUT(i)=thetaCUT(i-1)+seq !算出能力值
    enddo
    ! write(*,*) (thetaCUT(i),i=1,mat)
    !計算作答反應機率
    do i=1,mat
        do j=1, length
            prob(j, i)=probability(thetaCUT(i),a(j),b(j),c(j))
            pq(j, i)=prob(j, i)**resp(j)*(1-prob(j, i))**(1-resp(j)) !隨題數變動
        enddo
    enddo
    !計算作答反應機率乘機與倒數
    
    if(length==1)then
        do i=1, mat
            PQmultiply(i) = pq(1, i)
            posterior(i) = PQmultiply(i)*normalPDF(thetaCUT(i))
        end do
    else
        do i=1, mat
            PQmultiply(i) = pq(1, i)
            do j=2,length
                PQmultiply(i)=PQmultiply(i)*pq(j,i)
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
    deallocate(prob,pq,thetaCUT,PQmultiply,posterior) !取消定義的資料格數
    return
end subroutine subr_EAP


! ! === example ===
! program ex
!     implicit none
!     ! === 輸出格式 ===
!     99 format(1X, A, 100I10 ) ! 與length一同修改
!     10 format(1X, A, 100F5.2) ! 與length一同修改
!     ! === given data ===
!     integer, parameter :: row = 2 , length = 100
!     real :: a(length,row), b(length,row), c(length,row) !選題的試題參數
! 	! === output data ===
!     integer :: resp(length,row) !作答反應
!     real::thetaHAT(row) !估計能力值  
!     ! === changing data ===
!     integer :: i, k
!     integer :: try
!     real :: sumv, randv(length)
! 	! === unknown data ===
!     a = 1
!     call random_number(randv)
!     b(:,1) = randv(:)*3
!     b(:,2) = randv(:)*1
!     c = 0
!     resp (1:length,1) = 0
!     resp (1:length,2) = 1
!     WRITE(*,*) resp(:,1)
!     WRITE(*,99) 'resp=',resp(:,1)


    
!     sumv = sum( (/b(:,1)/) )
!     sumv = sum( (/b(:,2)/) )
!     WRITE(*,*) 'sumB=',sumv

!     try = 2
!     WRITE(*,10) 'a=',(a(i,try),i=1,length)
!     WRITE(*,10) 'b=',(b(i,try),i=1,length) 
!     WRITE(*,10) 'c=',(c(i,try),i=1,length)    
!     WRITE(*,99) 'resp=',(resp(i,try),i=1,length)
!     call subr_EAP(length,a(:,try),b(:,try),c(:,try),resp(:,try),thetaHAT(try))
!     do try = 1, row
!         call subr_EAP(length,a(:,try),b(:,try),c(:,try),resp(:,try),thetaHAT(try))
!     enddo

!     WRITE(*,*) 'thetaHat=',thetaHAT

!     k = 40 ! 只針對前k題算EAP 
!     do try = 1, row
!         call subr_EAP(k,a(1:k,try),b(1:k,try),c(1:k,try),resp(1:k,try),thetaHAT(try))
!     enddo

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



