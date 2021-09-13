    !-----------------------
    ! Omega subroutine
    !-----------------------
    subroutine subr_testOmega(numTest,numPool,itemUsed,length,try,alpha,Omega)
        implicit none
        ! === output ===
        real, intent(out), dimension(numTest)::Omega
        ! === input data ===
        integer, intent(in) ::numTest, numPool
        integer, intent(in) ::length, try
        integer, intent(in) ::alpha
        integer, intent(in), dimension(numTest,numPool)::itemUsed !numPool = 題庫數、numTest = 人數
        ! === local variable ===
        real, external :: combination
        integer, save::i !計算用
        real, save::sumv !計算用
        ! === run code ===
        sumv=0
        do i=1,numPool
            sumv = sumv + (itemUsed(try,i)-itemUsed(try-1,i)) * combination(try-itemUsed(try,i),alpha)
        enddo
        if (try<=alpha)then
            Omega(try)=0
        else
            Omega(try) = REAL(try-alpha-1)/try * Omega(try-1) + REAL(alpha+1)/try &
            - sumv / (length * combination(try,alpha+1))
        endif
        return
    end subroutine subr_testOmega

    ! === example ===
program ex
    implicit none
    INTEGER:: i, j
    ! === input data ===
    INTEGER, PARAMETER:: numTest = 100 ! 施測次數
    INTEGER, PARAMETER:: numPool = 300 ! 題庫
    integer, dimension(numPool, numTest):: itemUsed !numPool = 題庫數、numTest = 人數
    integer::length = 40
    integer::try = 100
    integer::alpha = 1
    ! === data path ===
    INTEGER :: status
    INTEGER :: nJump = 2 ! 讀取資料時要跳過的行數
    character(len = 50), parameter :: dataPath = "ListCAT_poolUsedSum.txt"
    character(len = 20), parameter :: dataPool = '(300I10)' ! 隨著 pool item number 改變而改變
    ! === output data ===
    real, dimension(numTest)::Omega
    ! === run data ===
    ! read data
    open(100, file= dataPath, status="old", action = 'read', iostat = status)
    WRITE(*,*) status
    do i = 1, nJump
        read(100,*) !// 跳過第一、二行
    enddo
    do i=1, numTest
        read(100, fmt = dataPool, iostat = status) (itemUsed(j,i), j=1,numPool)
    enddo
    close(100)
    ! run the subroutine
    CALL subr_testOmega(numTest,numPool,itemUsed,length,try,alpha,Omega) 
    do i=1,numTest
        WRITE(*,*) Omega(i)
    enddo
    stop
end program ex

!-----------------------
! combination 組合
!-----------------------
function combination(n,k)
    ! === output ===
    real:: combination
    ! === input data ===
    integer,intent(in)::n, k ! 用於計算n取k的組合數
    ! === local variable ===
    integer,save::i
    real::Numerator !分子
    real::Denominator !分母
    ! === run code ===
    if(n<k)then
        combination=0
    else
        Numerator = 1
        Denominator = 1
        do i=1,k
            Numerator=Numerator*(n+1-i)
            Denominator=Denominator*i
        enddo
        combination = Numerator/Denominator
    endif
    return
    end function combination