!------------------
!試題重疊率計算
!------------------
subroutine subr_testOverlap(item, numTest, length, overlap)
    implicit none
    ! numTest太大，或length太長，都會超出記憶體規格，最後跑不出結果。
    ! 剛開始會較慢，後面計算會加快!
    ! === output ===
    real, intent(out)::overlap ! real(kind=8), intent(out)::overlap 
    ! === input data ===
    integer, intent(in):: numTest !人數，切割item用
    integer, intent(in):: length !題長，切割item用
    integer, intent(in), dimension(length, numTest):: item
    ! === local variable ===
    integer, save:: i,j,p,q
    real, dimension(numTest)::sumUsed
    !real, save::sumv = 0.
    !real, save::Numerator !分子
    real, save::Denominator !分母
    real, external:: combination
    ! === run code ===    
    Denominator = combination(numTest,2)*length ! combination(numTest,2)*length
    do i=1,numTest
        if ( i==numTest ) then ! 最後一次測驗沒有比較的對象，所以忽略不計
            sumUsed(i) = 0.
            ! WRITE(*,*) sumUsed(i)
            exit
        endif
        do j=(i+1),numTest 
            do p=1,length
                do q=1,length
                    if ( item(p,i) == item(q,j) ) then
                    sumUsed(i) = sumUsed(i) + 1.
                    exit
                    endif
                end do
            enddo
        enddo
        sumUsed(i) = sumUsed(i)/Denominator
        ! WRITE(*,*) i,sumUsed(i)
    enddo
    call subr_sumReal(sumUsed, numTest, overlap)
    ! Numerator = sumv
    ! overlap = Numerator/Denominator
    return
end subroutine subr_testOverlap


! ! === example ===
! program ex
!     implicit none
!     INTEGER:: i, j
!     ! === input data ===
!     INTEGER, PARAMETER:: numTest = 10000 ! 施測次數
!     INTEGER, PARAMETER:: length = 100 ! 題長
!     integer, dimension(length, numTest):: itemChoosed
!     ! === data path ===
!     INTEGER :: status
!     INTEGER :: nJump = 2 ! 讀取資料時要跳過的行數
!     character(len = 50), parameter :: dataPath = "ListCAT_item.txt"
!     character(len = 20), parameter :: dataPool = '(500I10)' ! 隨著 pool item number 改變而改變
!     ! === output data ===
!     real :: overlap ! real(kind=8) :: overlap
!     ! === run data ===
!     ! read data
!     open(100, file= dataPath, status="old", action = 'read', iostat = status)
!     WRITE(*,*) status
!     do i = 1, nJump
!         read(100,*) !// 跳過第一、二行
!     enddo
!     do i=1, numTest
!         read(100, fmt = dataPool, iostat = status) (itemChoosed(j,i), j=1,length)
!     enddo
!     close(100)
!     CALL subr_testOverlap(itemChoosed, numTest, length, overlap)
!     WRITE(*,*) overlap
!     stop
! end program ex

! !-----------------------
! ! combination 組合
! !-----------------------
! function combination(n,k)
!     ! === output ===
!     real:: combination
!     ! === input data ===
!     integer,intent(in)::n, k ! 用於計算n取k的組合數
!     ! === local variable ===
!     integer,save::i
!     real::Numerator !分子
!     real::Denominator !分母
!     ! === run code ===
!     if(n<k)then
!         combination=0
!     else
!         Numerator = 1
!         Denominator = 1
!         do i=1,k
!             Numerator=Numerator*(n+1-i)
!             Denominator=Denominator*i
!         enddo
!         combination = Numerator/Denominator
!     endif
!     return
!     end function combination

!     Subroutine subr_sumReal(x, nvals, sum_x)
!         Implicit none
!         ! === local variables ===
!         integer, save :: i
!         ! === input data ===
!         integer, intent(in) :: nvals ! x資料數
!         real, intent(in), dimension(nvals) :: x
!         ! === output data ===
!         real, intent(out) :: sum_x
!         ! === run code ===
!         sum_x = 0.
!         do i=1, nvals
!             sum_x = sum_x + x(i)
!         enddo
!         return
!     end subroutine subr_sumReal