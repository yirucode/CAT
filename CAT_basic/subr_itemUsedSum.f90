subroutine subr_itemUsedSum(poolUsed,numTest,numPool,usedSum)
    implicit none 
    ! === local variable ===
    integer, save:: i
    ! === input data: rawData(向量資料), length(向量長度)    
    integer, intent(in):: numTest, numPool 
    integer, intent(in), dimension(numPool, numTest)::poolUsed
    ! === output data ===  
    integer, intent(out) ,dimension(numPool):: usedSum
    ! === run code ===
    usedSum = 0 ! 設定初始值
    do i = 1, numPool
        call subr_sumInt(poolUsed(i ,:), numTest, usedSum(i))
    enddo
    return
end subroutine subr_itemUsedSum

! ! === example ===
! program ex
!     implicit none
!     INTEGER:: i, j
!     ! === input data ===
!     INTEGER, PARAMETER:: numTest = 100 ! 施測次數
!     INTEGER, PARAMETER:: numPool = 300 ! 題庫
!     integer, dimension(numPool, numTest):: poolUsed = 99
!     ! === data path ===
!     INTEGER :: status
!     INTEGER :: nJump = 2 ! 讀取資料時要跳過的行數
!     character(len = 50), parameter :: dataPath = "ListCAT_poolUsed.txt"
!     character(len = 20), parameter :: dataPool = '(500I10)' ! 隨著 pool item number 改變而改變
!     ! === output data ===
!     integer :: sum_x
!     integer, dimension(numPool):: usedSum
!     ! === run data ===
!     ! read data
!     open(100, file= dataPath, status="old", action = 'read', iostat = status)
!     WRITE(*,*) status
!     do i = 1, nJump
!         read(100,*) !// 跳過第一、二行
!     enddo
!     do i=1, numTest
!         read(100, fmt = dataPool, iostat = status) (poolUsed(j,i), j=1,numPool)
!     enddo
!     close(100)
!     CALL subr_itemUsedSum(poolUsed,numTest,numPool,usedSum)
!     WRITE(*,*) (usedSum(i), i=1, numPool)
!     CALL subr_sumInt(usedSum, numPool, sum_x)
!     WRITE(*,*) sum_x
!     stop
! end program ex


! Subroutine subr_sumInt(x, nvals, sum_x)
!     Implicit none
!     ! === local variables ===
!     integer, save :: i
!     ! === input data ===
!     integer, intent(in) :: nvals ! x資料數
!     integer, intent(in), dimension(nvals) :: x
!     ! === output data ===
!     integer, intent(out) :: sum_x
!     ! === run code ===
!     sum_x = 0.
!     do i=1, nvals
!         sum_x = sum_x + x(i)
!     enddo
!     return
! end subroutine subr_sumInt
