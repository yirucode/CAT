Subroutine subr_contentCount(x, nvals, target, result)
    Implicit none
    ! === local variables ===
    integer, save :: i
    ! === input data ===
    integer, intent(in) :: nvals ! x資料數
    integer, intent(in) :: target ! 要算的數值
    integer, intent(in), dimension(nvals) :: x
    ! === output data ===
    integer, intent(out) :: result
    ! === run code ===
    result = 0
    do i=1, nvals
        if ( x(i) == target ) then
            result = result +1
        endif
    enddo
    return
end subroutine subr_contentCount

! program ex
!     implicit none
!     INTEGER:: i, j
!     ! === input data ===
!     INTEGER, PARAMETER:: numTest = 100 ! 施測次數、人數
!     INTEGER, PARAMETER:: numContentType = 3
!     INTEGER, PARAMETER:: length = 40 ! 題長
!     integer, dimension(length, numTest):: contentChoose 
!     ! === data path ===
!     INTEGER :: status
!     INTEGER :: nJump = 2 ! 讀取資料時要跳過的行數
!     character(len = 50), parameter :: dataPath = "ListCAT_content.txt"
!     character(len = 20), parameter :: dataLength = '(50I10)' ! 隨著 pool item number 改變而改變
!     ! === output data ===
!     integer, dimension(numContentType, numTest)::contentResult
!     ! === run data ===
!     ! read data
!     open(100, file= dataPath, status="old", action = 'read', iostat = status)
!     WRITE(*,*) status
!     do i = 1, nJump
!         read(100,*) !// 跳過第一、二行
!     enddo
!     do i=1, numTest
!         read(100, fmt = dataLength, iostat = status) (contentChoose (j,i), j=1,length)
!     enddo
!     close(100)
!     ! run the subroutine
!     do i=1, numTest
!         do j=1,numContentType
!             call subr_contentCount(contentChoose(:,i),length,j,contentResult(j,i))
!         enddo
!     enddo
!     WRITE(*,*) contentResult
!     stop
! end program ex
