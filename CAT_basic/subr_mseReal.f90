Subroutine subr_mseReal(x, realv, nvals, mse)
    Implicit none
    ! === local variables ===
    integer, save :: i
    real, save :: sum_x
    ! === input data ===
    integer, intent(in) :: nvals ! x資料數
    real, intent(in), dimension(nvals) :: x
    real, intent(in), dimension(nvals) :: realv
    ! === output data ===
    real, intent(out) :: mse
    ! === run code ===
    sum_x = 0.
    do i=1, nvals
        sum_x = sum_x + (x(i) - realv(i)) ** 2
    enddo
    mse = sum_x / real(nvals)
    return
end subroutine subr_mseReal

! ! === example ===
! program ex0805
!     implicit none
!     integer:: n
!     integer, parameter :: nvals = 5
!     real, dimension(nvals) :: x, realv
!     real :: mse
!     ! === run code ===
!     x = (/1,2,8,1,0/)
!     realv = 0
!     CALL subr_mseReal(x, realv ,nvals, mse)
!     WRITE(*,*) 'x=',x
!     WRITE(*,*) 'realv=',realv
!     WRITE(*,*) 'mse=', mse, 'nvals=', nvals
!     n = COUNT(x>0.) + COUNT(x==0.) + COUNT(x<0.)
!     WRITE(*,*) n
! end program ex0805