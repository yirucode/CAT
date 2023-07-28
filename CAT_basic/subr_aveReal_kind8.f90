Subroutine subr_aveReal_kind8(x, nvals, ave)
    Implicit none
    ! === local variables ===
    integer, save :: i
    real(kind=8), save :: sum_x
    ! === input data ===
    integer, intent(in) :: nvals ! x資料數
    real(kind=8), intent(in), dimension(nvals) :: x
    ! === output data ===
    real(kind=8), intent(out) :: ave
    ! === run code ===
    sum_x = 0.
    do i=1, nvals
        sum_x = sum_x + x(i)
    enddo
    ave = sum_x / real(nvals)
    return
end subroutine subr_aveReal_kind8

! program ex0805
!     implicit none
!     integer:: n
!     integer, parameter :: nvals = 5
!     real, dimension(nvals) :: x
!     real :: ave
!     ! === run code ===
!     x = (/1,2,8,1,0/)
!     CALL subr_aveReal(x, nvals, ave)
!     WRITE(*,*) 'x=',x
!     WRITE(*,*) 'ave=', ave, 'nvals=', nvals
!     n = COUNT(x>0.) + COUNT(x==0.) + COUNT(x<0.)
!     WRITE(*,*) n
! end program ex0805