Subroutine subr_varReal_kind8(x, nvals, var)
    Implicit none
    ! === local variables ===
    integer, save :: i
    real(kind=8), save :: sum_x, ave
    ! === input data ===
    integer, intent(in) :: nvals ! x資料數
    real(kind=8), intent(in), dimension(nvals) :: x
    ! === output data ===
    real(kind=8), intent(out) :: var
    ! === run code ===
    call subr_aveReal_kind8(x, nvals, ave)
    sum_x = 0.
    do i=1, nvals
        sum_x = sum_x + (x(i) - ave) ** 2
    enddo
    var = sum_x / real(nvals)
    return
end subroutine subr_varReal_kind8

! ! === example ===
! program ex0805
!     implicit none
!     integer:: n
!     integer, parameter :: nvals = 5
!     real, dimension(nvals) :: x
!     real :: ave, var
!     ! === run code ===
!     x = (/1,2,8,1,0/)
!     CALL subr_aveReal(x, nvals, ave)
!     CALL subr_varReal(x, nvals, var)
!     WRITE(*,*) 'x=',x
!     WRITE(*,*) 'ave=', ave, 'nvals=', nvals
!     WRITE(*,*) 'var=', var, 'nvals=', nvals
!     n = COUNT(x>0.) + COUNT(x==0.) + COUNT(x<0.)
!     WRITE(*,*) n
! end program ex0805

! Subroutine subr_aveReal(x, nvals, ave)
!     Implicit none
!     ! === local variables ===
!     integer, save :: i
!     real, save :: sum_x
!     ! === input data ===
!     integer, intent(in) :: nvals ! x資料數
!     real, intent(in), dimension(nvals) :: x
!     ! === output data ===
!     real, intent(out) :: ave
!     ! === run code ===
!     sum_x = 0.
!     do i=1, nvals
!         sum_x = sum_x + x(i)
!     enddo
!     ave = sum_x / real(nvals)
!     return
! end subroutine subr_aveReal