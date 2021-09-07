Subroutine subr_contentTargetP(x, nvals, result)
    Implicit none
    ! === local variables ===
    integer, save :: i
    integer, dimension(nvals) :: change
    ! === input data ===
    integer, intent(in) :: nvals ! x資料數
    integer, intent(in), dimension(nvals) :: x
    ! === output data ===
    real, intent(out), dimension(nvals) :: result
    ! === run code ===
    do i=1, nvals
        if (i==1) then
            change(i) = x(i)
        else
            change(i) = change(i-1) + x(i)
        endif
    enddo
    !WRITE(*,*) change
    result = change /real(change(nvals))
    return
end subroutine subr_contentTargetP


! program ex0805
!     implicit none
!     integer, parameter :: nvals = 5
!     integer, dimension(nvals) :: x
!     real, dimension(nvals) :: goal
!     ! === run code ===
!     x = (/1,2,8,1,8/)
!     call subr_contentTargetP(x, nvals, goal)
!     WRITE(*,*) 'x=',x
!     WRITE(*,*) 'goal=', goal
! end program ex0805