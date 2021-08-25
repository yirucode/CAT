Subroutine subr_sumReal(x, nvals, sum_x)
    Implicit none
    ! === local variables ===
    integer, save :: i
    ! === input data ===
    integer, intent(in) :: nvals ! x資料數
    real, intent(in), dimension(nvals) :: x
    ! === output data ===
    real, intent(out) :: sum_x
    ! === run code ===
    sum_x = 0.
    do i=1, nvals
        sum_x = sum_x + x(i)
    enddo
    return
end subroutine subr_sumReal