Subroutine subr_sumInt(x, nvals, sum_x)
    Implicit none
    ! === local variables ===
    integer, save :: i
    ! === input data ===
    integer, intent(in) :: nvals ! x資料數
    integer, intent(in), dimension(nvals) :: x
    ! === output data ===
    integer, intent(out) :: sum_x
    ! === run code ===
    sum_x = 0.
    do i=1, nvals
        sum_x = sum_x + x(i)
    enddo
    return
end subroutine subr_sumInt