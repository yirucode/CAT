Program ex0605
    implicit none
    integer :: i
    integer, parameter :: n=10
    do i = 1, n
        if (i == 4) cycle ! 符合條件時，則重頭執行
        write(*,'(I3)') I

    end do
    stop
end program ex0605