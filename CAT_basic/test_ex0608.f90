Program ex0608
    implicit none
    integer :: i, j
    loop1: do i = 1,3
        loop2:do j = 1,3
            if(j == 2) cycle loop2
            if(i == 2) exit loop1
            write(*,"(t2,A1,I3,A1,I3,A1)") '(',i,',',j,')'
        end do loop2
    end do loop1
    stop
end program ex0608