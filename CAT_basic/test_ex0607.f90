Program ex0607
    implicit none
    integer :: i, j
    outer: do i = 1, 5
        inner: do j = 1, 5
            write(*, "(t2,A1,I3,A1,I3,A1)") '(', i, ',', j,')'
        end do inner
    end do outer
    stop
end program ex0607