subroutine calc_hypotenuse( side_1, side_2, hypotenuse )
    implicit none
    real, intent(in) :: side_1, side_2
    real, intent(out) :: hypotenuse
    real :: temp
    temp = side_1 ** 2 + side_2 ** 2
    hypotenuse = sqrt(temp)
    return
end subroutine

program test_hypotenuse
    implicit none
    real :: S1, S2
    real :: hypot
    write(*,*) 'Program to test subroutine calc_hypotenuse:'
    write(*,*) 'Enter the hength of side 1'
    read(*,*) S1
    write(*,*) 'Enter the hength of side 2'
    read(*,*) S2
    call calc_hypotenuse(S1, S2, hypot)
    write (*,10) hypot
    10 Format(1X, 'The length of the hypotenuse is ', F10.4)
    stop
end program test_hypotenuse





