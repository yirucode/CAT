program test_random_seed
    implicit none
    real :: u
    INTEGER:: m,n,j
    m = 3
    n = 5
    call random_number(u)
    j = n + FLOOR((m+1-n)*u)  ! We want to choose one from m-n+1 integers
    print*, j
end program test_random_seed