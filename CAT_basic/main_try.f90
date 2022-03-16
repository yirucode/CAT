program test_random_seed
    implicit none
    real :: u(5)
    INTEGER:: m,n
    m = 1
    n = 5
    call random_number(u)
    !j = NINT((m+1-n)*u)  ! We want to choose one from m-n+1 integers
    print*, u
    !print*, 1+FLOOR(5*u)
    !print*, n + NINT((m-n)*u)
end program test_random_seed