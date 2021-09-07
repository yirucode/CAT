program test
    implicit none
    real:: x(3) = (/14,13,13/)
    real:: k(3)
    k = x(:)/10
    write(*,*) k
end program test