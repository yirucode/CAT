Program ex0431
    implicit none
    integer :: A = 2, B = 1
    real :: C, D
    C = B / A 
    D = real(B) / real(A)
    write(*,*) C ! → 0.00000
    write(*,*) D ! → 0.50000
    stop
end program ex0431