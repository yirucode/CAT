program main
    implicit none
    INTEGER:: i
    ! === input data
    INTEGER, PARAMETER:: n = 5
    REAL:: a(n) = (/8,(i,i=2,4),6/)
    ! === output data
    INTEGER:: maxPlace
    REAL:: maxvc
    ! === 
    CALL subr_maxvReal(a,n,maxvc,maxPlace)
    WRITE(*,*) INT(maxvc), maxPlace
end program main

