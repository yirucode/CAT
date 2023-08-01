!----------------------- 	
! Normal probability density function
!----------------------- 
function normalPDF(x)
    implicit none
    ! === output ===
    real:: normalPDF
    ! === input data ===
    real, intent(in):: x
    ! === local variable ===
    real, save:: pi = 3.14159
    real, save:: mu = 0.0, sigma = 1.0  !平均數=0、標準差=1
    ! === run code ===
    normalPDF = (1/((2*pi)**0.5 * sigma))* exp(-(x-mu)**2/(2*sigma**2))
    return
end function normalPDF

! ! === example ===
! program test
!     implicit none
!     real:: x, outv
!     real,external:: normalPDF !設定要用的函數
!     x = 0.0
!     outv = normalPDF(x)
!     WRITE(*,*) outv
! end program test

