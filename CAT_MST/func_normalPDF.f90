!----------------------- 	
! Normal probability density function
!----------------------- 
real function normalPDF(x)
implicit none
real, parameter:: pi = 3.14159
real:: x
real:: mu = 0, sigma = 1  !平均數=0、標準差=1
normalPDF = (1/((2*pi)**0.5 * sigma))* exp(-(x-mu)**2/(2*sigma**2))
return
end function normalPDF