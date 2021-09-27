!-----------------------
! Delta function
!-----------------------

real function func_deltaPsi(try,alpha,psiMax,length) !(item,row,length,num,overlap)
implicit none
! local value
real, external :: combination
! input data
real, intent(in)::psiMax
integer, intent(in)::try
integer, intent(in)::alpha
integer, intent(in)::length
func_deltaPsi = (1-psiMax) * length * combination(try-1, alpha)
return
end