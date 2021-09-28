!-----------------------
! Delta function
!-----------------------

real function func_deltaOmega(try,alpha,omegaMax,omegaPrevious,length) !(item,row,length,num,overlap)
implicit none
! local value
real, external :: combination
! input data
real, intent(in)::omegaMax, omegaPrevious
integer, intent(in)::try
integer, intent(in)::alpha
integer, intent(in)::length
func_deltaOmega = length * combination(try, alpha+1) * &
(((try-alpha-1)/try) * omegaPrevious + ((alpha+1)/try) - omegaMax) 
return
end