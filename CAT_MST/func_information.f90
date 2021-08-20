!----------------------- 	
! 3PL information
!----------------------- 
real function information(theta,a,b,c) 
implicit none
real::theta,a,b,c
real::prob !,probMARK
prob=c+(1-c)/(1+exp(-1.7*a*(theta-b)))
!probMARK=1.7*a*(1-prob)*(prob-c)/(1-c)
!information=probMARK**2/(prob*(1-prob))
information=((1.7**2 * a**2 *(1-prob))/prob)*((prob-c)**2 / (1-c)**2)
return
end function information