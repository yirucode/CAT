!----------------------- 	
! 3PL probability
!----------------------- 
function probability(theta,a,b,c) 
    implicit none
    ! === output data ===
    real::probability
    ! === input data ===
    real, intent(in)::theta,a,b,c
    ! === run code ===
    probability=c+(1-c)/(1+exp(-1.7*a*(theta-b))) !學姊的D為1.71
    return
end
    