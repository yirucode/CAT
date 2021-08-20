!----------------------- 
! generating responses 
!----------------------- 
subroutine response(theta,a,b,c,resp,randv) 
    real::a,b,c
    real::theta,prob,randv 
    integer::resp
    
    prob=c+(1-c)/(1+exp(-1.7*a*(theta-b)))
    call random_number(randv) 
    
    if (prob>randv) then   
        resp=1 
    else   
        resp=0 
    endif 
    
    return
    end 
    