!----------------------- 
! generating responses 
!----------------------- 
subroutine subr_resp(theta,a,b,c,resp,randv) 
    implicit none
    ! === local variable ===
    real, save::prob
    ! === input data ===
    real, intent(in)::a,b,c
    real, intent(in)::theta
    ! === output ===
    integer, intent(out)::resp
    real, intent(out)::randv
    ! === run code ===
    prob=c+(1-c)/(1+exp(-1.7*a*(theta-b)))
    call random_number(randv) 
    if (prob>randv) then   
        resp=1 
    else   
        resp=0 
    endif 
    return
end subroutine subr_resp

! ! === example ===
! program test
!     implicit none
!     real::a,b,c
!     real::theta = 1.
!     ! === unknow data ===
!     integer::resp
!     real::randv
!     ! === run code ===
!     a = 1
!     b = 2.2
!     c = 0.5
!     call subr_resp(theta,a,b,c,resp,randv)
!     WRITE(*,10) theta,a,b,c,resp,randv
!     10 Format(1X,'theta = ',F5.1,2X,'a,b,c =',3F5.1,/,&
!     1X, 'resp = ',I2 ,2X ,'random value =', F5.3)

!     call subr_resp(theta,a,b,c,resp,randv)
!     WRITE(*,10) theta,a,b,c,resp,randv
!     call subr_resp(theta,a,b,c,resp,randv)
!     WRITE(*,10) theta,a,b,c,resp,randv
!     call subr_resp(theta,a,b,c,resp,randv)
!     WRITE(*,10) theta,a,b,c,resp,randv
! end program test