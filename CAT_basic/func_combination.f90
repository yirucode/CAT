!-----------------------
! combination 組合
!-----------------------
function combination(n,k)
! === output ===
real:: combination
! === input data ===
integer,intent(in)::n, k ! 用於計算n取k的組合數
! === local variable ===
integer,save::i
real::Numerator !分子
real::Denominator !分母
! === run code ===
if(n<k)then
    combination=0
else if(n==k) then !為了加速，免去下面程式運算
    combination = 1
else if((n>k).AND.(k==0)) then
    combination = 1
else if((n>k).AND.(k>0)) then
    Numerator = 1
    Denominator = 1
    do i=1,k
        Numerator=Numerator*(n+1-i)
        Denominator=Denominator*i
    enddo
    combination = Numerator/Denominator
else
    print*, "error"
    stop
endif
return
end function combination

! ! === example ===
! program ex
!     implicit none
!     integer::n, k
!     real::outv
!     real,external:: combination
!     n = 10000
!     k = 2
!     outv = combination(n,k)
!     WRITE(*,*) outv
! end program ex

