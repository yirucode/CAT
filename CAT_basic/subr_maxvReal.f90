subroutine subr_maxvReal(rawData,length,maxv,place)
    implicit none 
    ! === local variable ===
    integer, save:: i
    ! === input data: rawData(向量資料), length(向量長度)    
    integer, intent(in):: length
    real, intent(in), dimension(length)::rawData
    ! === output data: maxv, place  
    integer, intent(out):: place
    real, intent(out):: maxv
    ! === run code ===
    maxv=rawData(1) !紀錄最大值
    place=1 !紀錄相對位置
    do i=2,length,1
        if(maxv < rawData(i))then
            maxv = rawData(i)
            place = i
        else
            maxv = maxv
            place = place
        end if
    end do
    return
end

! ! === example ===
! program main
!     implicit none
!     INTEGER:: i
!     ! === input data ===
!     INTEGER, PARAMETER:: n = 5
!     REAL:: a(n) = (/8,(i,i=2,4),1/)
!     ! === output data ===
!     INTEGER:: maxPlace
!     REAL:: maxvc
!     ! === run data ===
!     CALL subr_maxvReal(a(2:n),n-1,maxvc,maxPlace)
!     WRITE(*,*) INT(maxvc), maxPlace
! end program main

