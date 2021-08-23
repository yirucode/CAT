subroutine subr_maxvInt(rawData,length,maxv,place)
    implicit none 
    ! === local variable ===
    integer, save:: i
    ! === input data: rawData(向量資料), length(向量長度)
    integer, intent(in):: length
    integer, intent(in), dimension(length)::rawData
    ! === output data: maxv, place  
    integer, intent(out):: place
    integer, intent(out):: maxv
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
!     ! === input data
!     INTEGER, PARAMETER:: n = 5
!     INTEGER:: a(n) = (/1,(i,i=2,4),6/)
!     ! === output data
!     INTEGER:: maxPlace
!     INTEGER:: maxvc
!     ! === 
!     CALL subr_maxvInt(a,n,maxvc,maxPlace)
!     WRITE(*,*) maxvc, maxPlace
! end program main

