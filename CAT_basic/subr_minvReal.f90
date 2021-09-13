subroutine subr_minvReal(rawData,length,minv,place)
    implicit none 
    ! === local variable ===
    integer, save:: i
    ! === input data: rawData(向量資料), length(向量長度)    
    integer, intent(in):: length
    real, intent(in), dimension(length)::rawData
    ! === output data: minv, place  
    integer, intent(out):: place
    real, intent(out):: minv
    ! === run code ===
    minv=rawData(1) !紀錄最大值
    place=1 !紀錄相對位置
    do i=2,length,1
        if(minv > rawData(i))then
            minv = rawData(i)
            place = i
        else
            minv = minv
            place = place
        end if
    end do
    return
end subroutine subr_minvReal

! ! === example ===
! program main
!     implicit none
!     INTEGER:: i
!     ! === input data ===
!     INTEGER, PARAMETER:: n = 5
!     REAL:: a(n) = (/8,(i,i=2,4),6/)
!     ! === output data ===
!     INTEGER:: minPlace
!     REAL:: minv
!     ! === run data ===
!     CALL subr_minvReal(a,n,minv,minPlace)
!     WRITE(*,*) INT(minv), minPlace
! end program main

