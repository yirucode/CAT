!------------------
!PK值調整
!------------------
subroutine setPK(PS,col,rMAX,length,PK,numPK1)
implicit none
integer:: i,col,length
real::rMAX
real:: PS(col) !選中 Select
real:: PK(col) !所有曝光率參數
integer::numPK1 !PK=1的數目

!由大到小排列、並記錄相對位置 PK
integer::rankN1,rankN2,rankP1,rankP2
real::rankC1,rankC2
real::rank(col) !紀錄PK
integer::place_rank(col) !=(/(i,i=1,col)/)

do i=1,col
    place_rank(i)=i
end do

numPK1=0
do i=1,col
    if( PS(i) > rMAX ) then
        PK(i) = rMAX / PS(i)
    elseif( PS(i) <= rMAX ) then
        PK(i) = 1
        numPK1=numPK1+1
    endif
enddo
!write(*,*) numPK1

!------------------
!如果PK=1不夠時，進行調整
if (numPK1 < length) then

    do i=1,col
        rank(i)=PK(i)
    enddo
    
    !將PK進行排列 找出前幾項PK較大的值 並記錄位置
    do rankN1 = 1, col-1 !length
        do rankN2=1+rankN1,col
            rankC1=rank(rankN1)
            rankC2=rank(rankN2)
            rankP1=place_rank(rankN1)
            rankP2=place_rank(rankN2)
            
            if (rankC1<rankC2) then
                rank(rankN1)=rankC2
                rank(rankN2)=rankC1
                place_rank(rankN1)=rankP2
                place_rank(rankN2)=rankP1
            endif
        enddo
    enddo
    
    do i=1, length !col
        if(rank(i)<1) then
            PK(place_rank(i)) = 1    !調整PK值
        end if
    enddo
    
end if

return
end 

