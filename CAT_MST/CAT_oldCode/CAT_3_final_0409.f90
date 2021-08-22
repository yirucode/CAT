
!!!!!Declaration Section 宣告區
!!!!!(weitingc, 2021/03/15)
!!!!!每次都從module pool中選出
program CAT
implicit none
integer,parameter :: length=40	!題長
integer,parameter :: col=300	!題庫數
integer,parameter :: row=10000	!嘗試人數

integer,parameter :: stage=2	!
integer,parameter :: n_level=3	!level數
integer,parameter :: n_module=5	!level內module數
integer,parameter :: n_item=20	!module內題數

integer :: id_num(col), level(col), content(col), modulev(col)
real :: a(col),b(col),c(col)
real::a_choose(row,length),b_choose(row,length),c_choose(row,length) !選題的試題參數

real::infor(col) 
!integer::place_infor(col)
real::randv

integer :: used(col)=99
real::theta_begin(row)=0
real::theta_hat(row,length)=99
integer::resp(row,length)=99
!integer::id_choose(row,length)
integer:: place_choose(row,length) !選題的試題位置

!integer :: countA(col)=0, poolused(col)=0

integer :: i,j,try,choose,stage_n,level_go,k
real::maxv, sumv !最大值, 總和
integer::place !位置

real (kind=8):: t1 !開始時間
real (kind=8):: t2 !結束時間

real,external:: probability,information !函數

!計算最後總值所用參數
integer::m(row,col) !item usage row=作答人數、col=題庫數
real::OmegaGA1(row),OmegaGA2(row),OmegaGA3(row)
real::OmegaGA1_Mean,OmegaGA2_Mean,OmegaGA3_Mean
real::OmegaGA1_Variance,OmegaGA2_Variance,OmegaGA3_Variance
real::OmegaGA1_Max,OmegaGA2_Max,OmegaGA3_Max
real::OmegaGA1_Min,OmegaGA2_Min,OmegaGA3_Min

!計算最後總值所用參數-2
real::thetaTRUEmean=1 !真實能力值
real::thetaTRUE(row)=(/(1,i=1,row)/) !真實能力值
real::thetaHatVariance !估計能力值的變異數
real::thetaHATmean !估計能力值的平均數
real::thetaHatMSE !估計能力值的MSE

!------------------
!題庫試題選中與使用次數
!integer:: countS(col)=(/(0,i=1,col)/) !選中
integer:: countA(col)=0 !(/(0,i=1,col)/) !使用
integer::countPOOL(col)=(/(0,i=1,col)/) !題庫使用
real:: PA(col)=(/(0,i=1,col)/) !使用 Administer
real:: maxPA
integer::place_maxPA
integer::sumA !檢驗用
integer::sumPOOL !檢驗用
real::PoolUR !題庫使用率
real::PAvariance
real::PAmean
real(kind=8):: overlap !試題重疊率設定


!!!!!Execution Section 執行區
call cpu_time (t1) !開始計時

open(100,file="data/parameter_300.txt",status="old") 
do i=1,col
	read(100,*) a(i),b(i),c(i)
enddo

!print*, "theta=", theta_hat
do try=1,row

	do choose=1,length
		if (choose==1) then !first item
			
			do i=1,col
				used(i)=0 !還原
				infor(i)=information(theta_begin(try),a(i),b(i),c(i))
				!place_infor(i)=id_num(i)
			end do
			
			call subMAX(infor,col,maxv,place)
			
			i=place
			used(i)=1 !移除試題
			place_choose(try,choose)=i
			a_choose(try,choose)=a(i) !存入選題
			b_choose(try,choose)=b(i)
			c_choose(try,choose)=c(i)
			
			countPOOL(i)=1
			countA(i)=countA(i)+1
			
			!作答模擬
			call response(thetaTRUE(try),&
			a_choose(try,choose),b_choose(try,choose),c_choose(try,choose),&
			resp(try,choose),randv)
			
			call EAP(a_choose,b_choose,c_choose,resp,row,length,theta_hat,try,choose) !EAP能力估計
			
		else
		
			do i=1,col
				if (used(i)==0) then
					infor(i)=information(theta_hat(try,choose-1),a(i),b(i),c(i))
					!place_infor(i)=id_num(i)
				else
					infor(i)=0
				endif
				!print*, infor(i)
			end do		
			
			call subMAX(infor,col,maxv,place)
			
			i=place
			used(i)=1
			place_choose(try,choose)=i
			a_choose(try,choose)=a(i) !存入選題
			b_choose(try,choose)=b(i)
			c_choose(try,choose)=c(i)
			
			countPOOL(i)=1
			countA(i)=countA(i)+1
			
			!作答模擬
			call response(thetaTRUE(try),&
			a_choose(try,choose),b_choose(try,choose),c_choose(try,choose),&
			resp(try,choose),randv)
			
			call EAP(a_choose,b_choose,c_choose,resp,row,length,theta_hat,try,choose) !EAP能力估計
			
		endif
	enddo

	do i=1,col !400
		m(try,i)=countA(i)
	enddo
	!write(*,*) (m(try,i),i=1,col)

	call sub_omega(OmegaGA1,m,row,col,length,try,1) !計算本次的重疊率
	call sub_omega(OmegaGA2,m,row,col,length,try,2)
	call sub_omega(OmegaGA3,m,row,col,length,try,3)
		
		!print*, try,(resp(try,i),i=1,length)
enddo

!----------------------------------
!根據所估計的能力估計值 &OMEGA 進行計算

call sub_overlap(place_choose,row,length,overlap) !紀錄試題重疊率

!估計能力值 &OMEGA 的平均數、最大、最小
thetaHATmean = 0 !估計能力值的平均數
OmegaGA1_Mean = 0 !總體重疊率的平均值
OmegaGA2_Mean = 0
OmegaGA3_Mean = 0
OmegaGA1_Max = 0
OmegaGA2_Max = 0
OmegaGA3_Max = 0
OmegaGA1_Min = 1
OmegaGA2_Min = 1
OmegaGA3_Min = 1
do i=1,row !10000
	thetaHATmean = thetaHATmean + theta_hat(i,length)
	OmegaGA1_Mean = OmegaGA1_Mean + OmegaGA1(i)
	OmegaGA2_Mean = OmegaGA2_Mean + OmegaGA2(i)
	OmegaGA3_Mean = OmegaGA3_Mean + OmegaGA3(i)
	call subMaxPair(OmegaGA1_Max,OmegaGA1(i))
	call subMaxPair(OmegaGA2_Max,OmegaGA2(i))
	call subMaxPair(OmegaGA3_Max,OmegaGA3(i))
	
	if (i>1) then
		call subMinPair(OmegaGA1_Min,OmegaGA1(i))
	endif
	if (i>2) then
		call subMinPair(OmegaGA2_Min,OmegaGA2(i))
	endif
	if (i>3) then
		call subMinPair(OmegaGA3_Min,OmegaGA3(i))
	endif
end do
thetaHATmean = thetaHATmean/row
OmegaGA1_Mean = OmegaGA1_Mean/row
OmegaGA2_Mean = OmegaGA2_Mean/row
OmegaGA3_Mean = OmegaGA3_Mean/row
!write(*,*) thetaHATmean

thetaHatVariance = 0 !估計能力值的MSE
thetaHatMSE = 0 !估計能力值的MSE
OmegaGA1_Variance = 0
OmegaGA2_Variance = 0
OmegaGA3_Variance = 0
do i=1,row !10000
	thetaHatVariance = thetaHatVariance + (theta_hat(i,length)-thetaHATmean)**2
	thetaHatMSE = thetaHatMSE + (theta_hat(i,length) - thetaTRUEmean)**2
	OmegaGA1_Variance = OmegaGA1_Variance + (OmegaGA1(i)-OmegaGA1_Mean)**2
	OmegaGA2_Variance = OmegaGA2_Variance + (OmegaGA2(i)-OmegaGA2_Mean)**2
	OmegaGA3_Variance = OmegaGA3_Variance + (OmegaGA3(i)-OmegaGA3_Mean)**2
end do
thetaHatVariance = thetaHatVariance/row
thetaHatMSE = thetaHatMSE/row
OmegaGA1_Variance = OmegaGA1_Variance/row
OmegaGA2_Variance = OmegaGA2_Variance/row
OmegaGA3_Variance = OmegaGA3_Variance/row
!write(*,"(2F10.5)") thetaHatVariance,thetaHatMSE


!計算PA相關指標
sumA=0
sumPOOL=0
maxPA=0
place=0
do i=1,col
	!PS(i)=REAL(countS(i))/row
	PA(i)=REAL(countA(i))/row
	if(maxPA < PA(i))then
		maxPA = PA(i) 
		place = i
	else
		maxPA = maxPA
		place = place
	end if
	!sumS=sumS+countS(i)
	sumA=sumA+countA(i)
	sumPOOL=sumPOOL+countPOOL(i)
end do
PoolUR = real(sumPOOL)/col !題庫使用率
PAmean=0 !計算PA的平均
do i=1,col
	PAmean = PAmean+PA(i)
end do
PAmean = PAmean / col 
PAvariance = 0 !計算PA的變異
do i=1,col
	PAvariance = PAvariance + ( PA(i)-PAmean )**2
enddo
PAvariance = PAvariance / (col-1)

call cpu_time (t2) !結束計時
!------------------
open(101,file="summery_CAT_thetaHAT.txt",status="replace") !寫下能力估計
open(102,file="summery_CAT_response.txt",status="replace") !寫下作答反應
open(103,file="summery_CAT_item.txt",status="replace") !寫下所選的試題
open(104,file="summery_CAT_m.txt",status="replace") !紀錄m
open(105,file="summery_CAT_Omega.txt",status="replace")
open(106,file="summery_CAT_item_choose.txt",status="replace")

write(101,"(A5,A10,50I10)") "ID","true",(i,i=1,length)
write(102,"(A5,50I3)") "ID",(i,i=1,length)
write(103,"(A5,50I10)") "ID",(i,i=1,length)
write(104,"(A5,500I10)") "ID",(i,i=1,col) !紀錄m
write(105,"(A5,3A10)") "ID","GA1","GA2","GA3"
write(106,"(A10,A10,50I10)") "ID","parameter",(i,i=1,length)

do i=1,row !10000
	write(101,"(I5,50F10.4)") i,thetaTRUE(i),(theta_hat(i,j),j=1,length)
	write(102,"(I5,50I3)") i,(resp(i,j),j=1,length) !寫下作答反應
	write(103,"(I5,50I10)") i,(place_choose(i,j),j=1,length) !寫下所選的試題
	write(104,"(I5,500I10)") i,(m(i,j),j=1,col)
	write(105,"(I5,3F10.4)") i,OmegaGA1(i),OmegaGA2(i),OmegaGA3(i)
	
	write(106,"(I10,A10,50I10)") i,"itemID",(place_choose(i,j),j=1,length) !寫下所選的試題
	write(106,"(I10,A10,50F10.4)") i,"a",(a_choose(i,j),j=1,length)
	write(106,"(I10,A10,50F10.4)") i,"b",(b_choose(i,j),j=1,length)
	write(106,"(I10,A10,50F10.4)") i,"a",(c_choose(i,j),j=1,length)
	!write(*,*) i,(resp(i,j),j=1,length)
end do

!寫入所需資料
open(331,file="summery_CAT_summary.txt",status="replace") 
write(331,"(A15,I5,/,A15,I5,/,A15,I5,/)") 'item pool=',col,'length=',length,'try=',row ! / 為換行
write(331,"(A15,F15.5)") "Time",t2-t1
write(331,"(A15,F15.5)") "Mean",thetaHATmean
write(331,"(A15,F15.5)") "Bias",thetaHATmean-thetaTRUEmean
write(331,"(A15,F15.5)") "Variance",thetaHatVariance
write(331,"(A15,F15.5)") "MSE",thetaHatMSE
write(331,"(A15,F15.5)") "RMSE",thetaHatMSE**0.5
write(331,"(A15,F15.5)") "maxPA",maxPA
write(331,"(A15,F15.5)") "PAmean",PAmean
write(331,"(A15,F15.5)") "PAvar",PAvariance
write(331,"(A15,F15.5)") "PoolUR",PoolUR
write(331,"(A15,F15.5)") "overlap",overlap
write(331,"(A15,F15.5)") "GA1last",OmegaGA1(row)
write(331,"(A15,F15.5)") "GA1Mean",OmegaGA1_Mean
write(331,"(A15,F15.5)") "GA1sd",OmegaGA1_Variance**0.5
write(331,"(A15,F15.5)") "GA1Max",OmegaGA1_Max
write(331,"(A15,F15.5)") "GA1Min",OmegaGA1_Min
write(331,"(A15,F15.5)") "GA2last",OmegaGA2(row)
write(331,"(A15,F15.5)") "GA2Mean",OmegaGA2_Mean
write(331,"(A15,F15.5)") "GA2sd",OmegaGA2_Variance**0.5
write(331,"(A15,F15.5)") "GA2Max",OmegaGA2_Max
write(331,"(A15,F15.5)") "GA2Min",OmegaGA2_Min
write(331,"(A15,F15.5)") "GA3last",OmegaGA3(row)
write(331,"(A15,F15.5)") "GA3Mean",OmegaGA3_Mean
write(331,"(A15,F15.5)") "GA3sd",OmegaGA3_Variance**0.5
write(331,"(A15,F15.5)") "GA3Max",OmegaGA3_Max
write(331,"(A15,F15.5)") "GA3Min",OmegaGA3_Min

!!!!!Termination Section 終止區
end program CAT





!----------------------- 	
! 3PL probability
!----------------------- 
function probability(theta,a,b,c) 
implicit none
real::probability
real::theta,a,b,c
probability=c+(1-c)/(1+exp(-1.7*a*(theta-b))) !學姊的D為1.71
return
end

!----------------------- 	
! 3PL information
!----------------------- 
real function information(theta,a,b,c) 
implicit none
real::theta,a,b,c
real::prob,probMARK
prob=c+(1-c)/(1+exp(-1.7*a*(theta-b)))
!probMARK=1.7*a*(1-prob)*(prob-c)/(1-c)
!information=probMARK**2/(prob*(1-prob))
information=((1.7**2 * a**2 *(1-prob))/prob)*((prob-c)**2 / (1-c)**2)
return
end function information

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


!-----------------------
! Normal probability density function
!-----------------------
real function normalPDF(x)
implicit none
real, parameter:: pi = 3.14159
real:: x
real:: mu = 0, sigma = 1  !平均數=0、標準差=1
normalPDF = (1/((2*pi)**0.5 * sigma))* exp(-(x-mu)**2/(2*sigma**2))
return
end function normalPDF


!------------------
!EAP能力估計
!------------------
subroutine EAP(a,b,c,resp,row,length,thetaHAT,try,choose)
implicit none
integer::i,j,row,length
integer::try,choose
real::a(row,length),b(row,length),c(row,length) !選題的試題參數
integer::resp(row,length) !作答反應
real::thetaHAT(row,length) !估計能力值
real,external::probability,information,normalPDF !函數

!EAP使用參數
integer :: mat !根據估計間隔所切割的資料數
real :: seq = 0.01 !估計間隔
real,allocatable ::thetaCUT(:),PQmultiply(:),posterior(:) !切割能力值、機率乘積、後驗分配
real,allocatable ::prob(:,:),pq(:,:)
real::EAP_numerator !EAP分子
real::EAP_denominator !EAP分母

mat = INT((3-(-3))/seq+1) +1 !EAP初始設定

!deallocate(prob,pq) !取消定義的資料格數
allocate(prob(mat,choose),pq(mat,choose)) !重新定義的資料格數
allocate(thetaCUT(mat),PQmultiply(mat),posterior(mat)) !定義資料格數

thetaCUT(1)=-3 !起始能力
do i=2,mat
	thetaCUT(i)=thetaCUT(i-1)+seq !算出能力值
enddo

!計算作答反應機率
do i=1,mat
	do j=1,choose
		prob(i,j)=probability(thetaCUT(i),a(try,j),b(try,j),c(try,j))
		pq(i,j)=prob(i,j)**(resp(try,j))*(1-prob(i,j))**(1-resp(try,j))
	enddo
	!write(*,"(1X,20F10.4)") (pq(i,j),j=1,choose)  !隨題數變動
enddo

!計算作答反應機率乘機與倒數
if(choose==1)then
	do i=1,mat
		PQmultiply(i)=pq(i,1)
		posterior(i)=PQmultiply(i)*normalPDF(thetaCUT(i))
		!write(*,*) log(posterior(i))
	enddo
else
	do i=1,mat
		PQmultiply(i)=pq(i,1)
		do j=2,choose
			PQmultiply(i)=PQmultiply(i)*pq(i,j)
		enddo
		posterior(i)=PQmultiply(i)*normalPDF(thetaCUT(i))
		!write(*,*) log(posterior(i))
	enddo
end if

EAP_numerator=posterior(1)*thetaCUT(1)
EAP_denominator=posterior(1)
do i=2,mat
	EAP_numerator=EAP_numerator + posterior(i)*thetaCUT(i)
	EAP_denominator=EAP_denominator + posterior(i)
end do
thetaHAT(try,choose)=EAP_numerator/EAP_denominator !計算出能力估計值

return
end


!-----------------------
! combination 組合
!-----------------------
real function combination(n,k)
integer::i,j,n,k
real::Numerator !分子
real::Denominator !分母

if(n<k)then
	combination=0
else
	Numerator = 1
	Denominator = 1
	do i=1,k
		Numerator=Numerator*(n+1-i)
		Denominator=Denominator*i
	enddo
	combination = Numerator/Denominator
endif

return
end function


!-----------------------
! Omega subroutine
!-----------------------
subroutine sub_omega(Omega,m,row,col,length,try,gamma) !(item,row,length,num,overlap)
implicit none
real,external :: combination
real::Omega(row)
integer::m(row,col) !col = 題庫數、row = 人數
integer::row,col,length,try,gamma
integer::i !計算用
real::sumv !計算用

sumv=0.0
do i=1,col
	sumv = sumv + (m(try,i)-m(try-1,i)) * combination(try-m(try,i),gamma)
enddo


if (try<=gamma)then
	Omega(try)=0
else
	Omega(try) = REAL(try-gamma-1)/try * Omega(try-1) + REAL(gamma+1)/try &
	- sumv / (length * combination(try,gamma+1))
endif

return
end


!-----------------------
! Maximum and it's place
!-----------------------

subroutine subMAX(A,n,maxv,place)
integer:: i,n,place
real::A(n),maxv

maxv=A(1) !紀錄最大值
place=1 !紀錄相對位置
do i=2,n !400
	if(maxv < A(i))then
		maxv = A(i)
		place = i
	else
		maxv = maxv
		place = place
	end if
end do

return
end


!----------------------- 
! Maximum Compare
!----------------------- 
subroutine subMaxPair(Maxv,Compare)
implicit none
real::Maxv,Compare
if(Maxv < Compare)then
	Maxv = Compare
else
	Maxv = Maxv
end if
return
end 

!----------------------- 
! Minimum Compare
!----------------------- 
subroutine subMinPair(Minv,Compare)
implicit none
real::Minv,Compare
if(Minv > Compare)then
	Minv = Compare
else
	Minv = Minv
end if
return
end 


!------------------
!試題重疊率計算
!------------------
subroutine sub_overlap(item,row,length,overlap) !(item,row,length,num,overlap)
implicit none
integer:: i,j,p,q
integer:: num !指定比較的前?個人 →驗證用
integer:: row !人數，切割item用
integer:: length !題長，切割item用
integer:: item(row,length)
real(kind=8):: overlap
integer::sumv
real::Numerator !分子
real::Denominator !分母
real,external:: combination

sumv = 0
do i=1,row !(num-1) !→驗證用
	do j=(i+1),row !num !→驗證用
		do p=1,length
			do q=1,length
				
				if ( item(i,p) == item(j,q) ) then
				!write(*,*) i,j,p,q
				sumv = sumv + 1
				endif
				
			end do
		end do	
	enddo
enddo

Numerator = REAL(sumv)
Denominator = combination(row,2) * length
overlap = Numerator/Denominator

return
end 


