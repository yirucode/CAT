program main_OMST_cont_SH
    use ran_mod !呼叫常態隨機模組
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_300.txt"
    character(len = 50), parameter :: dataPath2 = "data/Population_Normal.txt"!Uniform Normal
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數 10000
    integer,parameter :: numPool = 300 !題庫數
    integer,parameter :: length = 20 !作答題長
    integer,parameter :: numContentType = 3
    ! === module set ===
    integer :: contentScale(numContentType) = (/2,2,1/)
    integer :: contentMultiplier = 2
    ! === OMST set ===
    integer :: usedStages
    integer,parameter :: numStages = 2
    integer :: realChoose
    ! === content target ===
    integer :: contentGoal = 0
    integer :: contentTarget(numContentType) != (/16,16,8/)
    integer :: contentChange(numContentType) 
    integer :: contentPoolNum(numContentType) = (/120,120,60/) ! SH控管新增
    integer :: contentPoolLimit(numContentType)   ! 初始值等於contentPoolNum；SH控管新增
    integer :: contentPoolChange(numContentType)  ! SH控管新增
    real :: randContent
    real :: contentTP(numContentType)
    ! === item parameter ===
    real::a(numPool), b(numPool), c(numPool) !題庫試題參數
    integer:: content(numPool)
    ! === true theta ===
    real :: thetaTrue(numTest) = 1. !真實能力值
    real :: thetaTrueMean = 0. !真實能力值之平均
    real :: thetaBegin = 0.
    ! === function ===
    real, external :: information, probability, normalPDF
    ! === unknown data ===
    ! === 迴圈用 ===
    integer :: i,j
    integer :: try
    integer :: choose
    real :: x
    ! === 運算暫存用 ===
    real :: maxv !最大值
    integer :: sumv_Int 
    integer :: place
    real :: infor(numPool) !題庫各試題的訊息量
    real :: randv(length, numTest)
    ! === output data ===
    integer :: resp(length, numTest) !作答反應
    integer:: place_choose(length, numTest) !選題的試題位置
    ! content 相關 
    integer:: content_choose(length, numTest)
    integer, dimension(numContentType, numTest)::contentResult
    real, dimension(numContentType)::contentResultMean
    integer, dimension(numContentType)::contentResultMax
    integer, dimension(numContentType)::contentResultMin
    ! 選題的試題參數 
    real:: a_choose(length, numTest),b_choose(length, numTest), &
    c_choose(length, numTest) 
    ! 試題使用率；使用 = Administer
    integer :: usedPool(numPool, numTest) !紀錄試題是否被使用過
    integer :: usedSum(numPool, numTest) !試題被使用過的累加次數
    real:: usedRate(numPool) !PA
    real:: usedRateMax !maxPA
    real:: usedRateMean !PAmean
    real:: usedRateVar !PAvar
    real :: poolUsedRate !PoolUR；題數使用率
    ! 測驗重疊率參數
    real:: testOverlapData
    real:: testOverlap
    ! 估計能力參數 
    ! real(kind=8)::thetaHat(numStages, numTest)
    ! real(kind=8)::thetaHatMean !估計能力值的平均數
    ! real(kind=8)::thetaBias !估計能力值與真值的差之平均
    ! real(kind=8)::thetaHatVar !估計能力值的變異數
    ! real(kind=8)::thetaHatMSE !估計能力值的MSE
    real::thetaHat(numStages, numTest)
    real::thetaHatMean !估計能力值的平均數
    real::thetaBias !估計能力值與真值的差之平均
    real::thetaHatVar !估計能力值的變異數
    real::thetaHatMSE !估計能力值的MSE
    ! Omega
    real, dimension(numTest)::omegaOne
    real, dimension(numTest)::omegaTwo
    real, dimension(numTest)::omegaThree
    real:: omegaOneMean, omegaTwoMean, omegaThreeMean !平均
    real:: omegaOneMax, omegaTwoMax, omegaThreeMax 
    real:: omegaOneMin, omegaTwoMin, omegaThreeMin
    real:: omegaOneVar, omegaTwoVar, omegaThreeVar
    ! Psi
    real, dimension(numTest)::psiOne
    real, dimension(numTest)::psiTwo
    real, dimension(numTest)::psiThree
    real:: psiOneMean, psiTwoMean, psiThreeMean
    real:: psiOneMax, psiTwoMax, psiThreeMax
    real:: psiOneMin, psiTwoMin, psiThreeMin
    real:: psiOneVar, psiTwoVar, psiThreeVar
    ! infor note
    real, dimension(length, numTest):: choose_inforTrue
    real, dimension(length, numTest):: choose_inforEstimate
    real, dimension(numTest):: choose_inforTrueSum
    real, dimension(numTest):: choose_inforEstimateSum
    real :: testMean_inforTrue
    real :: testMean_inforEstimate
    ! === 存取時間 ===
    real (kind=8) t1 !開始時間
    real (kind=8) t2 !結束時間
    ! === output error ===
    integer :: ierror
    ! === 輸出資料格式設定 === 
    character(len = 20), parameter :: input = 'ListCAT_thetaHat.txt'
    character(len = 20), parameter :: dataINT = '(100I10)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataF = '(100F10.4)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataFS = '(100F10.2)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataPool = '(500I10)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataPoolFS = '(500F10.2)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataContentReal = '(10F10.4)'  ! 隨著 content type number 改變
    character(len = 20), parameter :: dataContentInt = '(10I10)'
    ! ================== 
    ! === SH control === 
    ! 曝光率控管參數設定
    real, parameter::rMAX = 0.2
    real::stop_rMAX = rMAX + rMAX*0.1 !0.22
    real::stop_minMaxPA = 1 !紀錄迭代最小的maxPA
    integer::stop_NUM = 0
    real:: PK(numPool) = (/(1.0,i=1,numPool)/) !所有曝光率參數 初始值=1
    integer::numPK1 = numPool !PK=1的數目，初始值等於題庫數
    ! SH運算暫存用 
    real(kind = 8):: rand_SH !亂數；用於SH法
    integer:: itemID !題號；SH新增
    integer:: numIterate = 0!迭代次數；SH新增
    ! 試題被選擇率
    integer, dimension(numPool, numTest)::selectPool !SH新增
    integer, dimension(numPool, numTest)::selectSum !SH新增
    real:: selectRate(numPool) ! PS
    real:: selectRateMax !maxPS
    real:: selectRateMean !PSmean
    real:: selectRateVar !PSvar
    ! 題庫試題選中與使用次數之檢驗數值
    integer::sumS 
    integer::sumA 
    integer::sumS_Change
    integer::sumS_cont_Change
    integer::sum_num_SH
    ! integer::sumPOOL 
    ! SH 鬆綁參數    
    ! 判斷題庫是否有試題可選 
    integer :: count_InforZero

    ! === run code ===
    call cpu_time (t1) !開始計時
    ! 讀取資料
    ! 輸入試題參數
    open(100, file= dataPath, status="old") 
    do i=1,numPool
        read(100,*) a(i),b(i),c(i),content(i) !三參數
    enddo
    close(100)

    ! 輸入受試者真實能力值
    open(100, file= dataPath2, status="old") 
    read(100,*)  ! 跳過第一列
    do i=1,numTest
        read(100,*) x, thetaTrue(i) !從資料讀取三參數
    enddo
    close(100)

    ! ! 產生模擬的受試者真實能力值
    ! do i=1,numTest
    !     thetaTrue( i ) = REAL(normal( REAL(thetaTrueMean,KIND=8), 1.0D0 ),KIND=4) !自行生成
    !     ! thetaTrue( i ) = REAL(normal( REAL(2,KIND=8), 1.0D0 ),KIND=4) !自行生成
    ! enddo
    
    ! ! 檢驗模擬資料
    ! call subr_aveReal(thetaTrue, numTest, thetaHatMean)
    ! call subr_varReal(thetaTrue, numTest, thetaHatVar)
    ! WRITE(*,*) "mean = ",thetaHatMean, " var = ", thetaHatVar
    ! open(100, file= 'theta_Normal.txt', status="replace") 
    ! WRITE(100,"(A20,A20)") "ID", "theta"
    ! do i=1,numTest
    !     WRITE(100,"(I20,F20.15)") i, thetaTrue(i)
    ! enddo
    ! close(100)
    !==================
    !運算過程中，寫下SH曝光率迭代資料
    !==================
    open(unit = 101 , file = 'summery_SH_maxPA.txt' , status = 'replace', action = 'write', iostat= ierror)
    open(unit = 102 , file = 'summery_SH_PK_test.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 101, fmt ='(8A10)') "freq","item","PAmax","numPK1","PAmean","PAvar","PoolUR","OverLap"
    write(unit = 102, fmt ="(2A10,300I10)") "Iterate","data",(i,i=1,numPool)
    
    contentTarget = contentScale*contentMultiplier ! 模組內各領域試題題數
    contentPoolLimit = contentPoolNum - contentTarget*numStages ! 題庫各領域試題可用於迭代之上限量
    ! do i = 1, numPool
    !     PK(i) = 0.2
    ! enddo
        
    !開始SH的迭代
    do while( .true. )
    ! do x = 1,10
        do try = 1, numTest
        ! 每次迭代須重設的值
        if (try == 1) then
            do j = 1,numTest
                do i = 1,numPool
                    usedPool(i, j) = 0 
                    usedSum(i, j) = 0 
                enddo
            enddo
        endif
            do usedStages = 1, numStages
                do choose = 1, length/numStages
                    ! 不同受試者須重設的值
                    if (( choose == 1 ).AND.(usedStages == 1)) then
                        !重設試題使用狀況
                        realChoose = 0
                        do i = 1, numContentType
                            contentPoolChange(i) = 0
                        enddo
                        do i=1,numPool
                            selectPool(i, try) = 0 
                            selectSum(i, try) = 0 
                        enddo
                        do i=1,length
                            place_choose(i, try) = 0  
                            resp(i, try) = 9
                        enddo
                    endif
                    realChoose = realChoose +1 ! 題長中的第幾題
                    ! 內容領域相關設定
                    if ( choose == 1 ) then ! 不同module須重設的值
                        contentChange = contentTarget ! 重設內容領域控制參數
                    endif
                    ! 選擇要施測的內容領域
                    call subr_contentTargetP(contentChange, numContentType, contentTP) ! 計算各內容機率範圍
                    call random_number(randContent) !隨機選擇要施測的內容領域
                    do i = 1, numContentType
                        if (i.EQ.1) then
                            if ((randContent > 0) .AND. (randContent <= contentTP(i))) then
                                contentGoal = i
                            endif
                        else
                            if ((randContent > contentTP(i-1)) .AND. (randContent <= contentTP(i))) then
                                contentGoal = i
                            endif
                        endif
                    enddo
                    contentChange(contentGoal) = contentChange(contentGoal)-1 ! 刪除"選中&施測"的內容題數
                    
                    sum_num_SH = 0
                    ! 納入SH控管
                    do while (.true.)
                        contentPoolChange(contentGoal) = contentPoolChange(contentGoal) +1 ! 加總"被選中"的內容題數
                        sum_num_SH = sum_num_SH + 1
                        ! write(*,*) '==================='
                        ! write(*,*) 'sum number of SH  = ', sum_num_SH
                        ! write(*,*) 'contentPoolChange = ',contentPoolChange
                        ! 計算訊息量
                        if ( usedStages == 1 ) then       
                            do i = 1, numPool
                                if ( ( selectPool(i, try) == 0) .AND. (content(i) == contentGoal) ) then
                                    infor(i) = information(thetaBegin, a(i), b(i), c(i))
                                else
                                    infor(i) = 0
                                endif
                            enddo
                        else
                            do i = 1, numPool
                                if ( ( selectPool(i, try) == 0) .AND. (content(i) == contentGoal)) then
                                    infor(i) = information(thetaHat(usedStages-1, try), a(i), b(i), c(i))
                                else
                                    infor(i) = 0
                                endif
                            enddo
                        endif
                        call subr_maxvReal(infor, numPool, maxv, place_choose(realChoose, try)) ! 求出最大訊息量與其題庫ID(紀錄使用的試題題號)
                        ! 成功選題後，紀錄選出的試題
                        selectPool(place_choose(realChoose, try), try) = 1 !紀錄選出的試題
                        ! ! 計算被選次數
                        ! sumS = 0
                        ! do i = 1, numPool
                        !     sumS = sumS + selectPool(i, try)
                        ! enddo
                        ! write(*,*) 'sumS              = ', sumS
                        !產生用於SH的隨機變數
                        call random_number(rand_SH)
                        ! ! 驗證隨機變數的效果 
                        ! write(*,*) "======== "
                        ! write(*,*) "rand   = ", REAL(rand_SH,kind = 4)
                        ! write(*,*) "PK     = ", PK(place_choose(realChoose, try))
                        ! write(*,*) "place  = ", place_choose(realChoose, try)
                        if (REAL(rand_SH,kind = 4) <= PK(place_choose(realChoose, try))) then
                            exit
                        elseif (contentPoolChange(contentGoal)>contentPoolLimit(contentGoal)) then
                            ! write(*,*) "content error!"
                            exit ! 避免出現無試題選出的狀況
                        endif
                    enddo !SH篩選迴圈End
                    
                    ! 成功選題後，紀錄選出的試題
                    usedPool(place_choose(realChoose, try), try) = 1 !紀錄選出並使用的試題
                    ! 紀錄使用的試題參數
                    a_choose(realChoose, try) = a(place_choose(realChoose, try))
                    b_choose(realChoose, try) = b(place_choose(realChoose, try))
                    c_choose(realChoose, try) = c(place_choose(realChoose, try))
                    content_choose(realChoose, try) = content(place_choose(realChoose, try))
                    ! 模擬作答，並記錄作答反應
                    call subr_resp(thetaTrue(try), &
                    a_choose(realChoose, try),b_choose(realChoose, try),c_choose(realChoose, try),&
                    resp(realChoose, try),randv(realChoose, try))
                    ! ! 驗算內容選擇狀況
                    ! write(*,*) "==================="
                    ! write(*,*) "try               =", try
                    ! write(*,*) "realChoose        =", realChoose
                    ! write(*,*) "contentTP         =", (contentTP(i),i=1,numContentType)
                    ! write(*,*) "randContent       =", randContent
                    ! write(*,*) "contentGoal       =", contentGoal
                    ! write(*,*) "contentChange     =", (contentChange(i),i=1,numContentType)
                    ! write(*,*) "contentPoolLimit  =", (contentPoolLimit(i),i=1,numContentType)
                    ! write(*,*) "contentPoolChange =", (contentPoolChange(i),i=1,numContentType)
                    ! sumS = 0
                    ! do i = 1, numPool
                    !     sumS = sumS + usedPool(i, try)
                    ! enddo
                    ! write(*,*) 'try               =', try
                    ! write(*,*) 'sumS_Pool         = ', sumS

                    ! 驗算題庫選擇狀況
                    ! write(*,*) "==================="
                    ! write(*,*) "selectPool        =", (selectPool(i, try),i=1,numPool)
                    ! write(*,*) "usedPool          =", (usedPool(i, try),i=1,numPool)
                    ! write(*,*) "selectPool        =", selectPool(place_choose(realChoose, try), try)
                    ! write(*,*) "usedPool          =", usedPool(place_choose(realChoose, try), try)
                enddo

                ! EAP能力估計
                call subr_EAP(realChoose, &
                a_choose(1:usedStages*(length/numStages), try),&
                b_choose(1:usedStages*(length/numStages), try),&
                c_choose(1:usedStages*(length/numStages), try),&
                resp(1:usedStages*(length/numStages), try), thetaHat(usedStages, try))
                ! write(*,*) "==================="
                ! write(*,*) "thetaTrue   =", thetaTrue(try)
                ! write(*,*) "thetaHat    =", thetaHat(usedStages,try)
            enddo
            ! 計算每位受試者於不同內容領域中用了幾題
            do i=1,numContentType
                call subr_contentCount(content_choose(:,try),length,i,contentResult(i,try))
            enddo
            ! write(*,*) "==================="
            ! write(*,*) "thetaHat    =", (thetaHat(i,try),i=1,numStages)
            ! write(*,*) "contentResult =", (contentResult(i,try),i=1,numContentType)
        enddo
        
        ! === 施測所有受試者後 ===!
        ! 記錄迭代次數
        numIterate = numIterate + 1 
        ! 算出被施測(A)&被選擇(S)的累加次數
        do try = 1, numTest
            do i = 1, numPool
                if ( try == 1 ) then
                    selectSum(i,try) = selectPool(i,try) ! 被選擇(S)的累加次數
                    usedSum(i,try) = usedPool(i,try) ! 被施測(A)的累加次數
                else
                    selectSum(i,try) = selectSum(i,try-1) + selectPool(i,try) ! 被選擇(S)的累加次數
                    usedSum(i,try) = usedSum(i,try-1) + usedPool(i,try) ! 被施測(A)的累加次數
                endif
            enddo
        enddo
        ! 算出試題使用率(PA)與被選擇率(PS)
        do i=1, numPool
            usedRate(i) = REAL(usedSum(i,numTest))/numTest !PA
            selectRate(i) = REAL(selectSum(i,numTest))/numTest !PS
        enddo
        ! write(*,*) "usedRate          =", (usedRate(i),i=1,numPool)
        ! write(*,*) "selectRate        =", (selectRate(i),i=1,numPool)
        ! 算出最大PA
        call subr_maxvReal(usedRate, numPool, usedRateMax, place) !取PA的最大值與相對位置
        write(*,*) "numIterate= ",numIterate, " numPK1 = ", numPK1," place = ", place," maxPA= ",usedRateMax
        call subr_aveReal(usedRate, numPool, usedRateMean)
        call subr_varReal(usedRate, numPool, usedRateVar)
        call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate) !SH迭代題庫使用率
        ! 寫下資料
        write(unit = 101, fmt ="(2I10,F10.5,I10,4F10.5)") numIterate , place ,usedRateMax,&
        numPK1,usedRateMean,usedRateVar,poolUsedRate !"PoolUR" 
        
        ! 紀錄迭代最小的maxPA
        if ( usedRateMax < stop_minMaxPA ) then
            stop_minMaxPA = usedRateMax
            ! write(*,*) "min(maxPA)= ",usedRateMax
        end if

        !此式成立，則跳離此迴圈
        if (usedRateMax < stop_rMAX) then
            stop_NUM = stop_NUM +1
        end if
        if (stop_NUM >= 5) then !要出現5次此情形，才會跳出迴圈
            if (usedRateMax <= stop_minMaxPA) exit
        end if 

        !SH曝光率控制 PK值調整
        call setPK(selectRate,numPool,rMAX,length,PK,numPK1)
        write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numS",(selectSum(i,numTest),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numA",(usedSum(i,numTest),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PS",(selectRate(i),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PA",(usedRate(i),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PK",(PK(i),i=1,numPool)
    enddo !結束迭代

    write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numS",(selectSum(i,numTest),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numA",(usedSum(i,numTest),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PS",(selectRate(i),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PA",(usedRate(i),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PK",(PK(i),i=1,numPool)
    ! close(101)
    ! close(102)

    ! 根據迭代所得的值再施測一次
    ! 輸入受試者真實能力值
    open(100, file= dataPath2, status="old") 
    read(100,*)  ! 跳過第一列
    do i=1,numTest
        read(100,*) x, thetaTrue(i) !從資料讀取三參數
    enddo
    close(100)

    ! 重新施測須重設的值
    do j = 1,numTest
        do i = 1,numPool
            usedPool(i, j) = 0 
            usedSum(i, j) = 0
            selectPool(i, try) = 0 
            selectSum(i, try) = 0 
        enddo
        do i=1,length
            place_choose(i, try) = 0  
            resp(i, try) = 9
            a_choose(i, try) = 0
            b_choose(i, try) = 0
            c_choose(i, try) = 0
            content_choose(i, try) = 0
        enddo
    enddo

    do try = 1, numTest
        do usedStages = 1, numStages
            do choose = 1, length/numStages
                ! 不同受試者須重設的值
                if (( choose == 1 ).AND.(usedStages == 1)) then
                    !重設試題使用狀況
                    realChoose = 0
                    do i = 1, numContentType
                        contentPoolChange(i) = 0
                    enddo
                endif
                realChoose = realChoose +1 ! 題長中的第幾題
                ! 內容領域相關設定
                if ( choose == 1 ) then ! 不同module須重設的值
                    contentChange = contentTarget ! 重設內容領域控制參數
                endif
                ! 選擇要施測的內容領域
                call subr_contentTargetP(contentChange, numContentType, contentTP) ! 計算各內容機率範圍
                call random_number(randContent) !隨機選擇要施測的內容領域
                do i = 1, numContentType
                    if (i.EQ.1) then
                        if ((randContent > 0) .AND. (randContent <= contentTP(i))) then
                            contentGoal = i
                        endif
                    else
                        if ((randContent > contentTP(i-1)) .AND. (randContent <= contentTP(i))) then
                            contentGoal = i
                        endif
                    endif
                enddo
                contentChange(contentGoal) = contentChange(contentGoal)-1 ! 刪除"選中&施測"的內容題數
                
                ! 納入SH控管
                do while (.true.)
                    contentPoolChange(contentGoal) = contentPoolChange(contentGoal) +1 ! 加總"被選中"的內容題數
                    sum_num_SH = sum_num_SH + 1
                    ! 計算訊息量
                    if ( usedStages == 1 ) then       
                        do i = 1, numPool
                            if ( ( selectPool(i, try) == 0) .AND. (content(i) == contentGoal) ) then
                                infor(i) = information(thetaBegin, a(i), b(i), c(i))
                            else
                                infor(i) = 0
                            endif
                        enddo
                    else
                        do i = 1, numPool
                            if ( ( selectPool(i, try) == 0) .AND. (content(i) == contentGoal)) then
                                infor(i) = information(thetaHat(usedStages-1, try), a(i), b(i), c(i))
                            else
                                infor(i) = 0
                            endif
                        enddo
                    endif
                    call subr_maxvReal(infor, numPool, maxv, place_choose(realChoose, try)) ! 求出最大訊息量與其題庫ID(紀錄使用的試題題號)
                    ! 成功選題後，紀錄選出的試題
                    selectPool(place_choose(realChoose, try), try) = 1 !紀錄選出的試題
                    !產生用於SH的隨機變數
                    call random_number(rand_SH)
                    if (REAL(rand_SH,kind = 4) <= PK(place_choose(realChoose, try))) then
                        exit
                    elseif (contentPoolChange(contentGoal)>contentPoolLimit(contentGoal)) then
                        ! write(*,*) "content error!"
                        exit ! 避免出現無試題選出的狀況
                    endif
                enddo !SH篩選迴圈End
                
                ! 成功選題後，紀錄選出的試題
                usedPool(place_choose(realChoose, try), try) = 1 !紀錄選出並使用的試題
                ! 紀錄使用的試題參數
                a_choose(realChoose, try) = a(place_choose(realChoose, try))
                b_choose(realChoose, try) = b(place_choose(realChoose, try))
                c_choose(realChoose, try) = c(place_choose(realChoose, try))
                content_choose(realChoose, try) = content(place_choose(realChoose, try))
                ! 模擬作答，並記錄作答反應
                call subr_resp(thetaTrue(try), &
                a_choose(realChoose, try),b_choose(realChoose, try),c_choose(realChoose, try),&
                resp(realChoose, try),randv(realChoose, try))
            enddo
            
            ! EAP能力估計
            call subr_EAP(realChoose, &
            a_choose(1:usedStages*(length/numStages), try),&
            b_choose(1:usedStages*(length/numStages), try),&
            c_choose(1:usedStages*(length/numStages), try),&
            resp(1:usedStages*(length/numStages), try), thetaHat(usedStages, try))
        enddo
        ! 計算每位受試者於不同內容領域中用了幾題
        do i=1,numContentType
            call subr_contentCount(content_choose(:,try),length,i,contentResult(i,try))
        enddo
    enddo
        
        ! === 施測所有受試者後 ===!
        ! 算出被施測(A)&被選擇(S)的累加次數
        do try = 1, numTest
            do i = 1, numPool
                if ( try == 1 ) then
                    selectSum(i,try) = selectPool(i,try) ! 被選擇(S)的累加次數
                    usedSum(i,try) = usedPool(i,try) ! 被施測(A)的累加次數
                else
                    selectSum(i,try) = selectSum(i,try-1) + selectPool(i,try) ! 被選擇(S)的累加次數
                    usedSum(i,try) = usedSum(i,try-1) + usedPool(i,try) ! 被施測(A)的累加次數
                endif
            enddo
        enddo
        ! 算出試題使用率(PA)與被選擇率(PS)
        do i=1, numPool
            usedRate(i) = REAL(usedSum(i,numTest))/numTest !PA
            selectRate(i) = REAL(selectSum(i,numTest))/numTest !PS
        enddo
        ! write(*,*) "usedRate          =", (usedRate(i),i=1,numPool)
        ! write(*,*) "selectRate        =", (selectRate(i),i=1,numPool)
        ! 算出最大PA
        call subr_maxvReal(usedRate, numPool, usedRateMax, place) !取PA的最大值與相對位置
        write(*,*) "Final place = ", place," Final maxPA= ",usedRateMax
        call subr_aveReal(usedRate, numPool, usedRateMean)
        call subr_varReal(usedRate, numPool, usedRateVar)
        call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate) !SH迭代題庫使用率

        ! 寫下資料
        write(unit = 101, fmt ="(A30)") "================================="
        write(unit = 101, fmt ="(2I10,F10.5,I10,4F10.5)") numIterate , place ,usedRateMax,&
        numPK1,usedRateMean,usedRateVar,poolUsedRate !"PoolUR" 

        write(unit = 102, fmt ="(A30)") "================================="
        write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numS",(selectSum(i,numTest),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numA",(usedSum(i,numTest),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PS",(selectRate(i),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PA",(usedRate(i),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PK",(PK(i),i=1,numPool)
        close(101)
        close(102)

    
end program