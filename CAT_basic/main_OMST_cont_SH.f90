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
    integer :: contentTargetSum(numContentType) != (/16,16,8/)
    integer :: contentChange(numContentType) 
    integer :: contentPoolNum(numContentType) = (/120,120,60/) ! SH控管新增
    integer :: contentPoolNumSum(numContentType) ! SH控管新增
    integer :: contentPoolLimit(numContentType)   ! 初始值等於contentPoolNum；SH控管新增
    integer :: contentPoolChange(numContentType)  ! SH控管新增
    real :: randContent
    real :: contentTP(numContentType)
    ! === item parameter ===
    real::a(numPool), b(numPool), c(numPool) !題庫試題參數
    integer:: content(numPool)
    ! === true theta ===
    real :: thetaTrue(numTest) = 0.0 !真實能力值
    real :: thetaTrueMean = 0.0 !真實能力值之平均
    real :: thetaBegin = 0.0
    ! === function ===
    real, external :: information, probability, normalPDF
    ! === unknown data ===
    ! === 迴圈用 ===
    integer :: i,j
    integer :: try
    integer :: choose
    integer :: xi
    real :: x, y
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
    integer :: usedTotalSum(numTest)
    real:: usedRate(numPool) !PA
    real:: usedRateMax !maxPA
    real:: usedRateMean !PAmean
    real:: usedRateVar !PAvar
    ! PK = 1 試題的使用率
    real:: usedRate_PK1(numPool) !PA
    real:: usedRateMax_PK1 !maxPA
    real:: usedRateMean_PK1 !PAmean
    real:: usedRateVar_PK1 !PAvar
    ! PK < 1 試題的使用率
    real:: usedRate_PK0(numPool) !PA
    real:: usedRateMax_PK0 !maxPA
    real:: usedRateMean_PK0 !PAmean
    real:: usedRateVar_PK0 !PAvar
    !PoolUR；題數使用率
    real :: poolUsedRate 
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
    ! double precision, dimension(length, numTest):: choose_inforTrue
    ! double precision, dimension(length, numTest):: choose_inforEstimate
    ! double precision, dimension(numTest):: choose_inforTrueSum
    ! double precision, dimension(numTest):: choose_inforEstimateSum
    ! double precision:: testMean_inforTrue
    ! double precision:: testMean_inforEstimate
    real, dimension(length, numTest):: choose_inforTrue
    real, dimension(length, numTest):: choose_inforEstimate
    real, dimension(numTest):: choose_inforTrueSum
    real, dimension(numTest):: choose_inforEstimateSum
    real:: testMean_inforTrue
    real:: testMean_inforEstimate
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
    character(len = 20), parameter :: dataPool_T = '(A10,500I10)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataPoolFS = '(500F10.2)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataContentReal = '(10F10.4)'  ! 隨著 content type number 改變
    character(len = 20), parameter :: dataContentInt = '(10I10)'
    ! ================== 
    ! === SH control === 
    ! 曝光率控管參數設定 (2)
    real, parameter::rMAX = 0.2
    real, parameter:: stop_x = 0.01 ! 迭代緩衝的差值
    real::stop_minMaxPA = 1.0 !紀錄迭代最小的maxPA
    integer::stop_NUM = 0
    real:: PK(numPool) = (/(1.0,i=1,numPool)/) !所有曝光率參數 初始值=1
    integer::numPK1(numContentType)
    integer::numSumPK1 = numPool !PK=1的數目，初始值等於題庫數
    ! SH運算暫存用 
    real(kind = 8):: rand_SH !亂數；用於SH法
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
    ! 判斷題庫是否有試題可選 
    ! integer :: count_InforZero

    ! === run code ===
    call cpu_time (t1) !開始計時
    ! 讀取資料
    ! 輸入試題參數
    open(100, file= dataPath, status="old") 
    do i=1,numPool
        read(100,*) a(i),b(i),c(i),content(i) !三參數
    enddo
    close(100)

    ! ! 輸入受試者真實能力值
    ! open(100, file= dataPath2, status="old") 
    ! read(100,*)  ! 跳過第一列
    ! do i=1,numTest
    !     read(100,*) x, thetaTrue(i) !從資料讀取三參數
    ! enddo
    ! close(100)

    ! 產生模擬的受試者真實能力值
    do i=1,numTest
        thetaTrue( i ) = REAL(normal( REAL(thetaTrueMean,KIND=8), 1.0D0 ),KIND=4) !自行生成
        ! thetaTrue( i ) = REAL(normal( REAL(2,KIND=8), 1.0D0 ),KIND=4) !自行生成
    enddo
    
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
    open(unit = 101 , file = 'ListCAT_summery_SH_maxPA.txt' , status = 'replace', action = 'write', iostat= ierror)
    open(unit = 102 , file = 'ListCAT_summery_SH_PK_test.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 101, fmt ='(8A10)') "freq","item","PAmax","numSumPK1","PAmean","PAvar","PoolUR","OverLap"
    write(unit = 102, fmt ="(2A10,300I10)") "Iterate","data",(i,i=1,numPool)
    
    contentTarget = contentScale*contentMultiplier ! 模組內各領域試題題數
    do i = 1, numContentType
        if(i==1)then 
            contentTargetSum(i) = contentTarget(i)
            contentPoolNumSum(i) = contentPoolNum(i)
        else
            contentTargetSum(i) = contentTargetSum(i-1) + contentTarget(i)
            contentPoolNumSum(i) = contentPoolNumSum(i-1) + contentPoolNum(i)
        endif
    enddo
    ! contentPoolLimit = contentPoolNum - contentTarget*numStages ! 題庫各領域試題可用於迭代之上限量
    numPK1 = contentPoolNum  !題庫各領域試題之PK為1的數量初始值
    ! do i = 1, numPool
    !     PK(i) = 0.2
    ! enddo
        
    !開始SH的迭代
    do while( .true. )
    ! do xi = 1,15
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
                        usedTotalSum(try) = 0
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
                        ! contentPoolChange(contentGoal) = contentPoolChange(contentGoal) +1 ! 加總"被選中"的內容題數
                        sum_num_SH = sum_num_SH + 1
                        ! write(*,*) '==================='
                        ! write(*,*) 'sum number of SH  = ', sum_num_SH
                        ! write(*,*) 'contentGoal       = ', contentGoal
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
                        ! elseif (contentPoolChange(contentGoal)>contentPoolLimit(contentGoal)) then
                            ! write(*,*) "content error!"
                            ! exit
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
                    ! write(*,*) '======================================= '
                    ! write(*,*) 'try               = ', try
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
                usedTotalSum(try) = usedTotalSum(try) + usedPool(i,try)
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
        write(*,"(A10,I10,A10,I10,A10,I10,A10,F10.5)") "numIterate= ",numIterate, " numSumPK1 = ", numSumPK1,&
        " place = ", place," maxPA= ",usedRateMax
        call subr_aveReal(usedRate, numPool, usedRateMean)
        call subr_varReal(usedRate, numPool, usedRateVar)
        call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate) !SH迭代題庫使用率
        call subr_testOverlap(place_choose, numTest, length, testOverlapData) ! 不知為何會受下面subr_maxvInt的影響，待查證
        testOverlap = testOverlapData
        ! 寫下資料
        write(unit = 101, fmt ="(2I10,F10.5,I10,4F10.5)") numIterate , place ,usedRateMax,&
        numSumPK1,usedRateMean,usedRateVar,poolUsedRate, testOverlap 
        
        if (numIterate > 10) then
            ! 紀錄迭代最小的maxPA
            if ( usedRateMax < stop_minMaxPA ) then 
                stop_minMaxPA = usedRateMax
                ! write(*,*) "min(maxPA)= ",usedRateMax
            endif
            ! 迭代停止規則
            if ( numSumPK1 >= length) then
                if (ABS(usedRateMax - stop_minMaxPA) <= stop_x) then
                    stop_NUM = stop_NUM +1
                    if (stop_NUM >= 5) exit ! 此式成立(出現5次)，才會跳出迴圈
                endif
            else
                print*, "error!"
            endif
        endif

        !SH曝光率控制 PK值調整
        do i=1, numContentType
            if (i == 1) then
                call setPK(selectRate(1:contentPoolNumSum(i)),contentPoolNum(i),&
                rMAX,contentTarget(i)*numStages,PK(1:contentPoolNumSum(i)),numPK1(i))
            else
                call setPK(selectRate(contentPoolNumSum(i-1)+1:contentPoolNumSum(i)),contentPoolNum(i),&
                rMAX,contentTarget(i)*numStages,PK(contentPoolNumSum(i-1)+1:contentPoolNumSum(i)),numPK1(i))
            endif
        enddo
        do i = 1, numContentType
            numPK1(i) = 0
            if (i == 1) then
                do j = 1,contentPoolNumSum(i)
                    if (PK(j) == 1) then
                        numPK1(i) = numPK1(i) + 1
                    endif
                enddo
                numSumPK1 = numPK1(i)
            else
                do j = contentPoolNumSum(i-1)+1,contentPoolNumSum(i)
                    if (PK(j) == 1) then
                        numPK1(i) = numPK1(i) + 1
                    endif
                enddo
                numSumPK1 = numSumPK1 + numPK1(i)
            endif
        enddo
        
        write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numS",(selectSum(i,numTest),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numA",(usedSum(i,numTest),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PS",(selectRate(i),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PA",(usedRate(i),i=1,numPool)
        write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PK",(PK(i),i=1,numPool)
    enddo !結束迭代
    write(*,*) "========================== stop =========================="
    write(*,"(A10,I10,A10,I10,A10,I10,A10,F10.5)") "numIterate= ",numIterate, " numSumPK1 = ", numSumPK1,&
        " place = ", place," maxPA= ",usedRateMax

    write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numS",(selectSum(i,numTest),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numA",(usedSum(i,numTest),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PS",(selectRate(i),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PA",(usedRate(i),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PK",(PK(i),i=1,numPool)
    ! close(101)
    ! close(102)



    ! =======================================================
    ! =======================================================
    ! ========== 根據迭代所得的PK值再施測一次 =================
    ! 輸入受試者真實能力值
    open(100, file= dataPath2, status="old") 
    read(100,*)  ! 跳過第一列
    do i=1,numTest
        read(100,*) x, thetaTrue(i) !從資料讀取三參數
    enddo
    close(100)

    ! 重新施測須重設的值
    do try = 1,numTest
        do i = 1,numPool
            usedPool(i, try) = 0 
            usedSum(i, try) = 0
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
        usedTotalSum(try) = 0
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
                    ! elseif (contentPoolChange(contentGoal)>contentPoolLimit(contentGoal)) then
                    !     ! write(*,*) "content error!"
                    !     exit ! 避免出現無試題選出的狀況
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
                usedTotalSum(try) = usedPool(i,try)
            else
                selectSum(i,try) = selectSum(i,try-1) + selectPool(i,try) ! 被選擇(S)的累加次數
                usedSum(i,try) = usedSum(i,try-1) + usedPool(i,try) ! 被施測(A)的累加次數
                usedTotalSum(try) = usedTotalSum(try) + usedPool(i,try)
            endif
        enddo
    enddo
    ! 算出試題使用率(PA)與被選擇率(PS)
    do i=1, numPool
        usedRate(i) = REAL(usedSum(i,numTest))/numTest !PA
        selectRate(i) = REAL(selectSum(i,numTest))/numTest !PS
        if (PK(i)<1) then
            usedRate_PK1(i) = 0.0
            usedRate_PK0(i) = usedRate(i)
        elseif (PK(i)==1) then
            usedRate_PK1(i) = usedRate(i)
            usedRate_PK0(i) = 0.0
        endif
    enddo
    ! write(*,*) "usedRate          =", (usedRate(i),i=1,numPool)
    ! write(*,*) "selectRate        =", (selectRate(i),i=1,numPool)
    ! 算出 PK1&PK0 最大PA、PA平均值、PA變異數
    call subr_maxvReal(usedRate_PK1, numPool, usedRateMax_PK1, place) !取PA的最大值與相對位置
    call subr_maxvReal(usedRate_PK0, numPool, x, place) !取PA的最大值與相對位置
    usedRateMax_PK0 = x
    call subr_sumReal(usedRate_PK1, numPool, x)
    usedRateMean_PK1 = x/numSumPK1
    call subr_sumReal(usedRate_PK0, numPool, y)
    usedRateMean_PK0 = y/(numPool-numSumPK1)
    x = 0.0
    y = 0.0
    do i=1, numPool
        if (PK(i) == 1) x = x + (usedRate_PK1(i)-usedRateMean_PK1)**2
        if (PK(i) <  1) y = y + (usedRate_PK0(i)-usedRateMean_PK0)**2
    enddo
    usedRateVar_PK1 = x/REAL(numSumPK1)
    usedRateVar_PK0 = y/REAL(numPool-numSumPK1)
    
    ! 算出最大PA、PA平均值、PA變異數
    call subr_maxvReal(usedRate, numPool, usedRateMax, place) !取PA的最大值與相對位置
    call subr_aveReal(usedRate, numPool, usedRateMean)
    call subr_varReal(usedRate, numPool, usedRateVar)
    write(*,*) "Final place = ", place," Final maxPA= ",usedRateMax
    ! 算出題庫使用率&測驗重疊率
    call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate) !SH迭代題庫使用率
    call subr_testOverlap(place_choose, numTest, length, testOverlapData) ! 不知為何會受下面subr_maxvInt的影響，待查證
    testOverlap = testOverlapData ! test overlap
    ! 寫下資料
    write(unit = 101, fmt ="(A10,A50)") " ","========================="
    write(unit = 101, fmt ="(2I10,F10.5,I10,4F10.5)") numIterate , place ,usedRateMax,&
    numSumPK1,usedRateMean,usedRateVar,poolUsedRate,testOverlap 
    ! 寫下資料
    write(unit = 102, fmt ="(A10,A50)") " ","========================="
    write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numS",(selectSum(i,numTest),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400I10)") numIterate,"numA",(usedSum(i,numTest),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PS",(selectRate(i),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PA",(usedRate(i),i=1,numPool)
    write(unit = 102, fmt ="(I10,A10,400F10.5)") numIterate,"PK",(PK(i),i=1,numPool)
    close(101)
    close(102)

    call cpu_time (t2) !結束計時
    
    ! ====================================================
    ! ============= 計算並輸出所需資料 ====================
    ! thetaHat 計算 (根據OMST調整)
    call subr_aveReal(thetaTrue, numTest, thetaTrueMean)
    call subr_aveReal(thetaHat(numStages,:), numTest, thetaHatMean)
    thetaBias = thetaHatMean - thetaTrueMean
    call subr_varReal(thetaHat(numStages,:), numTest, thetaHatVar)
    call subr_mseReal(thetaHat(numStages,:), thetaTrue(:), numTest, x)
    thetaHatMSE = x
    ! ! item used rate 計算
    ! call subr_itemUsedRate(usedPool, numTest, numPool, usedRate)
    ! call subr_maxvReal(usedRate, numPool, usedRateMax, place)
    ! call subr_aveReal(usedRate, numPool, usedRateMean)
    ! call subr_varReal(usedRate, numPool, usedRateVar)
    ! ! item pool 計算
    ! call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate)
    ! ! test overlap
    ! call subr_testOverlap(place_choose, numTest, length, testOverlapData) 
    ! testOverlap = testOverlapData

    ! content mean 計算
    ! 上頭最後subr所得的值會受subr_maxvInt的影響，需另外存取，原因待查證
    do i = 1, numContentType
        call subr_aveIntToReal(contentResult(i,:), numTest, contentResultMean(i))
        call subr_maxvInt(contentResult(i,:), numTest, contentResultMax(i)) 
        call subr_minvInt(contentResult(i,:), numTest, contentResultMin(i))
    enddo
    ! == 計算Omega&Psi ==
    do try = 1, numTest
        call subr_testOmega(numTest,numPool,1,usedSum,length,try,omegaOne)
        call subr_testOmega(numTest,numPool,2,usedSum,length,try,omegaTwo)
        call subr_testOmega(numTest,numPool,3,usedSum,length,try,omegaThree)
        call subr_testPsi(numTest,numPool,1,usedSum,length,try,psiOne)
        call subr_testPsi(numTest,numPool,2,usedSum,length,try,psiTwo)
        call subr_testPsi(numTest,numPool,3,usedSum,length,try,psiThree)
    enddo
    call subr_aveReal(omegaOne, numTest, omegaOneMean)
    call subr_aveReal(omegaTwo, numTest, omegaTwoMean)
    call subr_aveReal(omegaThree, numTest, omegaThreeMean)
    call subr_maxvReal(omegaOne(2:numTest), numTest-1, omegaOneMax, place)
    call subr_maxvReal(omegaTwo(3:numTest), numTest-2, omegaTwoMax, place)
    call subr_maxvReal(omegaThree(4:numTest), numTest-3, omegaThreeMax, place)
    call subr_minvReal(omegaOne(2:numTest), numTest-1, omegaOnemin, place)
    call subr_minvReal(omegaTwo(3:numTest), numTest-2, omegaTwomin, place)
    call subr_minvReal(omegaThree(4:numTest), numTest-3, omegaThreemin, place)
    call subr_varReal(omegaOne(2:numTest), numTest-1, omegaOneVar)
    call subr_varReal(omegaTwo(3:numTest), numTest-2, omegaTwoVar)
    call subr_varReal(omegaThree(4:numTest), numTest-3, omegaThreeVar)
    call subr_aveReal(psiOne, numTest, psiOneMean)
    call subr_aveReal(psiTwo, numTest, psiTwoMean)
    call subr_aveReal(psiThree, numTest, psiThreeMean)
    call subr_maxvReal(psiOne(2:numTest), numTest-1, psiOneMax, place)
    call subr_maxvReal(psiTwo(3:numTest), numTest-2, psiTwoMax, place)
    call subr_maxvReal(psiThree(4:numTest), numTest-3, psiThreeMax, place)
    call subr_minvReal(psiOne(2:numTest), numTest-1, psiOnemin, place)
    call subr_minvReal(psiTwo(3:numTest), numTest-2, psiTwomin, place)
    call subr_minvReal(psiThree(4:numTest), numTest-3, psiThreemin, place)
    call subr_varReal(psiOne(2:numTest), numTest-1, psiOneVar)
    call subr_varReal(psiTwo(3:numTest), numTest-2, psiTwoVar)
    call subr_varReal(psiThree(4:numTest), numTest-3, psiThreeVar)
    ! mean of infor 計算
    do i = 1, numTest
        do j = 1, length
            choose_inforTrue(j,i) = information(thetaTrue(i), a_choose(j, i), b_choose(j, i), c_choose(j, i))
            choose_inforEstimate(j,i) = information(thetaHat(numStages,i), a_choose(j, i), b_choose(j, i), c_choose(j, i))
            if(j==1)then
                choose_inforTrueSum(i) = 0.0
                choose_inforTrueSum(i) = choose_inforTrue(j,i)
                choose_inforEstimateSum(i) = 0.0
                choose_inforEstimateSum(i) = choose_inforEstimate(j,i)
            else
                choose_inforTrueSum(i) = choose_inforTrueSum(i) + choose_inforTrue(j,i)
                choose_inforEstimateSum(i) = choose_inforEstimateSum(i) + choose_inforEstimate(j,i)
            endif
        enddo
    enddo
    call subr_aveReal(choose_inforTrueSum, numTest, testMean_inforTrue)
    call subr_aveReal(choose_inforEstimateSum, numTest, testMean_inforEstimate)
    ! === 輸出資料 ===
    open(unit = 100 , file = 'ListCAT_summary.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A30)') " OMST_with_cont&SH "
    write(unit = 100, fmt = '(A15,I15)') "stages", numStages
    write(unit = 100, fmt = '(A15,F15.5)') "time", t2-t1
    write(unit = 100, fmt = '(A15,I15)') "test_n", numTest
    write(unit = 100, fmt = '(A15,I15)') "pool_n", numPool
    write(unit = 100, fmt = '(A15,I15)') "length", length
    write(unit = 100, fmt = '(/,A30)') "ThetaHat_of_Estimates: "
    write(unit = 100, fmt = '(A15, F15.5)') "Mean", thetaHatMean
    write(unit = 100, fmt = '(A15, F15.5)') "Bias", thetaBias
    write(unit = 100, fmt = '(A15, F15.5)') "Var", thetaHatVar
    write(unit = 100, fmt = '(A15, F15.5)') "MSE", thetaHatMSE
    write(unit = 100, fmt = '(A15, F15.5)') "RMSE", thetaHatMSE**0.5
    write(unit = 100, fmt = '(/,A30)') "Item_Exposure_Rate: "
    write(unit = 100, fmt = '(A15, F15.5)') "max", usedRateMax
    write(unit = 100, fmt = '(A15, F15.5)') "mean", usedRateMean
    write(unit = 100, fmt = '(A15, F15.5)') "var", usedRateVar
    write(unit = 100, fmt = '(/,A30)') "Pool_Used: "
    write(unit = 100, fmt = '(A15, F15.5)') "Rate", poolUsedRate
    write(unit = 100, fmt = '(/,A30)') "Test_Overlap: "
    write(unit = 100, fmt = '(A15, F15.5)') "overlap", testOverlap
    write(unit = 100, fmt = '(/,A30)') "Mean_of_Infor: "
    write(unit = 100, fmt = '(A15, F15.5)') "True", testMean_inforTrue
    write(unit = 100, fmt = '(A15, F15.5)') "Estimate", testMean_inforEstimate
    write(unit = 100, fmt = '(/,A30)') "Psi_max"
    write(unit = 100, fmt = '(A15, F15.5)') "Set", 1.0
    write(unit = 100, fmt = '(A15, I15)') "alpha", 1
    write(unit = 100, fmt = '(A15, F15.5)') "Max_1", psiOneMax
    write(unit = 100, fmt = '(A15, F15.5)') "Max_2", psiTwoMax
    write(unit = 100, fmt = '(A15, F15.5)') "Max_3", psiThreeMax
    write(unit = 100, fmt = '(/,A30)') "r_max_of_SH: "
    write(unit = 100, fmt = '(A15, F15.5)') "rMax_Set", rMAX
    write(unit = 100, fmt = '(A15, F15.5)') "stop_minMaxPA", stop_minMaxPA
    write(unit = 100, fmt = '(A15, I15)') "sum_numPK1", numSumPK1
    write(unit = 100, fmt = '(A15, 5I5)') "numPK1", (numPK1(i),i=1,numContentType)
    write(unit = 100, fmt = '(/,A30)') "Item_Exposure_Rate_of_PK1: "
    write(unit = 100, fmt = '(A15, F15.5)') "max", usedRateMax_PK1
    write(unit = 100, fmt = '(A15, F15.5)') "mean", usedRateMean_PK1
    write(unit = 100, fmt = '(A15, F15.5)') "var", usedRateVar_PK1
    write(unit = 100, fmt = '(/,A30)') "Item_Exposure_Rate_of_PK0: "
    write(unit = 100, fmt = '(A15, F15.5)') "max", usedRateMax_PK0
    write(unit = 100, fmt = '(A15, F15.5)') "mean", usedRateMean_PK0
    write(unit = 100, fmt = '(A15, F15.5)') "var", usedRateVar_PK0
    close(100)
    ! == theta hat ==
    open(unit = 100 , file = 'ListCAT_theta.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "thetaHat = "
    write(unit = 100, fmt = dataINT) (j, j=1,numStages)
    do i=1,numTest
        write(unit = 100, fmt = dataF) (thetaHat(j,i),j=1,numStages)
    end do
    close (100)
    ! == response ==
    open(unit = 100 , file = 'ListCAT_resp.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "response = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataINT) (resp(j,i),j=1,length)
    end do
    close(100)
    ! == item choose ==
    open(unit = 100 , file = 'ListCAT_item.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "choose item = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataINT) (place_choose(j,i),j=1,length)
    end do
    close(100)
    ! == content ==
    open(unit = 100 , file = 'ListCAT_content.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "choose content = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataINT) (content_choose(j,i),j=1,length)
    end do
    close(100)
    ! == content summary ==
    open(unit = 100 , file = 'ListCAT_contentSum.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "type = "
    write(unit = 100, fmt = dataContentINT) (j, j=1,numContentType)
    write(unit = 100, fmt = '(A)') "mean = "
    write(unit = 100, fmt = dataContentReal) (contentResultMean(j),j=1,numContentType)
    write(unit = 100, fmt = '(A)') "max = "
    write(unit = 100, fmt = dataContentINT) (contentResultMax(j),j=1,numContentType)
    write(unit = 100, fmt = '(A)') "min = "
    write(unit = 100, fmt = dataContentINT) (contentResultMin(j),j=1,numContentType)
    write(unit = 100, fmt = '(/,A)') "content sum = "
    do i=1,numTest
        write(unit = 100, fmt = dataContentINT) (contentResult(j,i),j=1,numContentType)
    end do
    close(100)
    ! == pool used ==
    open(unit = 100 , file = 'ListCAT_poolUsed.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "pool used = "
    write(unit = 100, fmt = dataPool) (j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPool) (usedPool(j,i),j=1,numPool)
    end do
    close(100)
    open(unit = 100 , file = 'ListCAT_poolUsedSum.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "pool used sum = "
    write(unit = 100, fmt = dataPool_T) "SUM",(j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPool) usedTotalSum(i),(usedSum(j,i),j=1,numPool)
    end do
    close(100)
    ! == Omega & Psi ==
    open(unit = 100 , file = 'ListCAT_testOmega&Psi.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(7A10)') "Stat.", "Omega 1", "Omega 2", "Omega 3", "Psi 1", "Psi 2", "Psi 3"
    write(unit = 100, fmt = '(A10,6F10.5)') "Mean", omegaOneMean, omegaTwoMean, omegaThreeMean,&
    psiOneMean, psiTwoMean, psiThreeMean
    write(unit = 100, fmt = '(A10,6F10.5)') "Max", omegaOneMax, omegaTwoMax, omegaThreeMax,&
    psiOneMax, psiTwoMax, psiThreeMax
    write(unit = 100, fmt = '(A10,6F10.5)') "Min", omegaOneMin, omegaTwoMin, omegaThreeMin,&
    psiOneMin, psiTwoMin, psiThreeMin
    write(unit = 100, fmt = '(A10,6F10.5)') "SD", omegaOneVar**0.5, omegaTwoVar**0.5, omegaThreeVar**0.5,&
    psiOneVar**0.5, psiTwoVar**0.5, psiThreeVar**0.5
    write(unit = 100, fmt = '(A10,6F10.5)') "Last", omegaOne(numTest), omegaTwo(numTest), omegaThree(numTest),&
    psiOne(numTest), psiTwo(numTest), psiThree(numTest)
    write(unit = 100, fmt = '(/,A)') "data = "
    do i=1,numTest
        write(unit = 100, fmt = '(6F10.5)') omegaOne(i), omegaTwo(i), omegaThree(i),&
        psiOne(i), psiTwo(i), psiThree(i)
    end do
    close(100)
    stop
    
end program