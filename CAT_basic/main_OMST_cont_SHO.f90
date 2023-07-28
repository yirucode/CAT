program main_OMST_cont_SH
    use ran_mod !呼叫常態隨機模組
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_300.txt"
    character(len = 50), parameter :: dataPath2 = "data/Population_Normal.txt"!Uniform Normal
    ! === parameter ===
    integer,parameter :: numTest = 500 !重複次數
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
    real(kind=8) :: thetaTrue(numTest) = 1. !真實能力值
    real(kind=8) :: thetaTrueMean = 0. !真實能力值之平均
    real(kind=8) :: thetaBegin = 0.
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
    real(kind=8)::thetaHat(numStages, numTest)
    real(kind=8)::thetaHatMean !估計能力值的平均數
    real(kind=8)::thetaBias !估計能力值與真值的差之平均
    real(kind=8)::thetaHatVar !估計能力值的變異數
    real(kind=8)::thetaHatMSE !估計能力值的MSE
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
    real:: rand_SH !亂數；用於SH法
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
    ! integer::sumS 
    ! integer::sumA 
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
    
    ! ! 輸入受試者真實能力值
    ! open(100, file= dataPath2, status="old") 
    ! read(100,*)  ! 跳過第一列
    ! do i=1,numTest
    !     read(100,*) x, thetaTrue(i) !從資料讀取三參數
    ! enddo
    ! close(100)

    ! 產生模擬的受試者真實能力值
    do i=1,numTest
        ! thetaTrue( i ) = normal( REAL(thetaTrueMean,KIND = 8), 1.0D0 ) !自行生成
        thetaTrue( i ) = normal( REAL(thetaTrueMean, KIND = 8), 1.0D0 ) !自行生成
    enddo

    !==================
    !運算過程中，寫下SH曝光率迭代資料
    !==================
    ! open(unit = 102 , file = 'summery_SH_maxPA.txt' , status = 'replace', action = 'write', iostat= ierror)
    ! write(unit = 102, fmt = '(A)') "type = "
    ! close(102)
    ! write(unit = 101, fmt ='(8A10)') "freq","item","PAmax","numPK1","OverLap","PAmean","PAvar","PoolUR" 
    !寫下SH曝光率迭代資料 PK_test
    open(unit = 103 , file = 'summery_SH_PK_test.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 103, fmt ="(2A10,300I10)") "Iterate","data",(i,i=1,numPool)
    ! close(103)
    ! open(unit = 102, file = 'summery_SH_maxPA.txt' , status = 'unknown')
    ! open(unit = 103, file = 'summery_SH_PK_test.txt' , status = 'unknown')
    ! 開始模擬
    ! set module content constraint
    contentTarget = contentScale*contentMultiplier
    !開始SH的迭代
    do while( .true. )

        !重設試題使用狀況
        do try = 1, numTest
            do itemID = 1, numPool
                usedPool(itemID, try) = 0
                usedSum(itemID, try) = 0
                selectPool(itemID, try) = 0
                selectSum(itemID, try) = 0
            enddo
            do realChoose = 1, length
                a_choose(realChoose, try) = 0
                b_choose(realChoose, try) = 0
                c_choose(realChoose, try) = 0
                content_choose(realChoose, try) = 0
                resp(realChoose, try) = 0
            enddo
        enddo

        
        ! 開始施測
        do try = 1, numTest
            realChoose = 0
            do usedStages = 1, numStages
                do choose = 1, length/numStages

                    ! 內容領域相關設定
                    if ( choose == 1 ) then
                        do i = 1, numContentType
                            contentPoolChange(i) = 0
                        enddo
                        contentChange = contentTarget ! 重設內容領域控制參數
                        contentPoolLimit = contentPoolNum - contentChange ! 題庫各領域剩餘可選的數量
                    endif
                    call subr_contentTargetP(contentChange, numContentType, contentTP)

                    !SH曝光率控制
                    do while (.true.)
                        ! 隨機選擇要施測的內容領域
                        call random_number(randContent)
                        !WRITE(*,*) randContent
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
                        contentChange(contentGoal) = contentChange(contentGoal)-1 ! 刪除選中的內容題數
                        contentPoolChange(contentGoal) = contentPoolChange(contentGoal) +1
                        contentPoolLimit(contentGoal) = contentPoolLimit(contentGoal) -1
                        
                        ! 計算訊息量
                        call random_number(rand_SH) !產生用於SH的隨機變數
                        if ( usedStages == 1 ) then       
                            do i = 1, numPool
                                if (content(i) <= numContentType) then
                                    if ( ( usedPool(i, try) == 0 ) .AND. &
                                    (content(i) == contentGoal) .AND. &
                                    (selectPool(i, try) == 0)) then
                                        infor(i) = information(thetaBegin, a(i), b(i), c(i))
                                    else
                                        infor(i) = 0
                                    endif
                                else
                                    print*, "content error!"
                                    stop
                                endif
                            enddo
                        else
                            do i = 1, numPool
                                if (content(i) <= numContentType) then
                                    if ( ( usedPool(i, try) == 0 ) .AND. &
                                    ( content(i) == contentGoal ) .AND. &
                                    (selectPool(i, try) == 0)) then
                                        infor(i) = information(thetaHat(usedStages-1, try), a(i), b(i), c(i))
                                    else
                                        infor(i) = 0
                                    endif
                                else
                                    print*, "content error!"
                                    stop
                                endif
                            enddo
                        endif
                        realChoose = realChoose + 1
                        ! realChoose = (usedStages-1)*(length/numStages) + choose
                        call subr_maxvReal(infor, numPool, maxv, place_choose(realChoose, try)) ! 求出最大訊息量與其題庫ID(紀錄使用的試題題號)
                        ! 紀錄被選擇次數
                        selectPool(place_choose(realChoose, try), try) = 1
                        if (try == 1) then
                            selectSum(place_choose(realChoose, try), try) = 1
                        else
                            selectSum(place_choose(realChoose, try), try) = selectSum(place_choose(realChoose, try), try-1) + 1
                        endif
                        ! 需在content題數有限的狀況選出試題
                        if (contentPoolChange(contentGoal) < (contentPoolLimit(contentGoal)-contentChange(contentGoal))) then
                            !等式成立，則跳離此迴圈
                            if (rand_SH <= PK(place_choose(realChoose, try))) then
                                exit
                            else
                                contentChange(contentGoal) = contentChange(contentGoal)+1 ! 恢復選中的內容題數
                                contentPoolChange(contentGoal) = contentPoolChange(contentGoal) -1
                                contentPoolLimit(contentGoal) = contentPoolLimit(contentGoal) +1
                                realChoose = realChoose -1
                            endif 
                        else
                            exit
                        endif
                    enddo 
                    ! 成功選題後，紀錄選出的試題
                    usedPool(place_choose(realChoose, try), try) = 1 !紀錄使用試題
                    ! 紀錄使用的試題參數
                    a_choose(realChoose, try) = a(place_choose(realChoose, try))
                    b_choose(realChoose, try) = b(place_choose(realChoose, try))
                    c_choose(realChoose, try) = c(place_choose(realChoose, try))
                    content_choose(realChoose, try) = content(place_choose(realChoose, try))
                    ! 模擬作答，並記錄作答反應
                    call subr_resp(thetaTrue(try), &
                    a_choose(realChoose, try),b_choose(realChoose, try),c_choose(realChoose, try),&
                    resp(realChoose, try),randv(realChoose, try))
                    ! 紀錄試題累計使用次數與使用率
                    do i=1, numPool
                        if ( try == 1 ) then
                            usedSum(i,try) = usedPool(i,try)
                            usedRate(i) = usedSum(i,try)/try
                        else
                            usedSum(i,try) = usedSum(i,try-1) + usedPool(i,try)
                            usedRate(i) = usedSum(i,try)/try
                        endif
                    enddo
                enddo
                ! EAP能力估計
                call subr_EAP(realChoose, &
                a_choose(1:usedStages*(length/numStages), try),&
                b_choose(1:usedStages*(length/numStages), try),&
                c_choose(1:usedStages*(length/numStages), try),&
                resp(1:usedStages*(length/numStages), try), thetaHat(usedStages, try))
            enddo
            ! 計算每位受試者於不同內容領域中用了幾題
            do j=1,numContentType
                call subr_contentCount(content_choose(:,try),length,j,contentResult(j,try))
            enddo
        enddo

        ! === 施測所有受試者後 ===!
        ! 記錄迭代次數
        numIterate = numIterate + 1 
        ! 算出最大PA
        call subr_maxvReal(usedRate, numPool, usedRateMax, place) !取PA的最大值與相對位置
        write(*,*) "numIterate= ",numIterate, "; maxPA= ",usedRateMax
        ! if (numIterate > 50) then
        !     write(*,*) "numIterate too"
        ! endif
        ! 紀錄迭代最小的maxPA
        ! if ( usedRateMax < stop_minMaxPA ) then
        !     stop_minMaxPA = usedRateMax
        !     write(*,*) "min(maxPA)= ",usedRateMax
        ! end if

        ! item selsected rate 
        selectRate = REAL(selectSum(:, numTest))/numTest
        ! do i = 1, numPool
        !     selectRate(i) = REAL(selectSum(i, numTest))/numTest
        ! enddo
        ! item used rate 計算
        call subr_itemUsedRate(usedPool, numTest, numPool, usedRate)
        call subr_maxvReal(usedRate, numPool, usedRateMax, place)
        call subr_aveReal(usedRate, numPool, usedRateMean)
        call subr_varReal(usedRate, numPool, usedRateVar)
        ! 迭代題庫使用率
        call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate) !SH迭代題庫使用率
        ! test overlap
        call subr_testOverlap(place_choose, numTest, length, testOverlapData) ! 不知為何會受下面subr_maxvInt的影響，待查證
        testOverlap = testOverlapData
        !寫下SH曝光率迭代資料
        ! write(unit = 102, fmt = "(2I10,F10.5,I10,4F10.5)") numIterate,place,usedRateMax,numPK1,testOverlap,&
        ! usedRateMean,usedRateVar,poolUsedRate
        
        ! print*, numIterate,place,usedRateMax,numPK1,testOverlap,usedRateMean,usedRateVar,poolUsedRate
        
        !此式成立，則跳離此迴圈
        !if (maxPA < stop_rMAX) exit
        !要出現5次此情形，才會跳出迴圈
        if (usedRateMax < stop_rMAX) then
            stop_NUM = stop_NUM +1
        end if
        if (stop_NUM >= 5) then
            ! if (usedRateMax <= stop_minMaxPA) exit
            exit
        end if 
        !SH曝光率控制 PK值調整
        call setPK(selectRate,numPool,rMAX,length,PK,numPK1)
        
        !寫下SH曝光率迭代資料 PK_test(依題庫試題數調整)
        ! write(102,"(I10,A10,400F10.5)") numIterate,"PS",(PS(i),i=1,col)
        write(unit = 103, fmt = "(I10,A10,400F10.5)") numIterate,"PK",(PK(i),i=1,numPool)

    enddo

    close(102)
    close(103)

    ! 再施測一次


    call cpu_time (t2) !結束計時
    ! thetaHat 計算 (根據OMST調整)
    call subr_aveReal(thetaTrue, numTest, thetaTrueMean)
    call subr_aveReal(thetaHat(numStages,:), numTest, thetaHatMean)
    thetaBias = thetaHatMean - thetaTrueMean
    call subr_varReal(thetaHat(numStages,:), numTest, thetaHatVar)
    call subr_mseReal(thetaHat(numStages,:), thetaTrue(:), numTest, thetaHatMSE)
    ! item used rate 計算
    call subr_itemUsedRate(usedPool, numTest, numPool, usedRate)
    call subr_maxvReal(usedRate, numPool, usedRateMax, place)
    call subr_aveReal(usedRate, numPool, usedRateMean)
    call subr_varReal(usedRate, numPool, usedRateVar)
    ! item pool 計算
    call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate)
    ! test overlap
    call subr_testOverlap(place_choose, numTest, length, testOverlapData) ! 不知為何會受下面subr_maxvInt的影響，待查證
    testOverlap = testOverlapData
    ! content mean 計算
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
            choose_inforTrueSum(i) = choose_inforTrueSum(i) + choose_inforTrue(j,i)
            choose_inforEstimateSum(i) = choose_inforEstimateSum(i) + choose_inforEstimate(j,i)
        enddo
    enddo
    call subr_aveReal(choose_inforTrueSum, numTest, testMean_inforTrue)
    call subr_aveReal(choose_inforEstimateSum, numTest, testMean_inforEstimate)
    ! === 輸出資料 ===
    open(unit = 100 , file = 'ListCAT_summary.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "OMST_with_content_balance"
    write(unit = 100, fmt = '(A10,I10)') "stages", numStages
    write(unit = 100, fmt = '(A10,F10.5)') "time", t2-t1
    write(unit = 100, fmt = '(A10,I10)') "test_n", numTest
    write(unit = 100, fmt = '(A10,I10)') "pool_n", numPool
    write(unit = 100, fmt = '(A10,I10)') "length", length
    write(unit = 100, fmt = '(/,A)') "ThetaHat_of_Estimates: "
    write(unit = 100, fmt = '(A10, F10.5)') "Mean", thetaHatMean
    write(unit = 100, fmt = '(A10, F10.5)') "Bias", thetaBias
    write(unit = 100, fmt = '(A10, F10.5)') "Var", thetaHatVar
    write(unit = 100, fmt = '(A10, F10.5)') "MSE", thetaHatMSE
    write(unit = 100, fmt = '(A10, F10.5)') "RMSE", thetaHatMSE**0.5
    write(unit = 100, fmt = '(/,A)') "Item_Exposure_Ratee: "
    write(unit = 100, fmt = '(A10, F10.5)') "max", usedRateMax
    write(unit = 100, fmt = '(A10, F10.5)') "mean", usedRateMean
    write(unit = 100, fmt = '(A10, F10.5)') "var", usedRateVar
    write(unit = 100, fmt = '(/,A)') "Pool_Used: "
    write(unit = 100, fmt = '(A10, F10.5)') "Rate", poolUsedRate
    write(unit = 100, fmt = '(/,A)') "Test_Overlap: "
    write(unit = 100, fmt = '(A10, F10.5)') "overlap", testOverlap
    write(unit = 100, fmt = '(/,A)') "Mean_of_Infor: "
    write(unit = 100, fmt = '(A10, F10.5)') "True", testMean_inforTrue
    write(unit = 100, fmt = '(A10, F10.5)') "Estimate", testMean_inforEstimate
    write(unit = 100, fmt = '(/,A)') "Psi_max"
    write(unit = 100, fmt = '(A10, F10.5)') "Set", 1.0
    write(unit = 100, fmt = '(A10, I10)') "alpha", 1
    write(unit = 100, fmt = '(A10, F10.5)') "Max_1", psiOneMax
    write(unit = 100, fmt = '(A10, F10.5)') "Max_2", psiTwoMax
    write(unit = 100, fmt = '(A10, F10.5)') "Max_3", psiThreeMax
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
    write(unit = 100, fmt = dataPool) (j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPool) (usedSum(j,i),j=1,numPool)
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

end program main_OMST_cont_SH