program main_OMST_cont_SH
    use ran_mod !呼叫常態隨機模組
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_300.txt"
    character(len = 50), parameter :: dataPath2 = "data/Population_Normal.txt"!Uniform Normal
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數
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
    integer :: contentGoal
    integer :: contentTarget(numContentType) != (/16,16,8/)
    integer :: contentChange(numContentType) 
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
    
    ! === content 相關 ===
    integer:: content_choose(length, numTest)
    integer, dimension(numContentType, numTest)::contentResult
    real, dimension(numContentType)::contentResultMean
    integer, dimension(numContentType)::contentResultMax
    integer, dimension(numContentType)::contentResultMin
    ! === 估計能力參數 ===
    real::thetaHat(numStages, numTest)
    real::thetaHatMean !估計能力值的平均數
    real::thetaBias !估計能力值與真值的差之平均
    real::thetaHatVar !估計能力值的變異數
    real::thetaHatMSE !估計能力值的MSE

    ! 曝光率控管參數設定
    real::rMAX = 0.2
    real::stop_rMAX = 0.22 !=rMAX + rMAX*0.1
    real::stop_minMAXPA = 1 !紀錄迭代最小的maxPA
    integer::stop_NUM = 0
    real:: PK(numPool) = (/(1.0,i=1,numPool)/) !所有曝光率參數 初始值=1
    integer::numPK1 = numPool !初始值
    real::rand_SH !用於SH法的亂數
    
    integer :: itemID !SH新增
    integer::numIterate !迭代次數 !SH新增
    integer, dimension(numPool)::selectPool !SH新增
    
    !------------------
    !題庫試題選中與使用次數
    integer:: countS(numPool) !選中
    integer:: countA(numPool) !使用
    integer::countPOOL(numPool) !題庫使用
    real:: PS(numPool) !選中 Select
    real:: PA(numPool) !使用 Administer
    real:: PAmax
    integer::sumS !檢驗用
    integer::sumA !檢驗用
    integer::sumPOOL !檢驗用
    real::PoolUR !題庫使用率
    real::PAvariance
    real::PAmean

    integer :: usedPool(numPool, numTest) !紀錄試題是否被使用過
    integer :: usedSum(numPool, numTest) !試題被使用過的累加次數

    ! === 選題的試題參數 ===
    real:: a_choose(length, numTest),b_choose(length, numTest), &
    c_choose(length, numTest) 

    ! === 運算暫存用 ===
    real :: maxv !最大值 
    integer :: place
    real :: infor(numPool) !題庫各試題的訊息量
    real :: randv(length, numTest)
    ! === output data ===
    integer :: resp(length, numTest) !作答反應
    integer:: place_choose(length, numTest) !選題的試題位置
    ! SH 鬆綁參數    


    ! 試題使用率
    real:: usedRate(numPool) 
    real:: usedRateMax !maxPA
    real:: usedRateMean !PAmean
    real:: usedRateVar !PAvar
    ! 測驗重疊率參數
    real:: testOverlapData
    real:: testOverlap
    ! item pool 的相關資料紀錄
    real :: poolUsedRate !PoolUR

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
    
    !  判斷題庫是否有試題可選 
    integer :: count_InforZero
    
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
        thetaTrue( i ) = normal( REAL(thetaTrueMean,KIND = 8), 1.0D0 ) !自行生成
    enddo

    !==================
    !運算過程中，寫下SH曝光率迭代資料
    !==================
    open(101,file="summery_SH_maxPA.txt",status="replace")
    write(101,"(11A10)") "freq","item","PAmax","numPK1","sumS","sumA","sumPOOL","OverLap","PAmean","PAvar","PoolUR"

    !寫下SH曝光率迭代資料 PK_test
    open(102,file="summery_SH_PK_test.txt",status="replace")
    write(102,"(2A10,400I10)") "Iterate","data",(i,i=1,numPool)
    
    
    ! 開始模擬
    ! set module content constraint
    contentTarget = contentScale*contentMultiplier
    numIterate=0 !紀錄迭代次數

    !開始SH的迭代
    do while( .true. )

        !重設使用次數
        do itemID = 1, numPool
            countS(itemID) = 0
            countA(itemID) = 0
            countPOOL(itemID) = 0
        enddo

        !重設試題使用狀況
        do try = 1, numTest
            do itemID = 1, numPool
                usedPool(itemID, try) = 0
                usedSum(itemID, try) = 0
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
            do i = 1, numPool
                selectPool(i) = 0
            enddo
            
            !SH曝光率控制
            do while (.true.)
                do usedStages = 1, numStages
                    do choose = 1, length/numStages
                        ! 隨機選擇要施測的內容領域
                        if ( choose == 1 ) then
                            contentChange = contentTarget ! 重設內容領域控制參數
                        endif
                        call subr_contentTargetP(contentChange, numContentType, contentTP)
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
                        ! 計算訊息量
                        call random_number(rand_SH) !產生用於SH的隨機變數
                        if ( usedStages == 1 ) then       
                            do i = 1, numPool
                                if ( ( usedPool(i, try) == 0 ) .AND. &
                                (content(i) == contentGoal) .AND. &
                                (selectPool(i) == 0)) then
                                    infor(i) = information(thetaBegin, a(i), b(i), c(i))
                                else
                                    infor(i) = 0
                                endif
                            enddo
                        else
                            do i = 1, numPool
                                if ( ( usedPool(i, try) == 0 ) .AND. &
                                ( content(i) == contentGoal ) .AND. &
                                (selectPool(i) == 0)) then
                                    infor(i) = information(thetaHat(usedStages-1, try), a(i), b(i), c(i))
                                else
                                    infor(i) = 0
                                endif
                            enddo
                        endif
                        realChoose = realChoose + 1
                        !realChoose = (usedStages-1)*(length/numStages)+choose
                        call subr_maxvReal(infor, numPool, maxv, place_choose(realChoose, try)) ! 求出最大訊息量與其題庫ID(紀錄使用的試題題號)
                        !等式成立，則跳離此迴圈
                        if (rand_SH <= PK(place_choose(realChoose, try))) then
                            exit
                        else
                            realChoose = realChoose - 1
                            selectPool(place_choose(realChoose, try)) = 1
                        endif                                
                    enddo    
                    usedPool(place_choose(realChoose, try), try) = 1 !紀錄使用試題
                    ! 紀錄使用的試題參數
                    a_choose(realChoose, try) = a(place_choose(realChoose, try))
                    b_choose(realChoose, try) = b(place_choose(realChoose, try))
                    c_choose(realChoose, try) = c(place_choose(realChoose, try))
                    content_choose(realChoose, try) = content(place_choose(realChoose, try))
                    ! 模擬作答反應
                    call subr_resp(thetaTrue(try), &
                    a_choose(realChoose, try),b_choose(realChoose, try),c_choose(realChoose, try),&
                    resp(realChoose, try),randv(realChoose, try))
                enddo

                ! 紀錄試題累計使用次數
                do i=1, numPool
                    if ( try == 1 ) then
                        usedSum(i,try) = usedPool(i,try)
                    else
                        usedSum(i,try) = usedSum(i,try-1) + usedPool(i,try)
                    endif
                enddo
            ! 計算每位受試者於不同內容領域中用了幾題
            do j=1,numContentType
                call subr_contentCount(content_choose(:,try),length,j,contentResult(j,try))
            enddo
            ! EAP能力估計
            call subr_EAP(realChoose, &
            a_choose(1:usedStages*(length/numStages), try),&
            b_choose(1:usedStages*(length/numStages), try),&
            c_choose(1:usedStages*(length/numStages), try),&
            resp(1:usedStages*(length/numStages), try), thetaHat(usedStages, try))
        enddo
        enddo
        
    enddo





    call cpu_time (t2) !結束計時

    





end program main_OMST_cont_SH