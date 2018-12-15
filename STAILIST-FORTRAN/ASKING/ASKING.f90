!  ASKING.f90 
!
!  関数:
!  ASKING - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: ASKING
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************
    
    program ASKING
        use Question
        implicit none
        
        ! 変数
        CALL initialize_question()
        
        print *, 'Hello World'

    end program ASKING
    
     

    ! 0    5    0    5    0    5    0    5    0    5    0
    ! I feel calm*
    ! I feel secure*
    ! I am tense
    ! I am regretful
    ! I feel at ease*
    ! I feel upset
    ! I am presently worrying over possible misfortunes
    ! I feel rested*
    ! I feel anxious
    ! I feel comfortable*
    ! I feel self-confident*
    ! I feel nevous
    ! I am jittery
    ! I feel "high strung"
    ! I am relaxed*
    ! I feel content*
    ! I am worried
    ! I feel over-excited and "rattled"
    ! I feel joyful*
    ! I feel pleasant*
    ! 0    5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
    ! I feel pleasant*
    ! I tire quickly
    ! I feel like crying
    ! I wish I could be as happy as others seem to be
    ! I am losing out on things because I can't make up my mind soon enough
    ! I feel rested*
    ! I am "calm, cool and coollected"*
    ! I feel that difficulties are piling up so that I cannot overcome them
    ! I worry too much over something that really doesn't matter
    ! I am happy*
    ! I am inclined to take things hard
    ! I lack self-confidence
    ! I feel secure*
    ! I try to avoid facing a crisis or difficulty
    ! I feel blue
    ! I am content*
    ! Some unimportant thought runs through my mind and bothers me
    ! I take disappointments so keenly that I can't put them out of my mind
    ! I am a steady person*
    ! I get in a state of tension or turmoil as I think over my recent concerns and interests