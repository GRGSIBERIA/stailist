!  QUESTION.f90 
!
!  関数:
!  QUESTION - 質問文を準備するモジュール
!

!****************************************************************************
!
!  プログラム: QUESTION
!
!  目的:  質問文を準備する
!
!****************************************************************************
    
    module Question
        implicit none
        character, public :: ask(20)*90
        character, private :: command*10
        integer, private :: is_japanese ! 日本語-英語の切り替え
        integer, private :: is_state    ! STATE-TRAITの切り替え
        integer, private :: point       ! 得点
        integer, private :: command_line_count
        
    contains
        subroutine initialize_question()
            ! コマンドライン引数を受け取ってそれに合致した変数値を代入する
            DO command_line_count = 1, iargc()
                CALL GETARG(command_line_count, command)
            
                ! 日本語と英語の判別
                IF (command == "-ja" .or. command == "-jp" .or. command == "-j") THEN
                    is_japanese = 1
                    print *, "日本語モード"
                    GOTO 100
                ELSE IF (command == "-en" .or. command == "-e") THEN
                    is_japanese = 0
                    print *, "ENGLISH MODE"
                    GOTO 100
                END IF
            
                ! State-Traitの判別
                IF (command == "-state" .or. command == "-s") THEN
                    is_state = 1
                    GOTO 100
                ELSE IF (command == "-trait" .or. command == "-t") THEN
                    is_state = 0
                    GOTO 100
                END IF
            
    100         CONTINUE ! カット
            END DO
            
            IF (is_state == 0 .and. is_japanese == 0) THEN
                print *, "RUNNING STATE"
            ELSE IF (is_state == 1 .and. is_japanese == 0) THEN
                print *, "RUNNING TRAIT"
            ELSE IF (is_state == 0 .and. is_japanese == 1) THEN
                print *, "状態尺度"
            ELSE IF (is_state == 1 .and. is_japanese == 1) THEN
                print *, "状況尺度"
            END IF
        end subroutine
    end module