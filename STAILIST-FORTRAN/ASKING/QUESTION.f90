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
        character, private :: ask(20)*100   ! 質問項目（ランダム化）
        integer, private :: answer(20)      ! 被験者の回答（正規化済み）
        integer, private :: number(20)      ! 質問項目番号（ランダムにする前の番号）
        integer, private :: rotation(20)    ! 明るい回答フラグ（回答を正規化するフラグ）
        integer, parameter :: N = 20        ! 質問項目数
        integer, private :: is_japanese = 1 ! 日本語-英語の切り替え
        integer, private :: is_state = 1    ! STATE-TRAITの切り替え
        integer, private :: point = 0       ! 得点
        
    contains
        subroutine initialize_question()
            character command*10
            integer command_line_count, count
            
            DO count=1, N
                number(count) = count
            END DO
            
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
            
            ! 質問項目の反転を設定
            if (is_state == 0) THEN
                CALL set_trait_rotation()
            ELSE
                CALL set_state_rotation()
            END IF
            
            ! 聞く内容を提示する
            IF (is_state == 0 .and. is_japanese == 0) THEN
                print *, "ASKING TRAIT SCALE"
                CALL trait_english()
            ELSE IF (is_state == 1 .and. is_japanese == 0) THEN
                print *, "ASKING STATE SCALE"
                CALL state_english()
            ELSE IF (is_state == 0 .and. is_japanese == 1) THEN
                print *, "特性不安について尋ねます"
                CALL trait_japanese()
            ELSE IF (is_state == 1 .and. is_japanese == 1) THEN
                print *, "状態不安について尋ねます"
                CALL state_japanese()
            END IF
            
            ! 用意したaskとrotationの順番をランダム化する
            CALL shuffle()
        end subroutine
        
        subroutine shuffle()
            character :: tmp*100
            integer :: i, j, rottemp, seed_size
            integer, allocatable :: seed(:)
            real r
            
            ! ランダムのシード値を変更する
            CALL RANDOM_SEED(size=seed_size)
            allocate(seed(seed_size))
            DO i = 1, seed_size
                call SYSTEM_CLOCK(count=seed(i))
            END DO
            CALL RANDOM_SEED(put=seed(:))
            
            ! 配列をランダムにシャッフルする
            DO i = 1, N     ! もとはN-1だった
                CALL RANDOM_NUMBER(r)
                j = r * (i + 1) + 1
                if (N < j) THEN
                    j = N       ! こうしないとN番目の要素がシャッフルされないバグを見つける
                END IF
                
                tmp = ask(i)
                ask(i) = ask(j)
                ask(j) = tmp
                
                rottemp = rotation(i)
                rotation(i) = rotation(j)
                rotation(j) = rottemp
                
                rottemp = number(i)
                number(i) = number(j)
                number(j) = rottemp
            END DO
            deallocate(seed)
        end subroutine
        
        subroutine evaluate()
            real normalized
            normalized = point / (N*4.0)
            IF (is_japanese == 1) THEN
                print *, "あなたの得点は", normalized, "です"
            ELSE
                print *, "Your number of point is ", normalized
            END IF
        end subroutine
        
        subroutine tell_answer(index)
            integer, intent(in) :: index
            integer ans
200         print *, "---------------------------------"
            
            IF (is_japanese == 1) THEN
                print *, "1 (まったくない), 2 (いくらかある), 3 (かなりある), 4(はっきりある)"
                print *, "質問項目：", ask(index)
            ELSE
                print *, "1 (NOT AT ALL), 2 (SOMEWHAT), 3 (MODERATELY SO), 4 (VERY MUCH SO)"
                print *, "QUESTION: "
            END IF
            
            read *, ans
            
            ! 回答のバリデーション
            IF (ans > 4 .or. ans < 1) THEN
                IF (is_japanese == 1) THEN
                    print *, "1～4 までの値を入力してください"
                ELSE
                    print *, "Please fill in the value between 1 and 4."
                END IF
                GOTO 200
            END IF
            
            ! 回答の正規化
            IF (rotation(index) == 1) THEN
                ans = abs(ans - 5)
            END IF
            
            ! 回答の入力
            answer(index) = ans
            point = point + ans
        end subroutine
        
        subroutine write_csv()
            integer, parameter :: FNO = 99
            integer values(8)
            character date*8, time*10, diff*5
            character realdate*25, data_format*61
            integer i
            ! YYYY/MM/DD-HH:MM:MM+XX:XX
            realdate = "    /  /  -  :  :     :  "
            
            open(FNO, file="ANSWER.CSV", status="UNKNOWN", action="write", position="append")  ! ファイルが作成されていないならば作成し，作成されていればOLDで開く
            CALL DATE_AND_TIME(date, time, diff, values)
            
            ! DATETIMEな文字列を生成
            realdate(1:4) = date(1:4)
            realdate(6:7) = date(5:6)
            realdate(9:10) = date(7:8)
            realdate(12:13) = time(1:2)
            realdate(15:16) = time(3:4)
            realdate(18:19) = time(5:6)
            realdate(20:22) = diff(1:3)
            realdate(24:25) = diff(4:5)
            
            ! 普通に出力すると６要素ごとに改行される
            data_format(1:3) = "(i3"
            DO i=1, N-1
                data_format(i*3+1:i*3+4) = ",i3"
            END DO
            data_format(61:61) = ")"
            
            write (FNO, *) realdate
            write (FNO, data_format) number
            write (FNO, data_format) answer
            write (FNO, *) point, point / (4.0 * N)
            close (FNO)
        end subroutine
        
        subroutine set_state_rotation()
            rotation = (/ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1 /)
        end subroutine
        
        subroutine set_trait_rotation()
            rotation = (/ 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0 /)
        end subroutine
        
        subroutine state_english()
            ask(1) = "I feel calm"
            ask(2) = "I feel secure"
            ask(3) = "I am tense"
            ask(4) = "I feel strained"
            ask(5) = "I feel at ease"
            ask(6) = "I feel upset"
            ask(7) = "I am presently worrying over possible misfortunes"
            ask(8) = "I feel satisfied"
            ask(9) = "I feel frightened"
            ask(10) = "I feel comfortable"
            ask(11) = "I feel self-confident"
            ask(12) = "I feel nevous"
            ask(13) = "I am jittery"
            ask(14) = "I feel indecisive"
            ask(15) = "I am relaxed"
            ask(16) = "I feel content"
            ask(17) = "I am worried"
            ask(18) = "I feel confused"
            ask(19) = "I feel steady"
            ask(20) = "I feel pleasant"
        end subroutine
        
        subroutine trait_english()
            ask(1) = "I feel pleasant"
            ask(2) = "I feel nervous and restless"
            ask(3) = "I feel satisfied with myself"
            ask(4) = "I wish I could be as happy as others seem to be"
            ask(5) = "I feel like a failure"
            ask(6) = "I feel rested"
            ask(7) = "I am 'calm, cool and coollected'"
            ask(8) = "I feel that difficulties are piling up so that I cannot overcome them"
            ask(9) = "I worry too much over something that really doesn't matter"
            ask(10) = "I am happy"
            ask(11) = "I have disturbing thoughts"
            ask(12) = "I lack self-confidence"
            ask(13) = "I feel secure"
            ask(14) = "I make decision easily"
            ask(15) = "I feel inadequate"
            ask(16) = "I am content"
            ask(17) = "Some unimportant thought runs through my mind and bothers me"
            ask(18) = "I take disappointments so keenly that I can't put them out of my mind"
            ask(19) = "I am a steady person"
            ask(20) = "I get in a state of tension or turmoil as I think over my recent concerns and interests"
        end subroutine
        
        subroutine state_japanese()
            ask(1) = "気持ちが落ち着いている"
            ask(2) = "安定している"
            ask(3) = "緊張している"
            ask(4) = "くよくよしている"
            ask(5) = "気楽な気分である"
            ask(6) = "動揺している"
            ask(7) = "なにか悪いことが起こりはしないかと心配だ"
            ask(8) = "ホッと心が休まる感じがする"
            ask(9) = "なにか怖い感じだ"
            ask(10) = "居心地のよい感じがある"
            ask(11) = "自信がある"
            ask(12) = "神経質になっている"
            ask(13) = "気持ちが落ち着かずじってしていられない"
            ask(14) = "ピリピリと気持ちが張り詰めている"
            ask(15) = "くつろいでいる"
            ask(16) = "満ち足りている感じだ"
            ask(17) = "心に悩みがある"
            ask(18) = "興奮しすぎて気持ちが落ち着かない"
            ask(19) = "安心感がある"
            ask(20) = "快適な気分である"
        end subroutine
        
        subroutine trait_japanese()
            ask(1) = "快適だ"
            ask(2) = "不安と緊張でいっぱいだ"
            ask(3) = "自分自身に満足している"
            ask(4) = "幸せそうな人を見ると自分もそうなりたいと思う"
            ask(5) = "なにごとも失敗するような気がする"
            ask(6) = "ホッと心休まる感じになる"
            ask(7) = "落ち着いていて冷静で慌てない"
            ask(8) = "難しいことが重なって，もうどうにもならないと感じる"
            ask(9) = "実際にはさして重要でないことを心配しすぎる"
            ask(10) = "幸せな気持ちになる"
            ask(11) = "物事を難しく考えすぎてしまう"
            ask(12) = "自信がない"
            ask(13) = "安心感がある"
            ask(14) = "すぐに決断できる"
            ask(15) = "ゆううつな気分になる"
            ask(16) = "満ち足りた気持ちになる"
            ask(17) = "そう重要でもない考えが頭に浮かんで，それに煩わされる"
            ask(18) = "失敗するとひどくこたえてなかなか頭から離れない"
            ask(19) = "物事を着実に運ぶ"
            ask(20) = "この頃気になっている出来事を考え始めると，気持ちが緊張したり，動揺する"
        end subroutine
    end module