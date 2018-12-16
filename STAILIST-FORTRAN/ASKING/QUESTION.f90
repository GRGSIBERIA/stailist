!  QUESTION.f90 
!
!  �֐�:
!  QUESTION - ���╶���������郂�W���[��
!

!****************************************************************************
!
!  �v���O����: QUESTION
!
!  �ړI:  ���╶����������
!
!****************************************************************************
    
    module Question
        implicit none
        character, private :: ask(20)*100   ! ���⍀�ځi�����_�����j
        integer, private :: answer(20)      ! �팱�҂̉񓚁i���K���ς݁j
        integer, private :: number(20)      ! ���⍀�ڔԍ��i�����_���ɂ���O�̔ԍ��j
        integer, private :: rotation(20)    ! ���邢�񓚃t���O�i�񓚂𐳋K������t���O�j
        integer, parameter :: N = 20        ! ���⍀�ڐ�
        integer, private :: is_japanese = 1 ! ���{��-�p��̐؂�ւ�
        integer, private :: is_state = 1    ! STATE-TRAIT�̐؂�ւ�
        integer, private :: point = 0       ! ���_
        
    contains
        subroutine initialize_question()
            character command*10
            integer command_line_count, count
            
            DO count=1, N
                number(count) = count
            END DO
            
            ! �R�}���h���C���������󂯎���Ă���ɍ��v�����ϐ��l��������
            DO command_line_count = 1, iargc()
                CALL GETARG(command_line_count, command)
            
                ! ���{��Ɖp��̔���
                IF (command == "-ja" .or. command == "-jp" .or. command == "-j") THEN
                    is_japanese = 1
                    print *, "���{�ꃂ�[�h"
                    GOTO 100
                ELSE IF (command == "-en" .or. command == "-e") THEN
                    is_japanese = 0
                    print *, "ENGLISH MODE"
                    GOTO 100
                END IF
            
                ! State-Trait�̔���
                IF (command == "-state" .or. command == "-s") THEN
                    is_state = 1
                    GOTO 100
                ELSE IF (command == "-trait" .or. command == "-t") THEN
                    is_state = 0
                    GOTO 100
                END IF
            
    100         CONTINUE ! �J�b�g
            END DO
            
            ! ���⍀�ڂ̔��]��ݒ�
            if (is_state == 0) THEN
                CALL set_trait_rotation()
            ELSE
                CALL set_state_rotation()
            END IF
            
            ! �������e��񎦂���
            IF (is_state == 0 .and. is_japanese == 0) THEN
                print *, "ASKING TRAIT SCALE"
                CALL trait_english()
            ELSE IF (is_state == 1 .and. is_japanese == 0) THEN
                print *, "ASKING STATE SCALE"
                CALL state_english()
            ELSE IF (is_state == 0 .and. is_japanese == 1) THEN
                print *, "�����s���ɂ��Đq�˂܂�"
                CALL trait_japanese()
            ELSE IF (is_state == 1 .and. is_japanese == 1) THEN
                print *, "��ԕs���ɂ��Đq�˂܂�"
                CALL state_japanese()
            END IF
            
            ! �p�ӂ���ask��rotation�̏��Ԃ������_��������
            CALL shuffle()
        end subroutine
        
        subroutine shuffle()
            character :: tmp*100
            integer :: i, j, rottemp, seed_size
            integer, allocatable :: seed(:)
            real r
            
            ! �����_���̃V�[�h�l��ύX����
            CALL RANDOM_SEED(size=seed_size)
            allocate(seed(seed_size))
            DO i = 1, seed_size
                call SYSTEM_CLOCK(count=seed(i))
            END DO
            CALL RANDOM_SEED(put=seed(:))
            
            ! �z��������_���ɃV���b�t������
            DO i = 1, N     ! ���Ƃ�N-1������
                CALL RANDOM_NUMBER(r)
                j = r * (i + 1) + 1
                if (N < j) THEN
                    j = N       ! �������Ȃ���N�Ԗڂ̗v�f���V���b�t������Ȃ��o�O��������
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
                print *, "���Ȃ��̓��_��", normalized, "�ł�"
            ELSE
                print *, "Your number of point is ", normalized
            END IF
        end subroutine
        
        subroutine tell_answer(index)
            integer, intent(in) :: index
            integer ans
200         print *, "---------------------------------"
            
            IF (is_japanese == 1) THEN
                print *, "1 (�܂������Ȃ�), 2 (�����炩����), 3 (���Ȃ肠��), 4(�͂����肠��)"
                print *, "���⍀�ځF", ask(index)
            ELSE
                print *, "1 (NOT AT ALL), 2 (SOMEWHAT), 3 (MODERATELY SO), 4 (VERY MUCH SO)"
                print *, "QUESTION: "
            END IF
            
            read *, ans
            
            ! �񓚂̃o���f�[�V����
            IF (ans > 4 .or. ans < 1) THEN
                IF (is_japanese == 1) THEN
                    print *, "1�`4 �܂ł̒l����͂��Ă�������"
                ELSE
                    print *, "Please fill in the value between 1 and 4."
                END IF
                GOTO 200
            END IF
            
            ! �񓚂̐��K��
            IF (rotation(index) == 1) THEN
                ans = abs(ans - 5)
            END IF
            
            ! �񓚂̓���
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
            
            open(FNO, file="ANSWER.CSV", status="UNKNOWN", action="write", position="append")  ! �t�@�C�����쐬����Ă��Ȃ��Ȃ�΍쐬���C�쐬����Ă����OLD�ŊJ��
            CALL DATE_AND_TIME(date, time, diff, values)
            
            ! DATETIME�ȕ�����𐶐�
            realdate(1:4) = date(1:4)
            realdate(6:7) = date(5:6)
            realdate(9:10) = date(7:8)
            realdate(12:13) = time(1:2)
            realdate(15:16) = time(3:4)
            realdate(18:19) = time(5:6)
            realdate(20:22) = diff(1:3)
            realdate(24:25) = diff(4:5)
            
            ! ���ʂɏo�͂���ƂU�v�f���Ƃɉ��s�����
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
            ask(1) = "�C���������������Ă���"
            ask(2) = "���肵�Ă���"
            ask(3) = "�ْ����Ă���"
            ask(4) = "���悭�悵�Ă���"
            ask(5) = "�C�y�ȋC���ł���"
            ask(6) = "���h���Ă���"
            ask(7) = "�Ȃɂ��������Ƃ��N����͂��Ȃ����ƐS�z��"
            ask(8) = "�z�b�ƐS���x�܂銴��������"
            ask(9) = "�Ȃɂ��|��������"
            ask(10) = "���S�n�̂悢����������"
            ask(11) = "���M������"
            ask(12) = "�_�o���ɂȂ��Ă���"
            ask(13) = "�C���������������������Ă��Ă����Ȃ�"
            ask(14) = "�s���s���ƋC����������l�߂Ă���"
            ask(15) = "���낢�ł���"
            ask(16) = "��������Ă��銴����"
            ask(17) = "�S�ɔY�݂�����"
            ask(18) = "�����������ċC���������������Ȃ�"
            ask(19) = "���S��������"
            ask(20) = "���K�ȋC���ł���"
        end subroutine
        
        subroutine trait_japanese()
            ask(1) = "���K��"
            ask(2) = "�s���Ƌْ��ł����ς���"
            ask(3) = "�������g�ɖ������Ă���"
            ask(4) = "�K�������Ȑl������Ǝ����������Ȃ肽���Ǝv��"
            ask(5) = "�Ȃɂ��Ƃ����s����悤�ȋC������"
            ask(6) = "�z�b�ƐS�x�܂銴���ɂȂ�"
            ask(7) = "���������Ă��ė�ÂōQ�ĂȂ�"
            ask(8) = "������Ƃ��d�Ȃ��āC�����ǂ��ɂ��Ȃ�Ȃ��Ɗ�����"
            ask(9) = "���ۂɂ͂����ďd�v�łȂ����Ƃ�S�z��������"
            ask(10) = "�K���ȋC�����ɂȂ�"
            ask(11) = "���������l�������Ă��܂�"
            ask(12) = "���M���Ȃ�"
            ask(13) = "���S��������"
            ask(14) = "�����Ɍ��f�ł���"
            ask(15) = "�䂤���ȋC���ɂȂ�"
            ask(16) = "�������肽�C�����ɂȂ�"
            ask(17) = "�����d�v�ł��Ȃ��l�������ɕ�����ŁC����ɔς킳���"
            ask(18) = "���s����ƂЂǂ��������ĂȂ��Ȃ������痣��Ȃ�"
            ask(19) = "�����𒅎��ɉ^��"
            ask(20) = "���̍��C�ɂȂ��Ă���o�������l���n�߂�ƁC�C�������ْ�������C���h����"
        end subroutine
    end module