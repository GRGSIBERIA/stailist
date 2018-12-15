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
        character, public :: ask(20)*90
        character, private :: command*10
        integer, private :: is_japanese ! ���{��-�p��̐؂�ւ�
        integer, private :: is_state    ! STATE-TRAIT�̐؂�ւ�
        integer, private :: point       ! ���_
        integer, private :: command_line_count
        
    contains
        subroutine initialize_question()
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
            
            IF (is_state == 0 .and. is_japanese == 0) THEN
                print *, "RUNNING STATE"
            ELSE IF (is_state == 1 .and. is_japanese == 0) THEN
                print *, "RUNNING TRAIT"
            ELSE IF (is_state == 0 .and. is_japanese == 1) THEN
                print *, "��Ԏړx"
            ELSE IF (is_state == 1 .and. is_japanese == 1) THEN
                print *, "�󋵎ړx"
            END IF
        end subroutine
    end module