PD_SCK BIT P2.0
DOUT BIT P2.1
;INT0 BIT P3.2
;MEMORY LOCATİON ALLOCATION STARTS HERE
MINUEND_1 EQU 50H
MINUEND_2 EQU 51H
MINUEND_3 EQU 52H
MINUEND_4 EQU 53H

DIFFERENCE_1 EQU 58H
DIFFERENCE_2 EQU 59H
DIFFERENCE_3 EQU 60H
DIFFERENCE_4 EQU 61H

DIVIDEND_1 EQU 62H
DIVIDEND_2 EQU 63H
DIVIDEND_3 EQU 64H
DIVIDEND_4 EQU 65H

QUOTIENT_1 EQU 70H
QUOTIENT_2 EQU 71H
QUOTIENT_3 EQU 72H
QUOTIENT_4  EQU 73H

DATA_AVERAGE_HIGH EQU 74H
DATA_AVERAGE_LOW EQU 75H

VAL1_H EQU 76H ; VALUES THAT ARE CALCULATED WHILE OBTAINING THE AVERAGE
VAL1_L EQU 77H;  VALUES THAT ARE CALCULATED WHILE OBTAINING THE AVERAGE
VAL2_H EQU 78H ; VALUES THAT ARE CALCULATED WHILE OBTAINING THE AVERAGE
VAL2_L EQU 79H;  VALUES THAT ARE CALCULATED WHILE OBTAINING THE AVERAGE

TIMER0_OVERFLOW EQU 7AH
PROCESSING_DATA_HIGH EQU 7BH
PROCESSING_DATA_LOW EQU 7CH

TARE_VALUE_HIGH EQU 7DH
TARE_VALUE_LOW EQU 7EH

TARE_ACTIVE BIT 07FH

;TO BE USED IN CALIBRATION UNIT
SMALLEST_SO_FAR_H EQU 30H
SMALLEST_SO_FAR_M EQU 31H
SMALLEST_SO_FAR_L EQU 32H

PROCESSED_DATA_HIGH EQU 33H
PROCESSED_DATA_LOW EQU 34H

PROCESSED_DATA_LOW_PREVIOUS EQU 3AH
PROCESSED_DATA_HIGH_PREVIOUS EQU 3BH

INTERRUPT_DISABLE EQU 35H

AVERAGING_NUMBERS EQU 36H

STOPWATCH_HIGH EQU 37H
STOPWATCH_LOW EQU 38H

SUBTRAHEND_2 EQU 40h
SUBTRAHEND_3 EQU 41h
SUBTRAHEND_4 EQU 42h
SUBTRAHEND_1 EQU 00H; BEWARE, THIS IS A CONSTANT VALUE
;MEMORY LOCATION ALLOCATION IS FINISHED

;CONSTANTS EQUAL TO 569; THIS VALUE IS FOUND BY TRIAL AND ERROR. 
;THIS VALUE IS OF CRUCIAL IMPORTANCE WHILE CALIBRATING THE SCALE UNIT
DIVISOR_1 EQU 00H
DIVISOR_2 EQU 00H
DIVISOR_3 EQU 02H
DIVISOR_4 EQU 39H
;DIRECTIVES END HERE

ORG 0000H
LJMP MAIN
ORG 0003H
LJMP INT0_ISR
ORG 000BH
LJMP TIMER0_INT
ORG 002BH
LJMP TIMER2_INT
;INTERRUPT MATRIX ENDS HERE

MAIN:
                ACALL INITIALIZE_REGISTERS
                ACALL INITIALIZE_TARE_FEATURE
                ACALL CONFIGURE_LCD;
                ACALL CONFIGURE_SERIAL_PORT
                ACALL CALIBRATION
                ACALL CONFIGURE_50MS_DELAY
                ACALL STOPWATCH_INITIATE_TIMER2
                SJMP $
;END OF MAIN END OF MAIN END OF MAIN
;THE PROGRAM RUNS MAINLY ON ISR'S.

INT0_ISR:
                MOV 01H, C
                MOV C, TARE_ACTIVE
                CPL C
                MOV TARE_ACTIVE, C
                MOV C, 01H; POP THE CARRY
                
                JB TARE_ACTIVE, ACTIVATE_TARE
                JNB TARE_ACTIVE, DEACTIVATE_TARE
ACTIVATE_TARE:
                MOV TARE_VALUE_HIGH, PROCESSED_DATA_HIGH
                MOV TARE_VALUE_LOW, PROCESSED_DATA_LOW
                SJMP DONE3
DEACTIVATE_TARE:
                MOV TARE_VALUE_LOW, #00H
                MOV TARE_VALUE_HIGH, #00H
DONE3: 
		CLR EX0
		MOV INTERRUPT_DISABLE, #05H
		MOV STOPWATCH_HIGH, #00H
		MOV STOPWATCH_LOW, #00H
RETI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TIMER0_INT:
                CLR TR0
                MOV TH0, #0DCH
                MOV TL0, #00H
                DEC TIMER0_OVERFLOW

		JNB EX0, DEBOUNCING_BUTTON
		JB EX0, CONTINUE5
DEBOUNCING_BUTTON: DEC INTERRUPT_DISABLE;
		MOV A, INTERRUPT_DISABLE
		CJNE A, #00H, CONTINUE5
		SETB EX0
                
CONTINUE5:      MOV A, TIMER0_OVERFLOW
                CJNE A, #00H, SEND_TO_SERIAL
                ACALL DISPLAY_DATA
                MOV TIMER0_OVERFLOW, #0AH
SEND_TO_SERIAL:
                ACALL PROCESS_DATA
                ACALL SEND_TO_BLUETOOTH
                SETB TR0
                RETI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TIMER2_INT:
	PUSH ACC
	ACALL CHECK_IF_THERE_IS_ANY_CHANGE
	POP ACC
	CLR TF2
	RETI
;ISR ROUTINES END HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS SUBROUTINE CONSTANTLY CHECKS WHETHER READINGS TAKEN
;FROM THE LOAD CELL ARE CONSISTENT
;I.E. IF THERE IS ANY SUBSTANTIAL CHANGE IN THE READINGS,
;THIS SUBROUTINE EITHER STARTS OR STOPS THE STOPWATCH UNIT
;
;
;DESTROYS: ACC, CY, R0 TO R7
;
;OUTCOME: EITHER THE INCREMENTATION OF THE COUNTER ASSSOCIATED
;WITH THE STOPWATCH OR ITS ZEROING.
CHECK_IF_THERE_IS_ANY_CHANGE:
	MOV A, PROCESSED_DATA_HIGH
	CJNE A, PROCESSED_DATA_HIGH_PREVIOUS, NOT_EQUAL_PROCESS
	SJMP CHECK_THE_OTHER_CASE
NOT_EQUAL_PROCESS:
	JC PREVIOUS_IS_LARGER
	JNC CURRENT_IS_LARGER
CHECK_THE_OTHER_CASE:
	MOV A, PROCESSED_DATA_LOW
	CJNE A, PROCESSED_DATA_LOW_PREVIOUS, NOT_EQUAL_PROCESS_2
	SJMP THEY_ARE_EQUAL; DO NOT INCREMENT THE COUNTER
NOT_EQUAL_PROCESS_2:
	JC PREVIOUS_IS_LARGER
	JNC CURRENT_IS_LARGER

CURRENT_IS_LARGER:
	MOV R6, PROCESSED_DATA_HIGH
	MOV R7, PROCESSED_DATA_LOW
	MOV R4, PROCESSED_DATA_HIGH_PREVIOUS
	MOV R5, PROCESSED_DATA_LOW_PREVIOUS
	ACALL SUBB16_16 ;R2/R3
	SJMP CHECK_IF_LARGER_THAN_THRESHOLD

PREVIOUS_IS_LARGER:
;	MOV R6, PROCESSED_DATA_HIGH_PREVIOUS
;	MOV R7, PROCESSED_DATA_LOW_PREVIOUS
;	MOV R4, PROCESSED_DATA_HIGH
;	MOV R5, PROCESSED_DATA_LOW
;	ACALL SUBB16_16 ;R2/R3
;	SJMP CHECK_IF_LARGER_THAN_THRESHOLD
	MOV STOPWATCH_HIGH, #00H
	MOV STOPWATCH_LOW, #00H
	SJMP DONE

;OUR THRESHOULD IS ZERO. BAD NAMING.
CHECK_IF_LARGER_THAN_THRESHOLD:
	MOV A, R2
	CJNE A, #00H, INCREMENT_THE_COUNTER
	MOV A, R3
	CJNE A, #01H, INCREMENT_THE_COUNTER
	JC THEY_ARE_EQUAL
INCREMENT_THE_COUNTER:
	ACALL INCREMENT_STOPWATCH
THEY_ARE_EQUAL:
DONE:	RET
;END OF CHECK_IF_THERE_IS_ANY_CHANGE

;FIRST SENDS HIGH BYTE THEN THE LOW BYTE IN HEX
;THIS ROUTINE IS USED TO SEND VALUES WHICH ARE
;READ VIA THE LOAD CELL TO BLUETOOTH. 
;DESTROYS ACC, TI, SBUF
SEND_TO_BLUETOOTH:
                MOV A, PROCESSED_DATA_HIGH
                MOV SBUF, A
                JNB TI, $
                CLR TI
                MOV A, PROCESSED_DATA_LOW
                MOV SBUF, A
                JNB TI, $
                CLR TI
                ;LINE END CHARACTER MUST BE SENT
                MOV A, #0DH
                MOV SBUF, A
                JNB TI, $
                CLR TI
RET
;END OF BLUETOOTH ROUTINE

;THIS ROUTINE IS USED TO PROCESS THE 24BITS DATA OBTAINED FROM
;THE INSTRUMENTATIONAL AMPLIFIER AND SERIAL ADC HX711. AT THE 
;END, 24BITS DATA IS CONVERTED INTO A NUMBER IN GRAMS. 
;TWO'S COMPLEMENT PROCESS IS NEEDED SINCE HX711 SENDS THE DATA IN
;2'S COMPLEMENT FORM
;DESTROYS ACC, CY, R0-R7
;OUTCOME: THE VALUE READ FROM THE SENSOR IN TERMS OF GRAMS IS PLACED
;IN THE REGISTERS PROCESSED_DATA_HIGH
;REMARK: THIS ROUTINE ALSO BACK UP THE LAST VALUE OF PROCESSED DATA.
;THIS BACK UP VALUE IS USED IN CHECK_IF_THERE_IS_ANY_CHANGE ROUTINE.
PROCESS_DATA:
		MOV PROCESSED_DATA_HIGH_PREVIOUS, PROCESSED_DATA_HIGH
		MOV PROCESSED_DATA_LOW_PREVIOUS, PROCESSED_DATA_LOW
	
        	ACALL READ_DATA_FROM_HX711
        	ACALL SUB_AND_DIVIDE_BY_569
        	
        	MOV 7,QUOTIENT_4; LOW
        	MOV 6,QUOTIENT_3; HIGH
        	MOV A, 6
        	JB ACC.7, TAKE_TWO_COMPLEMENT
        	JNB ACC.7, CONTINUE4
        	TAKE_TWO_COMPLEMENT:; 
        	CLR C
  		MOV A,6
        	CPL A
  		MOV 6,A
        	MOV A, 7
  		CPL A
        	ADD A, #01
        	MOV 7, A
        	MOV A, 6
        	ADDC A, #00H
        	MOV 6, A

CONTINUE4:  	MOV PROCESSING_DATA_HIGH, 6
        	MOV PROCESSING_DATA_LOW, 7
        	MOV A, PROCESSING_DATA_HIGH
        	CJNE A, TARE_VALUE_HIGH, CHECK
        	SJMP CHECK_THE_OTHER
CHECK: 		JC DEACTIVATE_TARE2
      		JNC CONTINUE_TO_DO_ARITHMETIC_CALCULATIONS
CHECK_THE_OTHER:MOV A, PROCESSING_DATA_LOW
                CJNE A, TARE_VALUE_LOW, XX
                SJMP CONTINUE_TO_DO_ARITHMETIC_CALCULATIONS
                XX:JC DEACTIVATE_TARE2
                JNC CONTINUE_TO_DO_ARITHMETIC_CALCULATIONS
                DEACTIVATE_TARE2: MOV TARE_VALUE_HIGH, #00H
                MOV TARE_VALUE_LOW, #00H
                CLR TARE_ACTIVE
                
                CONTINUE_TO_DO_ARITHMETIC_CALCULATIONS:
                MOV R6, PROCESSING_DATA_HIGH
                MOV R7, PROCESSING_DATA_LOW
                MOV R4, TARE_VALUE_HIGH
                MOV R5, TARE_VALUE_LOW
                LCALL SUBB16_16
                MOV PROCESSED_DATA_HIGH, R2
                MOV PROCESSED_DATA_LOW, R3
RET
;END OF THE ROUTINE

;CALIBRATION UNIT. DO NOT FORGET TO PLACE THE LOAD CELL ON A
;FIRM GROUND. FOR MORE DETAIL, SEE THE ROUTINE FIND_THE_SMALLEST_READING
CALIBRATION:
                MOV DPTR, #CALIBRATION_INITIATED
                ACALL DISPLAY_STRING
                MOV A, #01H
                ACALL SEND_COMMAND
                MOV DPTR, #WAIT
                ACALL DISPLAY_STRING
                
                ACALL FIND_THE_SMALLEST_READING; VERIFIED
                
                MOV SUBTRAHEND_2,SMALLEST_SO_FAR_H
                MOV SUBTRAHEND_3,SMALLEST_SO_FAR_M
                MOV SUBTRAHEND_4,SMALLEST_SO_FAR_L
                
                MOV A, SUBTRAHEND_2
                MOV SBUF, A
                JNB TI, $
                CLR TI
        	MOV A, SUBTRAHEND_3
                MOV SBUF, A
                JNB TI, $
                CLR TI
        	MOV A, SUBTRAHEND_4
                MOV SBUF, A
                JNB TI, $
                CLR TI
        	MOV A, #0CCH
        	MOV SBUF, A
                JNB TI, $
                CLR TI
                MOV A, #01H
                ACALL SEND_COMMAND
                MOV DPTR, #CAL
                ACALL DISPLAY_STRING
                MOV A, #80H
                ACALL SEND_COMMAND
                MOV A, #01H
                ACALL SEND_COMMAND
                RET

;THE MAIN SUBROUTINE USED WHILE CALIBRATING THE UNIT. 
;TAKES AS MUCH AS VALUES FROM THE SENSOR. THEN DECLARES THAT THE SMALLEST
;READING IS THE REFERENCE VALUE TO BE USED DURING CALCULATIONS.
;AVERAGING METHOD WOULD BE A BETTER IDEA INSTEAD OF THIS ROUTINE, 
;BUT AVERAGING METHOD TOOK MORE THAN FEW MINUTES TO COMPLETE, SO WE
;DISCARDED THE IDEA OF AVERAGING
FIND_THE_SMALLEST_READING:
                               ACALL READ_DATA_FROM_HX711
                               MOV A, R5
                               CJNE A, #01H, FIND_THE_SMALLEST_READING
                               MOV A, R6
                               CJNE A, #40H, CHECK_THEM
                               SJMP CONTINUE2
CHECK_THEM:  		       JC FIND_THE_SMALLEST_READING

CONTINUE2:      	       MOV AVERAGING_NUMBERS, #0AH
                               MOV SMALLEST_SO_FAR_H, R5
                               MOV SMALLEST_SO_FAR_M, R6
                               MOV SMALLEST_SO_FAR_L, R7
 
READ_AGAIN:   		       ACALL READ_DATA_FROM_HX711
                               MOV A, R5
                               CJNE A, #01H, READ_AGAIN
                               MOV A, R6
                               CJNE A, #40H, CHECK_THEM2
                               SJMP CONTINUE3
CHECK_THEM2: 		       JC READ_AGAIN
CONTINUE3:		       MOV A, R5
                               CJNE A, SMALLEST_SO_FAR_H, HIGHEST_NOT_EQUAL
                               SJMP SECOND_CASE; IF THEY ARE EQUAL
HIGHEST_NOT_EQUAL:
                               JC SWAP_VALUES
                               JNC DONE2
SECOND_CASE:
                               MOV A, R6
                               CJNE A, SMALLEST_SO_FAR_M, MIDDLE_NOT_EQUAL
                               SJMP THIRD_CASE
MIDDLE_NOT_EQUAL:
                               JC SWAP_VALUES
                               JNC DONE2
THIRD_CASE:
                               MOV A, R7
                               CJNE A, SMALLEST_SO_FAR_L, LOWEST_NOT_EUQAL
                               SJMP DONE2
LOWEST_NOT_EUQAL:
                               JC SWAP_VALUES
                               JNC DONE2
SWAP_VALUES:
                               MOV SMALLEST_SO_FAR_H, R5
                               MOV SMALLEST_SO_FAR_M, R6
                               MOV SMALLEST_SO_FAR_L, R7
DONE2:
                DJNZ AVERAGING_NUMBERS, READ_AGAIN
                RET

                
;deprecatedsubroutine
OBTAIN_VALID_DATA:
                MOV R6, #0AH; TO BE USED AS A COUNTER
                MOV R7, #001H; CLEAR REGISTER
                MOV DATA_AVERAGE_HIGH, #00H
                MOV DATA_AVERAGE_LOW, #00H
                ACALL DATA_AVERAGING
                ACALL DISPLAY_DATA
RET
DEPRECATED
 ;DISCARDS THE VALUE IF IT IS NEGATIVE.
 ;DATA_AVERAGING:
 ;LOOP:   ACALL READ_DATA_FROM_HX711
 ;        ACALL SUB_AND_DIVIDE_BY_569
 ;        RET
 ;               AS: MOV A, QUOTIENT_3
 ;                MOV C, ACC.7
 ;                JB CY, DONE_NEGATIVE; CHECK IF THE VALUE IS NEGATIVE (WE KNOW THAT IT IS IN SIGNED FORMAT)
 ;                ;;;;;;;;;;;;;;;;;;;
 ;                MOV R1, QUOTIENT_3
 ;                MOV R0, QUOTIENT_4
 ;                MOV R3, #00H
 ;                MOV R2, 7; THE COUNTER
 ;                PUSH 6
 ;                PUSH 7
 ;                ACALL DIV16_16
 ;                MOV VAL2_L, R2
 ;                MOV VAL2_H, R3
 ;                ;VAL2 IS FOUND
 ;                MOV R1, DATA_AVERAGE_HIGH
 ;                MOV R0, DATA_AVERAGE_LOW
                MOV R3, #00H
                POP 7
                POP 6
                MOV R2, 7
                PUSH 7
                PUSH 6
                ACALL DIV16_16
                POP 6
                POP 7
                MOV VAL1_L, R2
                MOV VAL1_H, R3
                ;VAL1 IS FOUND
                PUSH 7
                PUSH 6
                MOV R6, DATA_AVERAGE_HIGH
                MOV R7, DATA_AVERAGE_LOW
                MOV R4, VAL2_H
                MOV R5, VAL2_L
                SJMP X
               LOOP2:SJMP LOOP
                X:ACALL ADD16_16
                POP 6
                POP 7
                ;R2 R3 FROM MOST TO LEAST HOLD THE RESULT
                PUSH 7
                PUSH 6
                MOV R6, 2
                MOV R7, 3
                MOV R4, VAL1_H
                MOV R5, VAL1_L
           
                ACALL SUBB16_16
                POP 6
                POP 7
                MOV DATA_AVERAGE_HIGH, R2
                MOV DATA_AVERAGE_LOW, R3
                INC R7
              
                SJMP DONE
DONE_NEGATIVE:  INC R6
DONE:  DJNZ R6, LOOP2
                RET

DISPLAY_DATA:
                MOV A, #80H
                ACALL SEND_COMMAND
                MOV 7,PROCESSED_DATA_LOW; LOW
                MOV 6,PROCESSED_DATA_HIGH; HIGH
                MOV A,6
                CJNE A,#0FFH,CC
                LJMP DISPLAY_NEGATIVE
                CC:ACALL BIN2BCD
                ;R3 R4 R5 MOST TO LEAST, RESPECTIVELY, IN PACKED BCD FORM
                ;ACALL PRINT24
                MOV A, R4
                SWAP A
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA
                MOV A, #'.'
                ACALL SEND_DATA
                MOV A, R4
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA
                MOV A, R5
                SWAP A
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA
                MOV A, R5
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA
                MOV A, #' '
                ACALL SEND_DATA
                MOV A, #'K'
                ACALL SEND_DATA
                MOV A, #'g'
                ACALL SEND_DATA
                MOV A, #' '
                ACALL SEND_DATA
                JB TARE_ACTIVE, PRINT_T
                JNB TARE_ACTIVE, DONT_PRINT_T
                PRINT_T:
                MOV DPTR, #TARE
                ACALL DISPLAY_STRING_IN_THE_SAME_LINE
                SJMP DONE4
                DONT_PRINT_T:
                MOV DPTR, #SPACE
                ACALL DISPLAY_STRING_IN_THE_SAME_LINE

		MOV A, #0C0H
		ACALL SEND_COMMAND
		MOV A, STOPWATCH_LOW
		RL A
		MOV R7, A
		MOV R6, STOPWATCH_HIGH
		ACALL BIN2BCD

		MOV A, R4
                SWAP A
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA
                MOV A, R4
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA
                MOV A, R5
                SWAP A
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA

		MOV A, #'.'
		ACALL SEND_DATA
                
                MOV A, R5
                ANL A, #0FH
                ORL A, #30H
                ACALL SEND_DATA

                MOV A, #' '
                ACALL SEND_DATA
                MOV A, #'s'
                ACALL SEND_DATA
                
                SJMP DONE4
                DISPLAY_NEGATIVE: MOV DPTR,#NEGATIVE
                ACALL DISPLAY_STRING

                
DONE4:           RET

;USED FOR DEBUGGING PURPOSES
;PRINT24:
;              clr c ;to print bits(24) i use this
;              MOV A,MINUEND_2
;              MOV R1,#8
;              AGAIN:RLC A
;              MOV B,A
;              JC A_1
;              MOV A,#0
;              SJMP HERE
;              A_1: MOV A,#1
;              HERE:ORL A,#30H
;              ACALL SEND_DATA
;              MOV A,B
;              DJNZ R1,AGAIN
;              ;ACALL DEL
;              CLR C
;              MOV A,MINUEND_3
;              MOV R1,#8
;              AGAIN2:RLC A
;              MOV B,A
;              JC A_11
;              MOV A,#0
;              SJMP HERE2
;              A_11: MOV A,#1
;              HERE2:ORL A,#30H
;              ACALL SEND_DATA
;              MOV A,B
;              DJNZ R1,AGAIN2
;              ;ACALL DEL
;              MOV A, #0C0H
;              ACALL SEND_COMMAND
;              CLR C
;              MOV A,MINUEND_4
;              MOV R1,#8
;              AGAIN3:RLC A
;              MOV B,A
;              JC A_12
;              MOV A,#0
;              SJMP HERE3
;              A_12: MOV A,#1
;              HERE3:ORL A,#30H
;              ACALL SEND_DATA
;              MOV A,B
;              DJNZ R1,AGAIN3
;              RET


;OUTPUT: R5 R6 R7 (MOST SIGNIFICANT TO LEAST)
READ_DATA_FROM_HX711:
                CLR PD_SCK
                CLR A
                SETB DOUT
                JB DOUT, $
                MOV R4, #8
SHIFT1:
                SETB PD_SCK
                NOP
                CLR PD_SCK
                MOV C, DOUT
                RLC A
                DJNZ R4, SHIFT1
                MOV R5, A
MOV MINUEND_2,5
                CLR A
                CLR C
                MOV R4, #8
SHIFT2:
                SETB PD_SCK
                NOP
                CLR PD_SCK
                MOV C, DOUT
                RLC A
                DJNZ R4, SHIFT2
                MOV R6, A
                MOV MINUEND_3,6
                CLR A
                CLR C
SHIFT3:
                SETB PD_SCK
                NOP
                CLR PD_SCK
                MOV C, DOUT
                RLC A
                DJNZ R4, SHIFT3
                MOV R7, A
                MOV MINUEND_4,7
                CLR A
                CLR C
               ;TELLING HX711 THAT I FINISHED RETRIEVING DATA
                SETB PD_SCK
                NOP
                CLR PD_SCK
RET
;

;32 BITS ARITHMETICS
;ALGORITHM: (VALUE_READ - REFERENCE_VALUE) / 569 = THE RESULT IN GRAMS
SUB_AND_DIVIDE_BY_569:   
                PUSH 3
                PUSH 2
                PUSH 1
                PUSH 0
                PUSH 7
                PUSH 6
                PUSH 5
                PUSH 4
                MOV MINUEND_1,#00
                MOV 3,MINUEND_1
                MOV 2,MINUEND_2
                MOV 1, MINUEND_3
                MOV 0, MINUEND_4
                MOV R7, #SUBTRAHEND_1
                MOV R6, SUBTRAHEND_2
                MOV R5,SUBTRAHEND_3
                MOV R4, SUBTRAHEND_4
                ACALL SUB32
                MOV DIFFERENCE_1, 3
                MOV DIFFERENCE_2, 2
                MOV DIFFERENCE_3, 1
                MOV DIFFERENCE_4, 0
     
                MOV DIVIDEND_1,DIFFERENCE_1
                MOV DIVIDEND_2,DIFFERENCE_2
                MOV DIVIDEND_3,DIFFERENCE_3
                MOV DIVIDEND_4,DIFFERENCE_4
                MOV R3,#00
                MOV R2,DIVIDEND_2
                MOV R1,DIVIDEND_3
                MOV R0,DIVIDEND_4
                MOV R5,#DIVISOR_3
                MOV R4,#DIVISOR_4
                ACALL DIV32
                MOV QUOTIENT_1,R3
                MOV QUOTIENT_2,R2
                MOV QUOTIENT_3,R1
                MOV QUOTIENT_4,R0
             
                POP 4
                POP 5
                POP 6
                POP 7
                POP 0
                POP 1
                POP 2
                POP 3
                RET

                
; subroutine DIV32
; 32-Bit / 16-Bit
;
; INPUT    r3, r2, r1, r0 = Dividend X
;           r5, r4 = Divisor Y
;
; OUTPUT   r3, r2, r1, r0 = quotient Q of division Q = X / Y
DIV32:
	       ANL     PSW, #0E7H      ; REGISTER BANK 0
               MOV     A, R4           ; GET DIVISOR HIGH BYTE
               ORL     A, R5           ; OR WITH LOW BYTE
               JNZ     DIV32_OK        ; DIVISOR OK IF NOT 0
               SETB    C               ; ELSE, OVERFLOW
               RET
DIV32_OK:      ACALL   CR0R3           ; 2'S COMP -> MAG/SIGN
               ACALL   CR4R5           ; 2'S COMP -> MAG/SIGN
               ACALL   UDIV32
               ACALL   MR0R3           ; MAG/SIGN -> 2'S COMP
               CLR     C               ; DIVISOR IS NOT 0
               RET                     ; DONE
UDIV32:        PUSH    08              ; SAVE REGISTER BANK 1
               PUSH    09
               PUSH    0AH
               PUSH    0BH
               PUSH    0CH
               PUSH    0DH
               PUSH    0EH
               PUSH    0FH
               PUSH    DPL
               PUSH    DPH
               PUSH    B
               SETB    RS0             ; SELECT REGISTER BANK 1
               MOV     R7, #0          ; CLEAR PARTIAL REMAINDER
               MOV     R6, #0
               MOV     R5, #0      
               MOV     R4, #0
               MOV     B, #32          ; SET LOOP COUNT
DIV_LP32:      CLR     RS0             ; SELECT REGISTER BANK 0
               CLR     C               ; CLEAR CARRY FLAG
               MOV     A, R0           ; SHIFT THE HIGHEST BIT OF THE
               RLC     A               ; DIVIDEND INTO...
               MOV     R0, A
               MOV     A, R1
               RLC     A
               MOV     R1, A
               MOV     A, R2
               RLC     A
               MOV     R2, A
               MOV     A, R3
               RLC     A
               MOV     R3, A
               SETB    RS0             ; SELECT REGISTER BANK 1
               MOV     A, R4           ; ... THE LOWEST BIT OF THE
               RLC     A               ; PARTIAL REMAINDER
               MOV     R4, A
               MOV     A, R5
               RLC     A
               MOV     R5, A
               MOV     A, R6
               RLC     A
               MOV     R6, A
               MOV     A, R7
               RLC     A
              MOV     R7, A
               MOV     A, R4           ; TRIAL SUBTRACT DIVISOR FROM
               CLR     C               ; PARTIAL REMAINDER
               SUBB    A, 04
               MOV     DPL, A
               MOV     A, R5
               SUBB    A, 05
               MOV     DPH, A
               MOV     A, R6
               SUBB    A, #0
               MOV     06, A
               MOV     A, R7
               SUBB    A, #0
               MOV     07, A
               CPL     C               ; COMPLEMENT EXTERNAL BORROW
               JNC     DIV_321         ; UPDATE PARTIAL REMAINDER IF
                                       ; BORROW
               MOV     R7, 07          ; UPDATE PARTIAL REMAINDER
               MOV     R6, 06
               MOV     R5, DPH
               MOV     R4, DPL
DIV_321:       MOV     A, R0           ; SHIFT RESULT BIT INTO PARTIAL
               RLC     A               ; QUOTIENT
               MOV     R0, A
               MOV     A, R1
               RLC     A
               MOV     R1, A
               MOV     A, R2
               RLC     A
               MOV     R2, A
               MOV     A, R3
               RLC     A
               MOV     R3, A
               DJNZ    B, DIV_LP32
               MOV     07, R7          ; PUT REMAINDER, SAVED BEFORE THE
               MOV     06, R6          ; LAST SUBTRACTION, IN BANK 0
               MOV     05, R5
               MOV     04, R4
               MOV     03, R3          ; PUT QUOTIENT IN BANK 0
               MOV     02, R2
               MOV     01, R1
               MOV     00, R0
               CLR     RS0
               POP     B
               POP     DPH
               POP     DPL
               POP     0FH             ; RETRIEVE REGISTER BANK 1
               POP     0EH
               POP     0DH
               POP     0CH
               POP     0BH
               POP     0AH
               POP     09
               POP     08
               RET
CR0R3:         MOV     A, R3           ; READ HIGH INTO ACCUMULATOR
               JB      ACC.7, C2A      ; NEGATIVE IF BIT 7 IS 1
               CLR     21H             ; CLEAR SIGN FLAG IF 'POSITIVE'
               RET                     ; DONE
C2A:           SETB    21H             ; SET SIGN FLAG
               MOV     A, R0           ; NUMBER IS NEGATIVE
               CPL     A               ; COMPLEMENT
               ADD     A, #1           ; AND ADD +1
               MOV     R0, A
               MOV     A, R1           ; GET NEXT BYTE
               CPL     A               ; COMPLEMENT
               ADDC    A, #0
               MOV     R1,A
               MOV     A, R2           ; GET NEXT BYTE
               CPL     A               ; COMPLEMENT
               ADDC    A, #0
               MOV     R2,A
               MOV     A, R3           ; GET NEXT BYTE
               CPL     A               ; COMPLEMENT
               ADDC    A, #0
               MOV     R3, A
               RET                     ; DONE
CR4R5:         MOV     A, R5           ; READ HIGH INTO ACCUMULATOR
               JB      ACC.7, C3A      ; NEGATIVE IF BIT 7 IS 1
               CLR     22H             ; CLEAR SIGN BIT IF 'POSITIVE'
               RET                     ; DONE
C3A:           SETB    22H             ; SET SIGN FLAG
               MOV     A, R4           ; NUMBER IS NEGATIVE
               CPL     A               ; COMPLEMENT
               ADD     A, #1           ; AND ADD +1
               MOV     R4, A
               MOV     A, R5           ; GET NEXT BYTE
               CPL     A               ; COMPLEMENT
               ADDC    A, #0
               MOV     R5, A
               RET
MR0R3:         JB      21H, MR0R3B     ; TEST X SIGN
               JB      22H, MR0R3A     ; TEST Y SIGN
               RET
MR0R3B:        JNB     22H, MR0R3A
               RET
MR0R3A:        MOV     A, R0           ; NEGATE NUMBER
               CPL     A               ; COMPLEMENT
               ADD     A, #1           ; AND ADD +1
               MOV     R0, A
               MOV     A, R1           ; GET NEXT BYTE
               CPL     A               ; COMPLEMENT
               ADDC    A, #0
               MOV     R1, A
               MOV     A, R2           ; GET NEXT BYTE
               CPL     A               ; COMPLEMENT
               ADDC    A, #0
               MOV     R2, A
               MOV     A, R3           ; GET NEXT BYTE
               CPL     A               ; COMPLEMENT
               ADDC    A, #0
               MOV     R3, A
               RET

;32 BITS - 32 BITS
SUB32:         ANL    PSW, #0E7H       ; REGISTER BANK 0
               MOV     A, R0           ; LOAD X LOW BYTE INTO ACC
               CLR     C               ; CLEAR CARRY FLAG
               SUBB    A, R4           ; SUBRACT Y LOW BYTE
               MOV     R0, A           ; PUT RESULT IN Z LOW BYTE
               MOV     A, R1           ; REPEAT WITH OTHER BYTES...
               SUBB    A, R5
               MOV     R1, A
               MOV     A, R2
               SUBB    A, R6
               MOV     R2, A
               MOV     A, R3
               SUBB    A, R7
               MOV     R3, A
               MOV     C, OV           ; SET C IF EXTERNAL BORROW
               RET

               
;CONVERTS BINARY INTO HEX
;INPUT R6 R7
;OUTPUT PACKED BCD IN R3 R4   R5           
BIN2BCD:      
                CLR A    ; BCD = 0
                MOV R5,A
                MOV R4,A
                MOV R3,A
                MOV R2,#16 ; TO PROCESS 16 BITS
                BIN_10:          
                MOV A,R7 ; BIN16 = BIN16 * 2
                ADD A,R7
                MOV R7,A
                MOV A,R6
                ADDC A,R6 ; CARRY = MSB of BIN16
                MOV R6,A
                MOV A,R5 ; BCD = BCD * 2 + CARRY
                ADDC A,R5
                DA A
                MOV R5,A
                MOV A,R4
                ADDC A,R4
                DA A
                MOV R4,A
                MOV A,R3
                ADDC A,R3
                DA A
                MOV R3,A
                DJNZ R2,BIN_10
RET

CONFIGURE_LCD:
                ;THIS SUBROUTINE SENDS THE INITIALIZATION COMMANDS TO THE LCD
                mov a,#38H
                ;TWO LINES, 5X7 MATRIX
                acall SEND_COMMAND
                mov a,#0FH
                ;DISPLAY ON, CURSOR BLINKING
                acall SEND_COMMAND
                mov a,#06H
                ;INCREMENT CURSOR (SHIFT CURSOR TO RIGHT)
                acall SEND_COMMAND
                mov a,#01H
                ;CLEAR DISPLAY SCREEN
                acall SEND_COMMAND
                mov a,#80H
                ;FORCE CURSOR TO BEGINNING OF THE FIRST LINE
                acall SEND_COMMAND
                MOV A,#0CH
                ACALL SEND_COMMAND
                ret
                
;P1.0-P1.7 ARE CONNECTED TO LCD DATA PINS D0-D7
;P3.5 IS CONNECTED TO RS
;P3.6 IS CONNECTED TO R/W
;P3.7 IS CONNECTED TO E
SEND_COMMAND:
                ;THIS  SUBROUTINE IS FOR SENDING THE COMMANDS TO LCD
                mov p1,a  ;THE COMMAND IS STORED IN A, SEND IT TO LCD
                clr p3.5  ;RS=0 BEFORE SENDING COMMAND
               clr p3.6  ;R/W=0 TO WRITE
                setb p3.7
                ;SEND A HIGH TO LOW SIGNAL TO ENABLE PIN
                acall DELAY
                clr p3.7
                ret
SEND_DATA:
                ;THIS  SUBROUTINE IS FOR SENDING THE DATA TO BE DISPLAYED
                mov p1,a  ;SEND THE DATA STORED IN A TO LCD
                setb p3.5
                ;RS=1 BEFORE SENDING DATA
                clr p3.6  ;R/W=0 TO WRITE
                setb p3.7
                ;SEND A HIGH TO LOW SIGNAL TO ENABLE PIN
                acall DELAY
                clr p3.7
                ret
DELAY:  ;A SHORT DELAY SUBROUTINE
                push 0
                push 1
                mov r0,#50
                DELAY_OUTER_LOOP:
                mov r1,#255
                djnz r1,$
                djnz r0,DELAY_OUTER_LOOP
                pop 1
                pop 0
                ret
                
;PUT THE ADDRESS OF THE STRING AND CALL THIS SUBROUTINE
DISPLAY_STRING:
                               MOV A, #80H
                               ACALL SEND_COMMAND
LOOP5:           CLR A
                MOVC A, @A+DPTR
                CJNE A, #00H, CONTINUE
                SJMP DONE22
CONTINUE:ACALL SEND_DATA
                INC DPTR
                SJMP LOOP5
DONE22: RET

DISPLAY_STRING_IN_THE_SAME_LINE:
LOOP6:           CLR A
                MOVC A, @A+DPTR
                CJNE A, #00H, CONTINUE34
                SJMP DONE223
CONTINUE34:ACALL SEND_DATA
                INC DPTR
                SJMP LOOP6
DONE223: RET

DIV16_16:
                  CLR C       ;Clear carry initially
                  MOV R4,#00h ;Clear R4 working variable initially
                  MOV R5,#00h ;CLear R5 working variable initially
                  MOV B,#00h  ;Clear B since B will count the number of left-shifted bits
                div1:
                  INC B      ;Increment counter for each left shift
                  MOV A,R2   ;Move the current divisor low byte into the accumulator
                  RLC A      ;Shift low-byte left, rotate through carry to apply highest bit to high-byte
                  MOV R2,A   ;Save the updated divisor low-byte
                  MOV A,R3   ;Move the current divisor high byte into the accumulator
                  RLC A      ;Shift high-byte left high, rotating in carry from low-byte
                  MOV R3,A   ;Save the updated divisor high-byte
                  JNC div1   ;Repeat until carry flag is set from high-byte
                div2:        ;Shift right the divisor
                  MOV A,R3   ;Move high-byte of divisor into accumulator
                  RRC A      ;Rotate high-byte of divisor right and into carry
                  MOV R3,A   ;Save updated value of high-byte of divisor
                  MOV A,R2   ;Move low-byte of divisor into accumulator
                  RRC A      ;Rotate low-byte of divisor right, with carry from high-byte
                  MOV R2,A   ;Save updated value of low-byte of divisor
                  CLR C      ;Clear carry, we don't need it anymore
                  MOV 07h,R1 ;Make a safe copy of the dividend high-byte
                  MOV 06h,R0 ;Make a safe copy of the dividend low-byte
                  MOV A,R0   ;Move low-byte of dividend into accumulator
                  SUBB A,R2  ;Dividend - shifted divisor = result bit (no factor, only 0 or 1)
                  MOV R0,A   ;Save updated dividend
                  MOV A,R1   ;Move high-byte of dividend into accumulator
                  SUBB A,R3  ;Subtract high-byte of divisor (all together 16-bit substraction)
                  MOV R1,A   ;Save updated high-byte back in high-byte of divisor
                  JNC div3   ;If carry flag is NOT set, result is 1
                  MOV R1,07h ;Otherwise result is 0, save copy of divisor to undo subtraction
                  MOV R0,06h
div3:
                  CPL C      ;Invert carry, so it can be directly copied into result
                  MOV A,R4
                  RLC A      ;Shift carry flag into temporary result
                  MOV R4,A
                  MOV A,R5
                  RLC A
                  MOV R5,A                    
                  DJNZ B,div2 ;Now count backwards and repeat until "B" is zero
                  MOV R3,05h  ;Move result to R3/R2
                  MOV R2,04h  ;Move result to R3/R2
                  RET
SUBB16_16:
                  ;Step 1 of the process
                  MOV A,R7  ;Move the low-byte into the accumulator
                  CLR C     ;Always clear carry before first subtraction
                  SUBB A,R5 ;Subtract the second low-byte from the accumulator
                  MOV R3,A  ;Move the answer to the low-byte of the result
                  ;Step 2 of the process
                  MOV A,R6  ;Move the high-byte into the accumulator
                  SUBB A,R4 ;Subtract the second high-byte from the accumulator
                  MOV R2,A  ;Move the answer to the low-byte of the result
                  ;Return - answer now resides in R2, and R3.
                  RET
ADD16_16:
                  ;Step 1 of the process
                  MOV A,R7     ;Move the low-byte into the accumulator
                  ADD A,R5     ;Add the second low-byte to the accumulator
                  MOV R3,A     ;Move the answer to the low-byte of the result
                  ;Step 2 of the process
                  MOV A,R6     ;Move the high-byte into the accumulator
                  ADDC A,R4    ;Add the second high-byte to the accumulator, plus carry.
                  MOV R2,A     ;Move the answer to the high-byte of the result
                  ;Step 3 of the process
                  MOV A,#00h   ;By default, the highest byte will be zero.
                  ADDC A,#00h  ;Add zero, plus carry from step 2.
                  MOV R1,A ;Move the answer to the highest byte of  the result
                  ;Return - answer now resides in R1, R2, and R3.
                  RET

;CONFIGURATION SUBROUTINES ARE PUT HERE                  
                  
CONFIGURE_SERIAL_PORT:
                MOV TCON, #01H
                MOV IE, #83H
                MOV TMOD, #21H
                MOV TH1, #0FDH
                MOV TL1, #0FDH
                CLR SM0
                SETB SM1
                SETB REN
                SETB TR1
                RET
                
CONFIGURE_50MS_DELAY:
                MOV TIMER0_OVERFLOW, #0AH
                MOV TH0, #04CH
                MOV TL0, #01H
                SETB TR0
                RET
                
INITIALIZE_REGISTERS:
                MOV R0, #30H
                MOV A, #00H
                MOV R1, #4FH
LOOP7: MOV @R0, A
                INC R0
                DJNZ R1, LOOP7
                MOV R0, #00H
                MOV R1, #00H
RET

INITIALIZE_TARE_FEATURE:
               CLR TARE_ACTIVE
               MOV TARE_VALUE_HIGH, #00H
               MOV TARE_VALUE_LOW, #00H	
RET

INCREMENT_STOPWATCH:
	CLR C
	MOV A, STOPWATCH_LOW
	ADD A, #01H
	MOV STOPWATCH_LOW, A

	MOV A, STOPWATCH_HIGH
	ADDC A, #00H
	MOV STOPWATCH_HIGH,A 
RET

STOPWATCH_INITIATE_TIMER2:
	SETB EA
	ANL T2MOD,#0FCh; 				 /* T2OE=0;DCEN=1; */
	ORL T2MOD,#01h;
	CLR EXF2;                   /* reset flag */
   	CLR TCLK;
   	CLR RCLK;                   /* disable baud rate generator */
   	CLR EXEN2;                  /* ignore events on T2EX */
    	CLR 0C9H;                   /* timer mode */
   	CLR 0C8H;                 /* reload mode */
	SETB ET2
	MOV TH2, #4CH
	MOV TL2, #01H
	MOV T2CON, #00H
	MOV RCAP2H, #4CH
	MOV RCAP2L, #01H
	SETB TR2
RET

;PUT LOOK UP TABLES HERE
TARE: DB 'TARE', 0
SPACE: DB '    ', 0
CAL: DB 'CALIBRATED',0
CALIBRATION_INITIATED: DB 'NIKE PRESENTS', 0
WAIT: DB 'WAIT . . .',0
NEGATIVE: DB 'NEGATIVE WEIGHT',0
END
 

