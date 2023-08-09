;A simple delay
    LIST P=16F690
	#include <P16F690.INC>	; File contains addresses for register and bit names
    cblock 0x30
;Delay variables
CNT1				    ;Inner loop counter use RAM
CNT2				    ;Mid loop counter use RAM
CNT3				    ;Outer loop counter use RAM
flag				    ;flag memory 
temp				    ;memory for storing temp
setTemp				    ;memory for storing set temp variable
timerr				    ;memory for timer
store				    ;variable used in lookup table
;Variables for storing  7 segment code for each 7 segment display
disp1
disp2       
check
;Interrupt Service routine variables
W_Save
STATUS_Save 
adcL				    ;variable used with ADC     
    endc

    org	0x00
    goto Initialization		    ;skip to initialization part

    org 	0x04
ISR:				    ;Interrupt Service Routine
				    ;Save contents of the Work and Status register
     movwf     W_Save
     movf      STATUS,w
     movwf     STATUS_Save
     
     ;check if interrupt is caused by timer0, else exit ISR
     banksel   INTCON
     btfss     INTCON,T0IF
     goto      ExitISR

     bcf       INTCON,T0IF	    ;clear Timer0 interrupt flag
    
     BTFSC     check,0
     GOTO      DisplayUnits
     

DisplayTens			    ;display on disp2
     bsf     PORTB,7		    ;turn off disp1
     movfw   disp2		    ;turn on disp2 segment and exit ISR
     movwf   PORTC
     bsf     check,0
     bcf     PORTB,6
     goto    ExitISR

DisplayUnits			    ;display on disp1
     bsf     PORTB,6		    ;turn off disp2
     movfw   disp1		    ;turn on disp2 segment and exit ISR
     movwf   PORTC
     bcf     check,0
     bcf     PORTB,7
     
     
ExitISR:  ;Load Work and Status register contents back and Exit the ISR

     movf      STATUS_Save,w
     movwf     STATUS
     swapf     W_Save,f
     swapf     W_Save,w
     retfie

Initialization
    ;Declaring all pins as digital pins	
    BANKSEL	ANSEL
    ;make all pins digital
    clrf	ANSEL
    clrf    	ANSELH
    ;Make RA0 analog
    bsf         ANSEL,0

    ;Declaring Inputs and Outputs
    BANKSEL     TRISA
    clrf        TRISA
    movlw       b'00101001'	    ;make RA0 and RA5 inputs, RA1 and RA2 ouputs
    movwf       TRISA
    clrf        TRISC		    ;configure PORTC pins as outputs
    clrf        TRISB		    ;configure PORTB pins as outputs 

    ;start by clearing the 2 7segment displays
    clrf        disp1
    clrf        disp2
    clrf        check

    banksel     PORTA
    bcf         PORTA,1		    ;turn off Red LEd
    bsf         PORTA,2		    ; turn on Green Led

    ;set up timer 1, will be used for generating interrupts to multiplex 7 segment displays
    banksel OPTION_REG
    bcf     OPTION_REG,T0CS	    ;select internal clock
    bcf     OPTION_REG,PSA	    ;assign Prescaler to Timer0
    bcf     OPTION_REG,2	    ;set prescaler to 64
    bsf     OPTION_REG,1
    bsf     OPTION_REG,0

    ;Set up interrupt for timer 0
    banksel INTCON
    bsf     INTCON,T0IE		    ;Enable timer0 interrupt
    bsf     INTCON,GIE		    ;enable global interrupts

    ;load 3 to flag variable
    movlw      .3
    movwf      flag


;*******************************************************************************
;rotating through menus
Cook_Options
    movf       flag,W      
    call       LOOKSUB2		    ;call lookup table
    movwf      disp2		    ;save returned 7segment code to disp2 for displaying
    ;delay
    clrf	disp1
    bsf         PORTA,4
    call        DELAY
    bcf         PORTA,4
    call        DELAY
    call        DELAY
    ;check if button is pressed
    btfss       PORTA,5
    goto        Change_Menu	    ;goto to Change_Menu is button is not pressed
    goto        Check_Menu1_Selected		    ;goto Check_Menu1_Selected if button is pressed


Change_Menu 
    decfsz  flag		    ;decrement flag
    goto    Cook_Options
    movlw   .3			    ;load 3 to flag if zero
    movwf   flag
    goto Cook_Options		    ;repeat
;**********************************************************************************         
Check_Menu1_Selected
    ;check if menu 1 was selected
    movf flag,W
    sublw .1
    btfss  STATUS,Z
    goto   Check_Menu2_Selected	    ;goto Check_Menu2_Selected if menu is not 1
    movlw  .27			    ;set temp to 27
    movwf  setTemp
    movlw  .50			    ;set timer to 50
    movwf  timerr
    goto   z3


Check_Menu2_Selected
    ;check if menu 2 was selected
    movf flag,W
    sublw .3
    btfsc  STATUS,Z
    goto   Check_Menu3_Selected    ;goto to Check_Menu3_Selected if menu is not 3
    movlw  .30			    ;set temp to 30
    movwf  setTemp
    movlw  .90			    ;set timer to 75
    movwf  timerr
    goto   z3


Check_Menu3_Selected
    ;check if menu 3 was selected
    movf flag,W
    sublw .2
    btfsc  STATUS,Z
    goto   Cook_Options		    ;go back to the menu if menu is no 1,2 or 3
    movlw  .28			    ;set temp to 28
    movwf  setTemp
    movlw  .75			    ;set timer to 30
    movwf  timerr


z3
    ;banksel   PORTA
    bcf       PORTA,2		    ;green led off
    bsf       PORTA,1		    ;red led on

    ;Read ADC, AN0...read sensor value
    movlw b'10000001'
    call   A2D			    ;call function to ged ADC value
    movwf  adcL

    clrf CNT1
    clrf temp ;0

a3
    ;Calculate actual temperature from ADC value
    ;calculate temp by subtracting 2 from the ADC value till is 0
    movlw  .2
    SUBWF  adcL,1
    btfss  STATUS,C
    goto   a4
    incf   temp			    ;increment temp each time you subtract
    goto   a3


a4
    ;offset temperature readings, subtract 1 if temp is above 26
    movfw temp
    sublw .26
    btfss STATUS,C
    decf  temp

    ;check if temp is equal to set temp;
    movf  temp,W
    subwf setTemp, 0
    btfsc STATUS,Z
    goto cook			    ;if equal skip to cook


    ;separating temp into 2 digits
    ;to get tens value subtract 10 till value is less that zero
    movlw  .10
    SUBWF  temp,1
    btfss  STATUS,C
    goto   a5
    incf   CNT1			    ;increment CNT1 every time you subtract
    goto   a4			    ;if still above 0, goto a4 and repeat

a5
    movf   CNT1,W   
    call   LOOKSUB2		    ;call lookup table
    movwf  disp1		    ;save returned 7segment code to disp1

    movf   temp,w 
    addlw  .10  
    call   LOOKSUB2		    ;call lookup table
    movwf  disp2		    ;save returned 7segment code to disp2

    ;small delay before repeating
    movlw  .3
    call DELAY
    goto   Check_Menu1_Selected
    
cook
    clrf  disp1

    ;start 5s timer
    movlw  .5
    movwf   flag
    clrf    disp1		    ;turn off disp1
d
    movf    flag,w
    call    LOOKSUB2		    ;call lookup table
    movwf   disp2		    ;save returned 7segment code to dispC
    movlw   .50
    call    DELAY
    decfsz  flag
    goto    d

    ;banksel PORTA
    bsf PORTB,4			    ;turn buzzer on
    clrf CNT1

Count_Down
;*******************************************************************
    ;count down timer
    movf   timerr, w
    movwf  CNT2
 q0
    ;get tens value
    movlw  .10
    SUBWF  CNT2,1
    btfss  STATUS,C
    goto   Display
    incf   CNT1
    goto   q0

Display
    ;display tens value on disp1
    movf   CNT1,W   
    call   LOOKSUB2			    ;call lookup table
    movwf  disp1			    ;save returned 7segment code to disp1

    ;display units value on disp2
    movf   CNT2,w 
    addlw  .10  
    call   LOOKSUB2			    ;call lookup table
    movwf  disp2			    ;save returned 7segment code to disp2

    ;delay
    movlw  .50
    call  DELAY
    clrf CNT1
    clrf CNT2

    ;decrement timer, go back to the top Count_Down is not zero yet
    decfsz  timerr
    goto  Count_Down


    ;count down over
    banksel PORTA
    bcf   PORTB,4  ;buzzer off
    bsf   PORTA,2  ;green led on
    bsf   PORTA,2  ;green led on
    bcf   PORTA,1  ;red led off
    ;move 3 to flag memory and go back to the menu
    movlw    .3
    movwf   flag
    clrf    disp1
    goto   Cook_Options  


;**************************************************************
DELAY   ;delay function
	movlw	.10			;Load counter
DELAY_VAR
	movwf	CNT3
L3	movlw	.100			;Load counter
	movwf	CNT2
L2	movlw	.249			;Load counter
	movwf	CNT1

L1	NOP
	DECFSZ	CNT1,f			;Decrement Counter
	GOTO 	L1			;Repeat if not Zero
	DECFSZ	CNT2,f			;Decrement Counter
	GOTO 	L2			;Repeat if not Zero
	DECFSZ	CNT3,f			;Decrement Counter
	GOTO 	L4			;go to L4
    return

L4  btfss       PORTA,5   ;check if button is pressed
    goto        L3
    return
    

;******************************************************************************************
;*************************************************************************
;Enter with offset value in W, exit data byte in W
;org 0xff
LOOKSUB2
    movwf   store
    movlw   high A1			;Get high base address
    movwf   PCLATH
    movf    store,W
    addwf   PCL,f			;change PC to execute jump
	
A1	
    RETLW   b'00111111'   ;0
    RETLW   b'00000110'   ;1
    RETLW   b'01011011'   ;2
    RETLW   b'01001111'   ;3
    RETLW   b'01100110'   ;4
    RETLW   b'01101101'   ;5
    RETLW   b'01111101'   ;6
    RETLW   b'00000111'   ;7
    RETLW   b'01111111'   ;8
    RETLW   b'01100111'   ;9

;**************************************************************
 A2D  ;Function for reading ADC value
     banksel ADCON0
     movwf  ADCON0
     call   SetupDelay
     bcf    PIR1,ADIF
     bsf    ADCON0,GO

DEL2
     btfss  PIR1,ADIF
     goto   DEL2
     bcf    PIR1,ADIF
     banksel  ADRESL
     movf    ADRESL,W
     banksel ADCON0
     return

;***************************************************************************************	
;A2D delay for stabilisation of hold capacitor.	
SetupDelay
    movlw  D'20'
    movwf  CNT1
SD
    decfsz  CNT1,F
    goto    SD
    return0
;**************************************************************

    org	    2007				;Configure PIC
    DW	    30C4				;Internal Osc, power-up enable etc.
    END