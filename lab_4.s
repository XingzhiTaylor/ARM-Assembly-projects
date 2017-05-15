;*-------------------------------------------------------------------
;* Name:        lab_4_program.s 
;* Purpose:     A sample style for lab-4
;* Term:        Winter 2014
;*-------------------------------------------------------------------
                THUMB                                 ; Declare THUMB instruction set 
                AREA     My_code, CODE, READONLY     ; 
                EXPORT         __MAIN                     ; Label __MAIN is used externally 
                EXPORT         EINT3_IRQHandler     ; without this the interupt routine will not be found

                ENTRY 

__MAIN

; The following lines are similar to previous labs.
; They just turn off all LEDs 
                LDR            R10, =LED_BASE_ADR        ; R10 is a  pointer to the base address for the LEDs
                MOV         R3, #0xB0000000        ; Turn off three LEDs on port 1  
                STR         R3, [r10, #0x20]
                MOV         R3, #0x0000007C
                STR         R3, [R10, #0x40]     ; Turn off five LEDs on port 2 

; This line is very important in your main program
; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
                MOV            R6, #0xABCD        ; Init the random number generator with a non-zero number
LOOP 
                LDR R2, =IO2IntEnf          ;This register can enable the falling edge interrupt. R2 now points to it
                LDR R4, [R2]                ;Load its value to R4
                ORR R4, #0X400              ;Set the 10th bit to 1. This enables the falling edge interrupt
                STR R4, [R2]                ;Store the set value back to the register
                
                MOV R2, #0                  ;Clear R2
                MOV R4, #0                  ;Clear R4
                
                LDR R2, =ISER0              ;This register can enable the EINT3 channel which is shared with GPIO interrupts with ISER0
                LDR R4, [R2]                ;Load its value to R4
                MOVT R5, #0X0020            ;200000 is too big for a move operation. Uew MOVT
                ORR R4, R5                  ;Set the 21th bit to 1. This enables the EINT3 channel
                STR R4, [R2]                ;Store the set value back to the register

PICKYLOOP                                        ;PICKYLOOP gets the random numbers and checks if they're between 5 and 25
                BL             RNG                  ;Generate a random number
                AND R6, R6, #0xFF                ;This is basically mod 256 (0 - 31) (picking last 5 bits). Now the value in R6 can be any number between 0 and 31.
                CMP R6, #50                        ;If R6 is less than 5
                BLT         PICKYLOOP           ;Generate another random number
                CMP R6, #250                     ;If R6 is greater than 25
                BGT         PICKYLOOP            ;Generate another random number    
                ;MOV R5, #10                        
                ;MUL R6, R6, R5                    ;Multiply the random number by 10 to get the delay in 0.1 seconds
                
DISPLAY            MOV R1, R6                      ;R1 is the register used in the desplay subroutine. Move the value of R6 to R1
                BL DISPLAY_NUM                  ;Display it                
                MOV R0, #10                      ;Delay for 1s
                BL DELAY                         
                SUBS R6, #10                    ;Decrement the counter, one less sec to display
                BGT DISPLAY                     ;If there are still data to display (R6 is not 0 yet), go back and display it
                B   BLINK                       ;If R6 reaches 0, blink the LED
                
                B             LOOP
        
BLINK            CMP R6, #0                      ;If R6 is no longer 0 because of the interrupt
                BPL DISPLAY                     ;Branch back and display the new random number
                MOV R1, #0                      ;If R6 is still 0
                BL DISPLAY_NUM                  ;Turn off the led
                MOV R0, #1                      ;Delay for 0.1 sec
                BL DELAY 
                CMP R6, #0                      ;If R6 is no longer 0 because of the interrupt
                BPL DISPLAY                     ;Branch back and display the new random number
                MOV R1, #255                    ;If R6 is still 0
                BL DISPLAY_NUM                  ;Turn on the led
                MOV R0, #1                      ;Delay for 0.1 sec. This gives a 10 Hz blink
                BL DELAY 
                B BLINK
        
        
; Display the number in R3 onto the 8 LEDs
DISPLAY_NUM        STMFD        R13!,{R1, R2, R3, R5, R14}
                MOV R5, #0                       ;CLEAR R5          
                BFI R5, R1, #0, #5               ;Copy the 5 right most bits in R1 to r5. These bits are to be displayed on port 2
                RBIT R5, R5                      ;Reverse the bit 
                EOR R5, R5, #0xFFFFFFFF          ;Inverse the bit since 1 is for off and 0 is for on
                LSR R5, #25                      ;Right shift the bits so that the left most bit of R5 is at bit position 6 in P2
                STR R5, [R10, #0x40]             ;Write the bits to the led (Port P2)               
                
                MOV R5, #0                       ;Clear r5
                LSR R1, #5
                BFI R5, R1, #0, #3               ;Get the rest 3 bits from r1 and store them in r5, from position 0-2
                LSL R5, #1                       ;Shift them, now they are at position 1-3
                AND R3, R5, #2                   ;Get the left most bit
                ADD R5, R3, LSR #1               ;Put it back to position 0, so the three bits are now at position 0,2,3
                RBIT R5, R5                      ;Reverse the bit, now the three bits are at position 31 29 28
                EOR R5, R5, #0xFFFFFFFF          ;Inverse the bit
                STR R5, [R10, #0x20]             ;Write the bits to the led (Port P1)
; Useful commands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations

                LDMFD        R13!,{R1, R2, R3, R5, R15}
                
                
                
;*------------------------------------------------------------------- 
; Subroutine RNG ... Generates a pseudo-Random Number in R6 
;*------------------------------------------------------------------- 
; R6 holds a random number as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R6 MUST be initialized to a non-zero 16-bit value at the start of the program
; R6 can be read anywhere in the code but must only be written to by this subroutine
RNG             STMFD        R13!,{R1-R3, R14}     ; Random Number Generator 
                AND            R1, R6, #0x8000
                AND            R2, R6, #0x2000
                LSL            R2, #2
                EOR            R3, R1, R2
                AND            R1, R6, #0x1000
                LSL            R1, #3
                EOR            R3, R3, R1
                AND            R1, R6, #0x0400
                LSL            R1, #5
                EOR            R3, R3, R1            ; The new bit to go into the LSB is present
                LSR            R3, #15
                LSL            R6, #1
                ORR            R6, R6, R3
                LDMFD        R13!,{R1-R3, R15}

;*------------------------------------------------------------------- 
; Subroutine DELAY ... Causes a delay of 1ms * R0 times
;*------------------------------------------------------------------- 
;
;        Delay 0.1ms (100us) * R0 times
;         aim for better than 10% accuracy
DELAY            STMFD        R13!,{R2, R14}
MultipleDelay        TEQ        R0, #0 ; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE
        MOV         R2, #0xFBD0   ; initialize R2 to 130000 in decimal (0.1s)
        MOVT        R2, #0x0001
delay_loop
        SUBS R2, R2, #1            ;R2 is a counter that decrements from 0x140000 to 0. When R2 is 0, we jump out of this loop. The time it takes to run the loop is approximately 1s
        BNE  delay_loop            ;If the counter R2 is not 0, return back to the delay loop
        SUBS  R0, #1                ;Every time we finish a loop, the loop counter decrement by 1
        BEQ  exitDelay             ;If R0 is 0, that means we don't need to loop again
        BNE  MultipleDelay           ;If R0 is not 0, that means we need another delay loop
exitDelay        LDMFD        R13!,{R2, R15}

; The Interrupt Service Routine MUST be in the startup file for simulation 
;   to work correctly.  Add it where there is the label "EINT3_IRQHandler
;
;*------------------------------------------------------------------- 
; Interrupt Service Routine (ISR) for EINT3_IRQHandler 
;*------------------------------------------------------------------- 
; This ISR handles the interrupt triggered when the INT0 push-button is pressed 
; with the assumption that the interrupt activation is done in the main program
EINT3_IRQHandler     
                    STMFD R13! ,{R2, R3, R4, R14}                 ; Use this command if you need it
                CMP         R6, #0              ;If R6 is 0 when entering the ISR, assign it a non-zero value
                MOVEQ        R6, #0xABC6    
PICKYLOOP2                                        ;PICKYLOOP gets the random numbers and checks if they're between 5 and 25
                BL             RNG      
                AND R4, R6, #0xFF                ;This is basically mod 32 (0 - 31) (picking last 5 bits)
                CMP R4, #50                        
                BLT         PICKYLOOP2
                CMP R4, #250
                BGT         PICKYLOOP2                
                MOV R6, R4                      ;The same picky loop as before
                
                
                
                LDR R2, =IO2INTCLR              ;This register clears the interrupt bit and let R2 points to it
                LDR R3, [R2]                    ;Load its value into R3
                ORR R3, #0X400                  ;Set the 10th bit to 1 and the interrupt signal is cleared
                STR R3, [R2]                    ;Write the value to the register
                
                    LDMFD R13! ,{R2, R3, R4, R15}                ; Use this command if you used STMFD (otherwise use BX LR) 


;*-------------------------------------------------------------------
; Below is a list of useful registers with their respective memory addresses.
;*------------------------------------------------------------------- 
LED_BASE_ADR    EQU     0x2009c000         ; Base address of the memory that controls the LEDs 
PINSEL3            EQU     0x4002C00C         ; Pin Select Register 3 for P1[31:16]
PINSEL4            EQU     0x4002C010         ; Pin Select Register 4 for P2[15:0]
FIO1DIR            EQU        0x2009C020         ; Fast Input Output Direction Register for Port 1 
FIO2DIR            EQU        0x2009C040         ; Fast Input Output Direction Register for Port 2 
FIO1SET            EQU        0x2009C038         ; Fast Input Output Set Register for Port 1 
FIO2SET            EQU        0x2009C058         ; Fast Input Output Set Register for Port 2 
FIO1CLR            EQU        0x2009C03C         ; Fast Input Output Clear Register for Port 1 
FIO2CLR            EQU        0x2009C05C         ; Fast Input Output Clear Register for Port 2 
IO2IntEnf        EQU        0x400280B4        ; GPIO Interrupt Enable for port 2 Falling Edge 
ISER0            EQU        0xE000E100        ; Interrupt Set-Enable Register 0 
;ISER0            EQU        0xE000E100        ; Interrupt Set-Enable Register 0 
IO2INTCLR        EQU        0x400280AC        ; Interrupt Port 2 Clear Register

                ALIGN 

                END
