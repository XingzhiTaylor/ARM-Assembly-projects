; ECE-222 Lab ... Winter 2013 term 
; Lab 3 sample code 
                THUMB         ; Thumb instruction set 
                AREA         My_code, CODE, READONLY
                EXPORT         __MAIN
                ENTRY  
__MAIN

; The following lines are similar to Lab-1 but use a defined address to make it easier.
; They just turn off all LEDs 
                LDR            R10, =LED_BASE_ADR        ; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports

                MOV         R3, #0xB0000000        ; Turn off three LEDs on port 1  
                STR         R3, [r10, #0x20]
                MOV         R3, #0x0000007C
                STR         R3, [R10, #0x40]     ; Turn off five LEDs on port 2 
                
                MOV R6, #0x00
                STRB R6, [R10, #0x41]           ;Set p2 as an input pin. P2.10 now contains the input

; This line is very important in your main program
; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
                MOV            R11, #0xABCD        ; Init the random number generator with a non-zero number
loop             BL             RandomNum     
                MOV R0, R11                     ;Store the random number in r0 for the length of random delay
                BL DELAY                        ;Go for the delay
                STR R3, [r10, #0x20]            ;After the delay, turn on a led
                MOV R9, #0                      ;Initialize the counter
                
POLL            MOV R4, #4                      ;Initialize the display counter
                MOV R0, #1                      ;Only one delay loop each time
                BL DELAY                        ;Go for a 0.1 ms delay loop
                ADD R9, R9, #1                  ;counter increments by 1
                LDR R6, =FIO2PIN                ;r6 is the pointer pointing to the input pin
                LDR R6, [R6]                    ;Now r6 contains the data in p2. The 10th bit of r6 should be the input
                MOV R7, R6, LSR #10             ;Right shift R6 by 10 bits to check the input
                ANDS R7, R7, #1                 ;Take the bit out
                MOV R8, R9                      
                TEQ R7, #0                      ;If the input bit is 1, display the counter value, otherwise keep polling
                BEQ DISPLAY
                BNE POLL

DISPLAY_AGAIN
                MOV R0, #50000                  ;Go for a 5s delay between bytes
                BL DELAY                         
                MOV R8, R9                      ;Reset R8
                MOV R4, #4                      ;Reset the display counter
                
DISPLAY            MOV R1, #0                      ;Clear R1
                BFI R1, R8, #0, #8              ;Move 8 bits into R1
                LSR R8, #8                      ;Shift R8 to get the next 8 bits
                BL DISPLAY_NUM                  ;Display it
                MOV R0, #20000                  ;Delay for 2s
                BL DELAY 
                SUBS R4, #1                     ;Decrement the counter, one less byte to display
                BNE DISPLAY                     ;If there are still data to display, go back and display it
                BEQ DISPLAY_AGAIN               ;If no data to display, prepare for the next round of display
 
                B loop

;
; Display the number in R3 onto the 8 LEDs
DISPLAY_NUM        STMFD        R13!,{R1, R2, R3, R5, R14}
                MOV R5, #0                       ;CLEAR R5          
                BFI R5, R1, #0, #5               ;Copy the 5 right most bits in R1 to r5. These bits are to be displayed on port 2
                RBIT R5, R5                      ;Reverse the bit 
                EOR R5, R5, #0xFFFFFFFF          ;Inverse the bit since 1 is for off and 0 is for on
                LSR R5, #25                      ;Put the five bits to position 2-6
                STR R5, [R10, #0x40]             ;Write the bits to the led                 
                
                MOV R5, #0                       ;Clear r5
                LSR R1, #5
                BFI R5, R1, #0, #3               ;Get the rest 3 bits from r1 and store them in r5, from position 0-2
                LSL R5, #1                       ;Shift them, now they are at position 1-3
                AND R3, R5, #2                   ;Get the left most bit
                ADD R5, R3, LSR #1               ;Put it back to position 0, so the three bits are now at position 0,2,3
                RBIT R5, R5                      ;Reverse the bit, now the three bits are at position 31 29 28
                EOR R5, R5, #0xFFFFFFFF          ;Inverse the bit
                STR R5, [R10, #0x20]             ;Write the bits to the led
; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations

                LDMFD        R13!,{R1, R2, R3, R5, R15}

;
; R11 holds a 16-bit random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R11 holds a non-zero 16-bit number.  If a zero is fed in the pseudo-random sequence will stay stuck at 0
; Take as many bits of R11 as you need.  If you take the lowest 4 bits then you get a number between 1 and 15.
;   If you take bits 5..1 you'll get a number between 0 and 15 (assuming you right shift by 1 bit).
;
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program OR ELSE!
; R11 can be read anywhere in the code but must only be written to by this subroutine
RandomNum        STMFD        R13!,{R1, R2, R3, R14}

                AND            R1, R11, #0x8000
                AND            R2, R11, #0x2000
                LSL            R2, #2
                EOR            R3, R1, R2
                AND            R1, R11, #0x1000
                LSL            R1, #3
                EOR            R3, R3, R1
                AND            R1, R11, #0x0400
                LSL            R1, #5
                EOR            R3, R3, R1        ; the new bit to go into the LSB is present
                LSR            R3, #15
                LSL            R11, #1
                ORR            R11, R11, R3
                
                LDMFD        R13!,{R1, R2, R3, R15}

;
;        Delay 0.1ms (100us) * R0 times
;         aim for better than 10% accuracy
DELAY            STMFD        R13!,{R2, R14}
MultipleDelay        TEQ        R0, #0 ; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE
        MOV         R2, #130   ; initialize R2 to 655360 in decimal         
delay_loop
        SUBS R2, R2, #1            ;R2 is a counter that decrements from 0x140000 to 0. When R2 is 0, we jump out of this loop. The time it takes to run the loop is approximately 1s
        BNE  delay_loop            ;If the counter R2 is not 0, return back to the delay loop
        SUBS  R0, #1                ;Every time we finish a loop, the loop counter decrement by 1
        BEQ  exitDelay             ;If R0 is 0, that means we don't need to loop again
        BNE  MultipleDelay           ;If R0 is not 0, that means we need another delay loop
exitDelay        LDMFD        R13!,{R2, R15}
                

LED_BASE_ADR    EQU     0x2009c000         ; Base address of the memory that controls the LEDs 
PINSEL3            EQU     0x4002c00c         ; Address of Pin Select Register 3 for P1[31:16]
PINSEL4            EQU     0x4002c010         ; Address of Pin Select Register 4 for P2[15:0]
FIO2PIN         EQU     0x2009c054
;    Usefull GPIO Registers
;    FIODIR  - register to set individual pins as input or output
;    FIOPIN  - register to read and write pins
;    FIOSET  - register to set I/O pins to 1 by writing a 1
;    FIOCLR  - register to clr I/O pins to 0 by writing a 1

                ALIGN 

                END
