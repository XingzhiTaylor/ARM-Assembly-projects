;*----------------------------------------------------------------------------
;* Name:    Lab_1_program.s 
;* Purpose: This code flashes one LED at approximately 1 Hz frequency 
;* Author: 	Rasoul Keshavarzi 
;*----------------------------------------------------------------------------*/
	THUMB		; Declare THUMB instruction set 
	AREA		My_code, CODE, READONLY 	; 
	EXPORT		__MAIN 		; Label __MAIN is used externally q
	ENTRY 
__MAIN
; The following operations can be done in simpler methods. They are done in this 
; way to practice different memory addressing methods. 
; MOV moves into the lower word (16 bits) and clears the upper word
; MOVT moves into the upper word
; show several ways to create an address using a fixed offset and register as offset
;   and several examples are used below
; NOTE MOV can move ANY 16-bit, and only SOME >16-bit, constants into a register
; BNE and BEQ can be used to branch on the last operation being Not Equal or EQual to zero
;
	MOV 		R2, #0xC000		; move 0xC000 into R2
	MOV 		R4, #0x0		; init R4 register to 0 to build address
	MOVT 		R4, #0x2009		; assign 0x20090000 into R4
	ADD 		R4, R4, R2 		; add 0xC000 to R4 to get 0x2009C000 

	MOV 		R3, #0x0000007C	; move initial value for port P2 into R3 
	STR 		R3, [R4, #0x40] ; Turn off five LEDs on port 2 

	MOV 		R3, #0xB0000000	; move initial value for port P1 into R3
	STR 		R3, [R4, #0x20]	; Turn off three LEDs on Port 1 using an offset

	MOV 		R2, #0x20		; put Port 1 offset into R2 for use later

	MOV 		R0, #0xFFFF 	; Initialize R0 lower word for countdown

	MOV 		R8, #0xB0000000	;initialize R8 to B0000000. After toggling, it's A000000 that can turn a light on
outer_loop

	MOV         R6, #0x140000   ; initialize R6 to 1000

delay_loop
        SUBS R6, R6, #1         ;R6 is a counter that decrements from 0x140000 to 0. When R6 is 0, we jump out of this loop. The time it takes to run the loop is approximately 1s
        BNE  delay_loop	        
		
    EOR         R8, #0x10000000 ;use xor to toggle the 28th bit, switching R8 between A0000000 and B0000000
	STR 		R8, [R4, #0x20]	;write R8 into the first LED light

	B 		outer_loop		; This branch needs to be fixed!

 	END 
