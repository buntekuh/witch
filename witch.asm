  DEVICE ZXSPECTRUM48
; the goal of this script is to write a function
; that will print a given char sprite at x,y location on the screen

; we expect the location of the sprite to be stored in
; x: d, y: e
; c contains the bitmask value to print
	org $8000
	jp start
; simple 16 byte witch character
; the zx spectrum attribute bits set F -Flash, B - Brightness, P1, P2, P3 Paper and I1,I2, I3 - Ink as follows: F B P1 P2 P3 I1 I2 I3
birdie:
	defb 2			; sprite is 2 characters wide
	defb 2			; sprite is 2 characters high
	; 4 x attribute value
	defb %00000110	; yellow on black
	defb %00000101	; cyan on black
	defb %00000010	; red on black
	defb %00000101	; cyan on black

	defb %00001010
	defb %00101001
	defb %10011111
	defb %01110001
	defb %00100101
	defb %11110001
	defb %00011111
	defb %00000110

	defb %00001111
	defb %00011111
	defb %00011010
	defb %00111010
	defb %00101010
	defb %00101111
	defb %00001001
	defb %00001101

	defb %0
	defb %0
	defb %0
	defb %0
	defb %11000000
	defb %11111100
	defb %0
	defb %0

	defb %00000000
	defb %11000000
	defb %11000000
	defb %10100000
	defb %10100000
	defb %10000000
	defb %0
	defb %10000000
witch:
	defb %01100000
	defb %01111110
	defb %01111111
	defb %11000110
	defb %11010100
	defb %11010111
	defb %01000001
	defb %00110110

	defb %00010100
	defb %00100010
	defb %01000010
	defb %10000001
	defb %11100001
	defb %00111111
	defb %00100100
	defb %00110110

start:
  ld a, 4             ;0=blk,1=blu,2=red,3=mag,4=grn,5=cyn,6=yel,7=wht
  call 8859           ;Set border to color stored in a

	ld d, 132			;center sprite horizontally
	ld e, 97			;center sprite vertically

  ld c, %00101010     ; red on yellow
  call set_color_block

	ld hl, witch		; first byte of sprite
	ld b, 16			; use b as a counter over the sprite bytes

loop_sprite_values:
	ld a, (hl)
	ld c, a				; save sprite bitmap value in c

	push hl
	push bc
	call calculate_screen_address
	pop bc
	pop hl

	inc hl				; next sprite line
	inc e				; next lower position on screen
	djnz loop_sprite_values			; loop while counter is bigger than 0

	ret

; 010TTLLL CCCccccc
; T – these two bits refer to which third of the screen is being addressed: 00 – Top, 01 – Middle, 10 – Bottom
; L – these three bits indicate which line within a character is being addressed: from 0 – 7, or 000 – 111 in binary
; C – these three bits indicate which character row is being addressed: from 0 – 7
; c – these five bits refer to which character column is being addressed: from 0 – 31
; The top three bits ( 010 ) of the high byte don’t change.

;  Effectively we only have to turn around the order of bits as follows:
;	010 Y7 Y6 Y2 Y1 Y0   Y5 Y4 Y3 X7 X6 X5 X4 X3

;	; Input  DE= XY
;	; output HL= screen mem pos
calculate_screen_address:
	; calculate high byte of the screen address and store in H register
	; moving Y2 Y1 Y0 into LLL
	ld a, e				; load y into accumulator
	and %00000111		; isolate the 3 bits
	ld h, a				; save LLL into high byte, -----LLL is set
	; Moving Y7 Y6 into TT
	ld a, e				; load y into accumulator
	rra					; rotate right 3 times to get them into position
	rra
	rra
	and %00011000		; isolate the 2 bits
	or h				; save into accumulator, ---TTLLL
	; setting the top three bits that never change, screen space starts at 16kb being %01000000
	or %01000000		; set the top 3 bits
	ld h, a				; save in high byte, 010TTLLL all bits in high byte are set

	; Calculate low byte of screen address and store in L register
	; move X4 X3 X2 X1 X1 into ccccc
	ld a, d				; load x into accumulator
	rra					; rotate them right 3 times to get them into position
	rra
	rra
	and %00011111		; ensure the top 3 bits are clear
	ld l, a				; save into low byte, ---ccccc is set
	; Moving Y5 Y4 Y3 into CCC
	ld a, e				; load y into accumulator
	rla					;rotate left into position
	rla
	and %11100000		;isolate the 3 bits
	or l				; combine what is already set in l into a, CCCccccc is set
	ld l, a				; save in low byte

	; one byte in screen memory contains 8 pixel. If x position is not divisble by 8 the bits need to be shifted right
	; Calculate pixel postion and store in A reg.
	ld a, d				; load x into accumulator
	and %00000111		; how often do we need to shift right

	jr z, put_sprite_on_screen	; no need to shift if a is zero

	ld b, a				; make b our counter, how often we'll need to shift
	ld a, 0				; a receives the shifted right hand part of the image

shift_into_next_byte:
	srl c				; shift into carry
	rra					; shift carry into a
	djnz shift_into_next_byte ; decrement counter in b, if it is not zero repeat shift

put_sprite_on_screen:
	ld (hl), c
	inc hl
	ld (hl), a
	ret

; Input  DE= XY (x=bytes - so 32 across) not bc
; Input  c= attribute color to set
; store screen address in hl
; The screen address is at 01011 Y7 Y6 Y5 Y4 Y3 X7 X6 X5 X4 X3
; Register H will contain 01011 Y7 Y6
; Register L will contain Y5 Y4 Y3 X7 X6 X5 X4 X3
set_color_block:			;YYYYYyyy 	Color ram is in 8x8 tiles
  ; store Y Y6 in H
  ld a, e       ; ld y into accumulator
  and %11000000 ; isolate the 5 bits, remove the three insignificant bits: YY00000
  rlca          ; rotate left into carry and rotate carry into 2 times
  rlca          ; this moves the bits into position 00000YY
  add %01011000 ; add the memory address the attribute memory starts at
  ld h, a       ; save in High register 010110YY

; store Y5 Y4 Y3 into L
  ld a, e       ; ld y into accumulator
  and %00111000  ; isolate the three bits 00YYY00
  rla           ; rotate into position
  rla           ; YYY00000
  ld l, a       ; store in Low register
  ld a, d       ; load x into accumulator
  and %11111000 ; isolate the 5 bits XXXXX000
  rra           ; rotate into position
  rra
  rra           ; 000XXXXX
  or l          ; combine with value in l YYYXXXXX
  ld l, a       ; store in l

  ld (hl), c    ; poke memory address with attribute value
  ret


  /* ld a, e							;so low three Y bits are ignored
  and %11000000	;YY------
  rlca
  rlca			;------YY
  add %01011000 		;color ram base
  ld h, a
  ld a, e
  and %00111000		;--YYY---
  rlca
  rlca				;YYY-----

  add d				;Add Xpos
  ld l, a

  ld (hl), c
	ret */


; Deployment: Snapshot
	SAVESNA "witch.sna", start
