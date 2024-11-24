; Programming Games for NES - Megablast

;*****************************************************************
; Define NES cartridge Header
;*****************************************************************

.segment "HEADER"
INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID 
.byte $02 ; 16k PRG bank count
.byte $01 ; 8k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;*****************************************************************
; Import both the background and sprite character sets
;*****************************************************************

.segment "TILES"
.incbin "megablast.chr"

;*****************************************************************
; Define NES interrupt vectors
;*****************************************************************

.segment "VECTORS"
.word nmi
.word reset
.word irq

;*****************************************************************
; 6502 Zero Page Memory (256 bytes)
;*****************************************************************

.segment "ZEROPAGE"

time: .res 2
lasttime: .res 1

d_x:					 .res 1 ; x direction of ball
d_y:					 .res 1 ; y direction of ball

frame_timer: 			 .res 1 ; frame timer

score: 					 .res 3
update: 				 .res 1

temp: .res 10

snake_size:				 .res 1 ; score
snake_current_loop_size: .res 1 

temp_srcX_address: .res 4
temp_dstX_address: .res 4

temp_srcY_address: .res 4
temp_dstY_address: .res 4


;*****************************************************************
; Sprite OAM Data area - copied to VRAM in NMI routine
;*****************************************************************

.segment "OAM"
oam: .res 256	; sprite OAM data

;*****************************************************************
; Include NES Function Library
;*****************************************************************

.include "neslib.s"

;*****************************************************************
; Remainder of normal RAM area
;*****************************************************************

.segment "BSS"
palette: .res 32 ; current palette buffer

;*****************************************************************
; Main application entry point for starup/reset
;*****************************************************************

.segment "CODE"
.proc reset
	sei			; mask interrupts
	lda #0
	sta PPU_CONTROL	; disable NMI
	sta PPU_MASK	; disable rendering
	sta APU_DM_CONTROL	; disable DMC IRQ
	lda #40
	sta JOYPAD2		; disable APU frame IRQ

	cld			; disable decimal mode
	ldx #$FF
	txs			; initialise stack

	; wait for first vBlank
	bit PPU_STATUS
wait_vblank:
	bit PPU_STATUS
	bpl wait_vblank

	; clear all RAM to 0
	lda #0
	ldx #0
clear_ram:
	sta $0000,x
	sta $0100,x
	sta $0200,x
	sta $0300,x
	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x
	inx
	bne clear_ram

	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
clear_oam:
	sta oam,x
	inx
	inx
	inx
	inx
	bne clear_oam

; wait for second vBlank
wait_vblank2:
	bit PPU_STATUS
	bpl wait_vblank2
	
	; NES is initialized and ready to begin
	; - enable the NMI for graphical updates and jump to our main program
	lda #%10001000
	sta PPU_CONTROL
	jmp main
.endproc

;*****************************************************************
; NMI Routine - called every vBlank
;*****************************************************************

.segment "CODE"
.proc nmi
	; save registers
	pha
	txa
	pha
	tya
	pha

	; incrememt our time tick counter
	inc time
	bne :+
		inc time + 1
	:
	inc frame_timer ; increment frame 

	bit PPU_STATUS
	; transfer sprite OAM data using DMA
	lda #>oam
	sta SPRITE_DMA

	; transfer current palette to PPU
	vram_set_address $3F00
	ldx #0 ; transfer the 32 bytes to VRAM
@loop:
	lda palette, x
	sta PPU_VRAM_IO
	inx
	cpx #32
	bcc @loop

	lda #%00000001 ; has the score updated?
	bit update
	beq @skipscore
		jsr display_score ; display score
		lda #%11111110 ; reset score update flag
		and update
		sta update
@skipscore:

	; write current scroll and control settings
	lda #0
	sta PPU_VRAM_ADDRESS1
	sta PPU_VRAM_ADDRESS1
	lda ppu_ctl0
	sta PPU_CONTROL
	lda ppu_ctl1
	sta PPU_MASK

	; flag PPU update complete
	ldx #0
	stx nmi_ready

	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti
.endproc

;*****************************************************************
; IRQ Clock Interrupt Routine
;*****************************************************************

.segment "CODE"
irq:
	rti

;*****************************************************************
; Main application logic section includes the game loop
;*****************************************************************
 .segment "CODE"
 .proc main
 	; main application - rendering is currently off

 	; initialize palette table
 	ldx #0
paletteloop:
	lda default_palette, x
	sta palette, x
	inx
	cpx #32
	bcc paletteloop

 	; draw the title screen
	jsr display_title_screen

	; set our game settings
	lda #VBLANK_NMI|BG_0000|OBJ_1000
   	sta ppu_ctl0
   	lda #BG_ON|OBJ_ON
   	sta ppu_ctl1

	jsr ppu_update

	; wait for a gamepad button to be pressed
titleloop:
	jsr gamepad_poll
	lda gamepad
	and #PAD_A|PAD_B|PAD_START|PAD_SELECT
	beq titleloop

	lda time
	sta SEED0
	lda time+1
	sta SEED0+1
	jsr randomize
	sbc time+1
	sta SEED2   
	jsr randomize
	sbc time
	sta SEED2+1

	lda #0 ; reset the player's score
	sta score
	sta score+1
	sta score+2

	; draw the game screen
	jsr display_game_screen

	; place head segment
 	lda #64
 	sta oam ; set Y
 	lda #16
 	sta oam + 3 ; set X
 	lda #0
 	sta oam + 1 ; set pattern
 	lda #0
 	sta oam + 2 ; set attributes

	; place first body segment
 	lda #64
 	sta oam + (1 * 4) ; set Y
 	lda #8
 	sta oam + (1 * 4) + 3 ; set X
 	lda #0
 	sta oam + (1 * 4) + 1 ; set pattern
 	lda #0
 	sta oam + (1 * 4) + 2 ; set attributes

	; place second body segment
 	lda #64
 	sta oam + (2 * 4); set Y
 	lda #0
 	sta oam + (2 * 4) + 3 ; set X
 	lda #0
 	sta oam + (2 * 4) + 1 ; set pattern
 	lda #0
 	sta oam + (2 * 4) + 2 ; set attributes

	; place food
	jsr rand
	and #%00000111
	asl
	asl
	asl
	clc
	adc #80
	sta oam + (60 * 4)

	lda #0
	sta oam + (60 * 4) + 1 ; set pattern
 	lda #1
 	sta oam + (60 * 4) + 2 ; set attributes

	jsr rand
	and #%00000111
	asl
	asl
	asl
	sta oam + (60 * 4) + 3

	lda #2
	sta snake_size

	jsr ppu_update

mainloop:
	lda time
	; ensure the time has actually changed
	cmp lasttime
	beq mainloop
	; time has changed update the lasttime value
	sta lasttime

	lda frame_timer
	cmp #20
	bne END_UPDATE_TIMER
		lda #0
		sta frame_timer

		jsr update_body

		lda oam
		cmp oam + (60 * 4)
		bne END_UPDATE_TIMER
			lda oam + 3
			cmp oam + (60 * 4) + 3
			bne END_UPDATE_TIMER
				jsr rand
				and #%00000111
				asl
				asl
				asl
				clc
				adc #80
				sta oam + (60 * 4)

				jsr rand
				and #%00000111
				asl
				asl
				asl
				clc
				sta oam  + (60 * 4) + 3

				lda #1
				jsr add_score

				inc snake_size

				jsr update_body



	END_UPDATE_TIMER:

	jsr player_actions
	;jsr move_player_bullet

 	jmp mainloop
.endproc

;*****************************************************************
; Check for the game controller, move the player or fire a bullet
;*****************************************************************
.segment "CODE"

.proc player_actions
	jsr gamepad_poll
	; now move the bat if left or right pressed
 	lda gamepad
 	and #PAD_L
 	beq NOT_GAMEPAD_LEFT
 		; gamepad has been pressed left
		lda d_x
		cmp #0
		bne NOT_GAMEPAD_LEFT

		lda #$F8
		sta d_x
		lda #0
		sta d_y
NOT_GAMEPAD_LEFT:

 	lda gamepad
 	and #PAD_R
 	beq NOT_GAMEPAD_RIGHT
		lda d_x
		cmp #0
		bne NOT_GAMEPAD_RIGHT

 		lda #$08
		sta d_x
		lda #$0
		sta d_y
NOT_GAMEPAD_RIGHT:

lda gamepad
     and #PAD_U
     beq NOT_GAMEPAD_UP
	 	lda d_y
		cmp #0
		bne NOT_GAMEPAD_UP

		lda #$F8
		sta d_y
		lda #$0
		sta d_x
 NOT_GAMEPAD_UP:

 lda gamepad
     and #PAD_D
     beq NOT_GAMEPAD_DOWN
	 	lda d_y
		cmp #0
		bne NOT_GAMEPAD_DOWN

        lda #$08
		sta d_y
		lda #0
		sta d_x ; change X to the left
 NOT_GAMEPAD_DOWN:

	rts
.endproc

;*****************************************************************
; Check for the game controller, move the player or fire a bullet
;*****************************************************************
.segment "CODE"

;.proc move_player_bullet
;	lda oam + 16
;	cmp #$FF ; see if bullet sprite is on screen
;	beq @exit
;		; bullet is on screen, move it up
;		sec
;		sbc #4
;		sta oam + 16
;		bcs @exit
;			; value carried so we have gone off the top of the screen
;			; hide bullet
;			lda #$FF
;			sta oam + 16
;
;@exit:
;	rts
;.endproc

;*****************************************************************
; Display Title Screen
;*****************************************************************
 .segment "CODE"

title_text:
.byte "U MOEDER",0

press_play_text:
.byte "PRESS FIRE TO BEGIN",0

title_attributes:
.byte %00000111,%00000111,%00000111,%00000111
.byte %00000111,%00000111,%00000111,%00000111

.proc display_title_screen
	jsr ppu_off ; Wait for the screen to be drawn and then turn off drawing

	jsr clear_nametable ; Clear the 1st name table

	; Write our title text
	vram_set_address (NAME_TABLE_0_ADDRESS + 4 * 32 + 6)
	assign_16i text_address, title_text
	jsr write_text

	; Write our press play text
	vram_set_address (NAME_TABLE_0_ADDRESS + 16 * 32 + 6)
	assign_16i text_address, press_play_text
	jsr write_text

	; draw a base line
	vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32 + 16)
	ldy #0
	lda #0 ; tile number to repeat
looptest:
	sta PPU_VRAM_IO
	iny
	cpy #1
	bne looptest

	; Set the title text to use the 2nd palette entries
	vram_set_address (ATTRIBUTE_TABLE_0_ADDRESS + 8)
	assign_16i paddr, title_attributes
	ldy #0
loop:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #8
	bne loop

	

	jsr ppu_update ; Wait until the screen has been drawn

	rts
.endproc

;*****************************************************************
; Display Main Game Screen
;*****************************************************************

.segment "RODATA"
; put the data in our data segment of the ROM
game_screen_mountain:
.byte 001,002,003,004,001,002,003,004,001,002,003,004,001,002,003,004
.byte 001,002,003,004,001,002,003,004,001,002,003,004,001,002,003,004
game_screen_scoreline:
.byte "SCORE 0000000"

.segment "ZEROPAGE"

paddr: .res 2 ; 16-bit address pointer

.segment "CODE"
.proc display_game_screen
	jsr ppu_off ; Wait for the screen to be drawn and then turn off drawing

	jsr clear_nametable ; Clear the 1st name table

	; output mountain line
	;vram_set_address (NAME_TABLE_0_ADDRESS + 22 * 32)
	;assign_16i paddr, game_screen_mountain
	ldy #0
loop:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #32
	bne loop

	; draw a base line
	vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32)
	ldy #0
	lda #9 ; tile number to repeat
loop2:
	sta PPU_VRAM_IO
	iny
	cpy #32
	bne loop2

	; output the score section on the next line
	assign_16i paddr, game_screen_scoreline
	ldy #0
loop3:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #13
	bne loop3

	jsr ppu_update ; Wait until the screen has been drawn
	rts
.endproc

;*****************************************************************
; add_score: Add to the players score
; Parameters:
; a = value to add to the score
;*****************************************************************
.segment "CODE"
.proc add_score
	clc
	adc score ; add the value in a to the 1st byte of the score
	sta score
	cmp #100
	bcc @skip

	sec ; 1st byte has exceeded 99, handle overflow
	sbc #100
	sta score
	inc score+1
	lda score+1
	cmp #100
	bcc @skip

	sec ; 2nd byte has exceeded 99, handle overflow
	sbc #100
	sta score+1
	inc score+2
	lda score+2
	cmp #100
	bcc @skip
	sec ; if 3rd byte has exceeded 99, adjust and discard overflow
	sbc #100
	sta score+2
	
@skip:
	lda #%000000001 ; set flag to write score to the screen
	ora update
	sta update

	;lda highscore+2
	;cmp score+2
	;bcc @highscore
	;bne @nothighscore

	;lda highscore+1
	;cmp score+1
	;bcc @highscore
	;bne @nothighscore

	;lda highscore
	;cmp score
	;bcs @nothighscore

;@highscore:
	;lda score
	;sta highscore
	;lda score+1
	;sta highscore+1
	;lda score+2
	;sta highscore+2

	lda #%00000010 ; set flag to write high score to the screen
	ora update
	sta update

@nothighscore:
	rts
.endproc

;*****************************************************************
; display_score: Write the score to the screen
;*****************************************************************
.segment "CODE"

.proc display_score
	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 6)

	lda score+2 ; transform each decimal digit of the score
	jsr dec99_to_bytes
	stx temp
	sta temp+1

	lda score+1
	jsr dec99_to_bytes
	stx temp+2
	sta temp+3

	lda score
	jsr dec99_to_bytes
	stx temp+4
	sta temp+5

	ldx #0 ; write the six characters to the screen
@loop:
	lda temp,x
	clc
	adc #48
	sta PPU_VRAM_IO
	inx
	cpx #6
	bne @loop
	lda #48 ; write trailing zero
	sta PPU_VRAM_IO

	vram_clear_address
	rts
.endproc

.segment "CODE"
.proc update_body
	lda snake_size
	sta snake_current_loop_size

	UPDATE_SNAKE_BODY:
		lda snake_current_loop_size
		cmp #0
		beq UPDATE_HEAD
			lda snake_current_loop_size
			asl
			asl
			sta temp_dstY_address

			lda temp_dstY_address
			clc
			adc #3
			sta temp_dstX_address

			dec snake_current_loop_size
			lda snake_current_loop_size
			asl
			asl
			sta temp_srcY_address

			lda temp_srcY_address
			clc
			adc #3
			sta temp_srcX_address

			ldx temp_srcY_address
			ldy temp_dstY_address
			lda oam, X
			sta oam, Y

			ldx temp_srcX_address
			ldy temp_dstX_address
			lda oam, X
			sta oam, Y

			jmp UPDATE_SNAKE_BODY

	UPDATE_HEAD:
		;update head
		lda oam ; get the current Y
		clc
		adc d_y ; add the Y velocity
		sta oam ; write the change

		lda oam + 3 ; get the current x
		clc
		adc d_x    ; add the X velocity
		sta oam + 3

	rts
.endproc

;*****************************************************************
; Our default palette table has 16 entries for tiles and 16 entries for sprites
;*****************************************************************

.segment "RODATA"
default_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$19,$29,$39 ; bg1 green
.byte $0F,$11,$21,$31 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$28,$21,$11 ; sp0 player
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine
