; Programming Games for NES - Megablast

;*****************************************************************
; Define NES cartridge Header
;*****************************************************************

.segment "HEADER"
INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 1 ; 1 = battery backed SRAM at $6000-7FFF

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
.incbin "snake.chr"

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
highscore:				 .res 3
update: 				 .res 1

temp: .res 10

snake_size:				 .res 4 ; score
actual_snake_size:		 .res 4 ; snake size including the head
snake_current_loop_size: .res 4 

temp_srcX_address: .res 4
temp_dstX_address: .res 4

temp_srcY_address: .res 4
temp_dstY_address: .res 4

temp_pickupX: .res 4
temp_pickupY: .res 4

temp_dstPattern_address: .res 4

x_border: .res 4
y_border: .res 4

game_over:  .res 1 ; used as a boolean
game_start: .res 1 ; used as a boolean
just_picked_up: .res 1 ; just picked up food

testing: .res 1

lo_bit: .res 1
hi_bit: .res 1

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
segment_current_tile: .res 600 ; 2 bytes per pos: first byte for row and the second byte is for the column.

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
	lda #%00000010 ; has the high score updated?
	bit update
	beq @skiphighscore
		jsr display_highscore ; display high score
		lda #%11111101 ; reset high score update flag
		and update
		sta update
@skiphighscore:

	lda #%00001000 ; does the game over message need to be displayed?
	bit update
	beq @skipgameover
		vram_set_address (NAME_TABLE_0_ADDRESS + 28 * 32 + 7)
		assign_16i text_address, game_over_text 
		jsr write_text

		vram_set_address (NAME_TABLE_0_ADDRESS + 29 * 32 + 7)
		assign_16i text_address, restart_text 
		jsr write_text

		lda highscore
		sta BATTERY_RAM

		lda highscore + 1
		sta BATTERY_RAM + 1

		lda highscore + 1
		sta BATTERY_RAM + 1

		lda #%11110111 ; reset game over message update flag
		and update
		sta update
@skipgameover:

; ;jsr display_snake_segment
; 	lda #%00000100 ; has the score updated?
; 	bit update
; 	beq @skipsegment
; 		jsr delete_tail_draw_segments
; 		jsr update_body_test
; 		jsr display_snake_segment ; display score
; 		lda #%11111011 ; reset score update flag
; 		and update
; 		sta update
; @skipsegment:
	lda game_over
	cmp #1
	beq END_UPDATE_TIMER

	lda game_start
	cmp #0
	beq END_UPDATE_TIMER

	lda frame_timer
	cmp #1
	bne END_UPDATE_TIMER
		jsr delete_tail_draw_segments
		jsr display_snake_segment

END_UPDATE_TIMER:

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
	and #PAD_START|PAD_SELECT
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

	lda BATTERY_RAM
	sta highscore

	lda BATTERY_RAM + 1
	sta highscore + 1

	lda BATTERY_RAM + 1
	sta highscore + 1

	lda #32 ; define borders
	sta x_border
	lda #32
	sta y_border

	; place head segment
 	lda #127
 	sta oam ; set Y
 	lda #128
 	sta oam + 3 ; set X
 	lda #3
 	sta oam + 1 ; set pattern
 	lda #2
 	sta oam + 2 ; set attributes

	; draw the game screen
	jsr display_game_screen

	jsr display_highscore

	; place food
	jsr rand
	and #%00000111
	asl
	asl
	asl
	clc
	adc #95
	sta oam + (63 * 4)

	lda #1
	sta oam + (63 * 4) + 1 ; set pattern
 	lda #1
 	sta oam + (63 * 4) + 2 ; set attributes

	jsr rand
	and #%00000111
	asl
	asl
	asl
	clc
	adc #96 
	sta oam + (63 * 4) + 3

	lda #3
	sta snake_size
	
	lda #3
	sta actual_snake_size

	jsr ppu_update

mainloop:
	lda time
	; ensure the time has actually changed
	cmp lasttime
	beq mainloop
	; time has changed update the lasttime value
	sta lasttime

	jsr player_actions

	lda game_start
	cmp #0
	beq END_UPDATE_TIMER

	lda game_over
	cmp #1
	beq END_UPDATE_TIMER

	lda frame_timer
	cmp #10
	bne END_UPDATE_TIMER
		; reset frame timer
		lda #0
		sta frame_timer
		; update snake and check if head it's a segment after update 

		; lda just_picked_up
		; cmp #1
		; beq NOT_DELETE
		; 	jsr delete_tail_draw_segments
		; NOT_DELETE:

		; lda #0
		; sta just_picked_up
		; ;jsr update_body
		jsr check_head_hit
		jsr update_body_test
		
		; jsr display_snake_segment
		; ;jsr update_body_test

		; lda #%00000100 ; set flag to write high score to the screen
		; ora update
		; sta update 

		; check if the head is on the pickup so it can place it somewhere else on a random place
		lda oam
		cmp oam + (63 * 4)
		bne END_UPDATE_TIMER
			lda oam + 3
			cmp oam + (63 * 4) + 3
			bne END_UPDATE_TIMER
			jsr place_pickup

	END_UPDATE_TIMER:
	
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
		lda oam
		clc
		adc #1
		lsr
		lsr
		lsr
		cmp segment_current_tile
		beq NOT_GAMEPAD_LEFT

		lda d_x
		cmp #0
		bne NOT_GAMEPAD_LEFT

		lda #0
		sta d_y
		lda #$F8
		sta d_x

		lda game_start
		cmp #1
		beq NOT_GAMEPAD_LEFT
			lda #10
			sta frame_timer
			lda #1
			sta game_start
		jmp NOT_GAMEPAD_DOWN
NOT_GAMEPAD_LEFT:

 	lda gamepad
 	and #PAD_R
 	beq NOT_GAMEPAD_RIGHT
		lda oam
		clc
		adc #1
		lsr
		lsr
		lsr
		cmp segment_current_tile
		beq NOT_GAMEPAD_RIGHT

		lda d_x
		cmp #0
		bne NOT_GAMEPAD_RIGHT

		lda #$0
		sta d_y
 		lda #$08
		sta d_x

		lda game_start
		cmp #1
		beq NOT_GAMEPAD_RIGHT
			lda #10
			sta frame_timer
			lda #1
			sta game_start
		jmp NOT_GAMEPAD_DOWN
NOT_GAMEPAD_RIGHT:
lda gamepad
     and #PAD_U
     beq NOT_GAMEPAD_UP
	 	lda oam + 3
		lsr
		lsr
		lsr
		cmp segment_current_tile + 1
		beq NOT_GAMEPAD_UP

	 	lda d_y
		cmp #0
		bne NOT_GAMEPAD_UP

		lda #$0
		sta d_x
		lda #$F8
		sta d_y

		lda game_start
		cmp #1
		beq NOT_GAMEPAD_UP
			lda #10
			sta frame_timer
			lda #1
			sta game_start

		jmp NOT_GAMEPAD_DOWN
 NOT_GAMEPAD_UP:

 lda gamepad
     and #PAD_D
     beq NOT_GAMEPAD_DOWN
	 	lda oam + 3
		lsr
		lsr
		lsr
		cmp segment_current_tile + 1
		beq NOT_GAMEPAD_DOWN

	 	lda d_y
		cmp #0
		bne NOT_GAMEPAD_DOWN

		lda #0
		sta d_x
        lda #$08
		sta d_y

		lda game_start
		cmp #1
		beq NOT_GAMEPAD_DOWN
			lda #10
			sta frame_timer
			lda #1
			sta game_start
 NOT_GAMEPAD_DOWN:

	 lda gamepad
     and #PAD_A
     beq NOT_GAMEPAD_A
	 	lda game_over
		cmp #1
		bne NOT_GAMEPAD_A
			jsr reset
NOT_GAMEPAD_A:

	rts
.endproc

.segment "CODE"

title_text:
.byte "SNAKE",0

press_play_text:
.byte "PRESS SELECT OR START TO BEGIN",0

by_text:
.byte "BY",0

team_possible_text:
.byte "TEAM POSSIBLE",0

title_attributes:
.byte %00000101,%00000101,%00000101,%00000101
.byte %00000101,%00000101,%00000101,%00000101

.proc display_title_screen
	jsr ppu_off ; Wait for the screen to be drawn and then turn off drawing

	jsr clear_nametable ; Clear the 1st name table

	
	; Write our title text
	vram_set_address (NAME_TABLE_0_ADDRESS + 10 * 32 + 13)
	assign_16i text_address, title_text
	jsr write_text

	; Write our name text
	vram_set_address (NAME_TABLE_0_ADDRESS + 13 * 32 + 9)
	assign_16i text_address, team_possible_text
	jsr write_text

	; Write our press play text
	vram_set_address (NAME_TABLE_0_ADDRESS + 20 * 32 + 1)
	assign_16i text_address, press_play_text
	jsr write_text

	; ; Write our by text
	; vram_set_address (NAME_TABLE_0_ADDRESS + 18 * 32 + 15)
	; assign_16i text_address, by_text
	; jsr write_text

	jsr ppu_update ; Wait until the screen has been drawn

	rts
.endproc

.segment "CODE"
game_over_text:
.byte "GAME OVER",0

restart_text:
.byte "PRESS A TO RESTART",0
.proc display_losing_screen
	jsr ppu_off ; Wait for the screen to be drawn and then turn off drawing

	jsr clear_nametable ; Clear the 1st name table

	
	; Write our title text
	vram_set_address (NAME_TABLE_0_ADDRESS + 28 * 32 + 6)
	assign_16i text_address, game_over_text
	jsr write_text

	; Set the title text to use the 2nd palette entries
	vram_set_address (ATTRIBUTE_TABLE_0_ADDRESS + 32)
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
.byte "SCORE 0000000", 0

game_screen_high_score:
.byte "HIGHSCORE 0000000", 0

score_attributes:
.byte %00000101,%00000101,%00000101,%00000101
.byte %00000101,%00000101,%00000101,%00000101

.segment "ZEROPAGE"

paddr: .res 2 ; 16-bit address pointer
paddr2: .res 2 ; 16-bit address pointer

.segment "CODE"
.proc display_game_screen
	jsr ppu_off ; Wait for the screen to be drawn and then turn off drawing

	jsr clear_nametable ; Clear the 1st name table

	; output the score section on the next line
	; Write our title text
	vram_set_address (NAME_TABLE_0_ADDRESS + 0 * 32 + 1)
	assign_16i text_address, game_screen_scoreline
	jsr write_text

	; output the score section on the next line
	vram_set_address (NAME_TABLE_0_ADDRESS + 1 * 32 + 14)
	assign_16i text_address, game_screen_high_score
	jsr write_text

	; Draw the playing field
	vram_set_address (NAME_TABLE_0_ADDRESS + 2 * 32 + 3)
	lda #$05
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 2 * 32 + 4)
	ldx #0
	lda #$04
	@loop:
	sta PPU_VRAM_IO
	inx
	cpx #24
	bne @loop

	vram_set_address (NAME_TABLE_0_ADDRESS + 2 * 32 + 28)
	lda #$06
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 3 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 3 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 4 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 4 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 5 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 5 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 6 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 6 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 7 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 7 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 8 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 8 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 9 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 9 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 10 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 10 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 11 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 11 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 12 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 12 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 13 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 13 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 14 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 14 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 15 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 15 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 16 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 16 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 17 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 17 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 18 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 18 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 19 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 19 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 20 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 20 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 21 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 21 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 22 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 22 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 23 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 23 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 24 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 24 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 25 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 25 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 3)
	lda #$01
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 28)
	lda #$02
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 3)
	lda #$08
	sta PPU_VRAM_IO

	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 4)
	ldx #0
	lda #$03
	@loop2:
	sta PPU_VRAM_IO
	inx
	cpx #24
	bne @loop2

	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 28)
	lda #$07
	sta PPU_VRAM_IO

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

	lda highscore+2
	cmp score+2
	bcc @highscore
	bne @nothighscore

	lda highscore+1
	cmp score+1
	bcc @highscore
	bne @nothighscore

	lda highscore
	cmp score
	bcs @nothighscore

@highscore:
	lda score
	sta highscore
	lda score+1
	sta highscore+1
	lda score+2
	sta highscore+2

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
	vram_set_address ($2000 + 0 * 32 + 7)

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

;*****************************************************************
; display_highscore: Write the high score to the screen
;*****************************************************************
.segment "CODE"

.proc display_highscore
	vram_set_address (NAME_TABLE_0_ADDRESS + 1 * 32 + 24)

	lda highscore+2 ; transform each decimal digit of the high score
	jsr dec99_to_bytes
	stx temp
	sta temp+1

	lda highscore+1
	jsr dec99_to_bytes
	stx temp+2
	sta temp+3

	lda highscore
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
.proc place_pickup
		inc snake_size ; Increment snake size
		inc actual_snake_size

		lda #1
		sta just_picked_up

		;jsr update_body	
	PLACE_PICKUP_LOOP:
	againY:
    	jsr rand
    	and #%00011111 ; Limit range to 0-7
		cmp #23
		bpl againY
    	asl
    	asl
    	asl ; *8
    	clc
    	adc #24
    	sta temp_pickupY

	againX:
    	jsr rand
    	and #%00011111 ; Limit range to 0-7
		cmp #23
		bpl againX
    	asl
    	asl
    	asl ; * 8 
		clc
		adc #32 
    	sta temp_pickupX


		lda oam
    	cmp temp_pickupY
    	bne CHECK_PICKUP_COLLISION

    	lda oam + 3
    	cmp temp_pickupX
    	bne CHECK_PICKUP_COLLISION

    	; check if the temp placement collides with snake
    	ldx #0
		stx snake_current_loop_size
	CHECK_PICKUP_COLLISION:
		lda snake_current_loop_size
    	cmp snake_size
    	beq PLACE_PICKUP_DONE

		lda snake_current_loop_size
		asl
		tax

    	lda segment_current_tile, X
		asl
		asl
		asl
    	cmp temp_pickupY
    	bne NEXT_SEGMENT

    	lda segment_current_tile + 1, X
		asl
		asl
		asl
    	cmp temp_pickupX
    	bne NEXT_SEGMENT

    	jmp PLACE_PICKUP_LOOP

	NEXT_SEGMENT:
    	inc snake_current_loop_size
    	jmp CHECK_PICKUP_COLLISION  

	; the check is done so now we can place it
	PLACE_PICKUP_DONE:
    	lda temp_pickupY
		sec
		sbc #1
    	sta oam + (63 * 4) ; Store Y position in OAM

    	lda temp_pickupX
    	sta oam + (63 * 4) + 3 ; Store X position in OAM

    	lda #1                  
    	jsr add_score

    	rts
.endproc

.segment "CODE"
.proc update_body
	lda snake_size
	sta snake_current_loop_size

	UPDATE_SNAKE_BODY:
		lda snake_current_loop_size ; load loping and compare if it is 0 jump to update head
		cmp #0
		beq UPDATE_HEAD
			; idx * 4 = result 1
			lda snake_current_loop_size 
			asl
			asl
			sta temp_dstY_address 

			lda temp_dstY_address
			clc
			adc #1
			sta temp_dstPattern_address

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

		lda d_y
		cmp #0
		beq X_RTOTATION
		clc

		lda #2
		sta oam + 1

		lda #%00000010
		sta oam + 2

		lda d_y
		cmp #$08
		bne X_RTOTATION

		lda #%10000010
		sta oam + 2

		jmp END

		X_RTOTATION:
		lda d_x
		cmp #0
		beq END
		clc

		lda #3
		sta oam + 1

		lda #%00000010
		sta oam + 2

		lda d_x
		cmp #$F8
		bne END

		lda #%01000010
		sta oam + 2

	END:

	rts
.endproc

.segment "CODE"
.proc check_head_hit
lda oam
clc
adc #1
cmp #216
beq GAME_OVER
cmp #16
beq GAME_OVER

lda oam + 3
cmp #24
beq GAME_OVER
cmp #224
beq GAME_OVER

lda snake_size
sta snake_current_loop_size

dec snake_current_loop_size

	CHECK_SEGMENT_HIT:
		lda snake_current_loop_size
		cmp #2
		beq LOOP_FINISHED
			lda snake_current_loop_size
			asl
			sta temp_dstY_address

			lda temp_dstY_address
			clc
			adc #1
			sta temp_dstX_address

			dec snake_current_loop_size

			lda oam
			clc 
			adc #1
			lsr
			lsr
			lsr
			sta temp_srcY_address


			; compare y coordinate
			ldy temp_dstY_address
			lda segment_current_tile, Y
			cmp temp_srcY_address
			bne CHECK_SEGMENT_HIT

			lda oam + 3
			lsr
			lsr
			lsr
			sta temp_srcX_address

			; compare x coordinate
			ldx temp_dstX_address
			lda segment_current_tile, X
			cmp temp_srcX_address
			bne CHECK_SEGMENT_HIT

			GAME_OVER:
			; set game_over to 1 so that the snake does not get updated anymore
			lda #%00001000
			ora update
			sta update
			lda #1
			sta game_over

			LOOP_FINISHED:
			rts
.endproc

.segment "CODE"
.proc update_body_test
	lda snake_size
	sta snake_current_loop_size
	dec snake_current_loop_size
	UPDATE_SNAKE_BODY:
		; lda snake_current_loop_size
		; cmp #0
		; beq UPDATE_HEAD
		clc
		lda snake_current_loop_size
		cmp #0
		beq UPDATE_HEAD
			
			lda snake_current_loop_size
			asl
			sta temp_dstY_address

			lda temp_dstY_address
			clc
			adc #1
			sta temp_dstX_address

			dec snake_current_loop_size
			lda snake_current_loop_size
			asl
			sta temp_srcY_address

			lda temp_srcY_address
			clc
			adc #1
			sta temp_srcX_address

			ldx temp_srcY_address
			ldy temp_dstY_address
			lda segment_current_tile, X
			sta segment_current_tile, Y

			ldx temp_srcX_address
			ldy temp_dstX_address
			lda segment_current_tile, X
			sta segment_current_tile, Y
			

			jmp UPDATE_SNAKE_BODY

	UPDATE_HEAD:
		; lda snake_current_loop_size
		; cmp #0
		; beq @notlast
		; 	lda segment_current_tile
		; 	sta segment_current_tile + 2

		; 	lda segment_current_tile
		; 	sta segment_current_tile + 3

	@notlast:
	; updating segment right before head
		lda oam
		clc
		adc #1
		lsr 
		lsr
		lsr
		sta segment_current_tile

		lda oam + 3
		lsr 
		lsr
		lsr
		sta segment_current_tile + 1


		;update head
		lda oam ; get the current Y
		clc
		adc d_y ; add the Y velocity
		sta oam ; write the change

		lda oam + 3 ; get the current x
		clc
		adc d_x    ; add the X velocity
		sta oam + 3

		lda d_y
		cmp #0
		beq X_RTOTATION
		clc

		lda #2
		sta oam + 1

		lda #%00000010
		sta oam + 2

		lda d_y
		cmp #$08
		bne X_RTOTATION

		lda #%10000010
		sta oam + 2

		jmp END


		X_RTOTATION:
		lda d_x
		cmp #0
		beq END
		clc

		lda #3
		sta oam + 1

		lda #%00000010
		sta oam + 2

		lda d_x
		cmp #$F8
		bne END

		lda #%01000010
		sta oam + 2

	END:

		; lda #%00000100 ; set flag to write high score to the screen
		; ora update
		; sta update
	rts
.endproc

.segment "CODE"
.proc display_snake_segment
	;jsr ppu_off

		; a 
		lda #$20
		sta hi_bit

		lda #$00
		sta lo_bit

		; update y pos
		lda segment_current_tile
		lsr
		lsr
		lsr
		sta temp_dstY_address

		lda hi_bit
		clc
		adc temp_dstY_address
		sta hi_bit

		lda segment_current_tile
		asl
		asl
		asl
		asl
		asl
		sta lo_bit

		lda segment_current_tile + 1
		sta temp_dstX_address

	; 	clc
	; 	ldx lo_bit
	; loop:
	; 	lda temp_dstX_address
	; 	cmp #0
	; 	beq endloop
	; 		clc
	; 		inx
	; 		lda hi_bit
	; 		adc #0
	; 		sta hi_bit
	; 		clc
	; 		dec temp_dstX_address
	; 		jmp loop

	; endloop:
	; 	stx lo_bit

		lda lo_bit
		clc
		adc temp_dstX_address
		sta lo_bit

		; draw a base line
		lda PPU_STATUS
		lda hi_bit
		sta PPU_VRAM_ADDRESS2
		lda lo_bit
		sta PPU_VRAM_ADDRESS2

		lda #$60
		sta PPU_VRAM_IO

	;jsr ppu_update
	;vram_clear_address
	rts

.endproc

.segment "CODE"
.proc clear_sprites
;
lda #255
ldx #0
clear_oam:
sta oam,x
inx
inx
inx
inx
bne clear_oam
rts
.endproc

.segment "CODE"
.proc delete_tail_draw_segments
	;jsr ppu_off
	lda snake_size
	sta snake_current_loop_size
	
		dec snake_current_loop_size
		lda snake_current_loop_size
		asl
		sta temp_dstY_address

		lda temp_dstY_address
		clc
		adc #1
		sta temp_dstX_address

		; a 
		lda #$20
		sta hi_bit

		lda #$00
		sta lo_bit

		; update y pos
		ldy temp_dstY_address
		lda segment_current_tile, Y
		lsr
		lsr
		lsr
		sta temp_srcY_address

		lda hi_bit
		clc
		adc temp_srcY_address
		sta hi_bit

		ldy temp_dstY_address
		lda segment_current_tile, Y
		asl
		asl
		asl
		asl
		asl
		sta lo_bit

		ldx temp_dstX_address
		lda segment_current_tile, X
		sta temp_dstX_address

	; 	ldx lo_bit
	; 	clc
	; loop2:
	; 	lda temp_dstX_address
	; 	cmp #0
	; 	beq endloop2
	; 		clc
	; 		inx
	; 		lda hi_bit
	; 		adc #0
	; 		sta hi_bit
	; 		clc
	; 		dec temp_dstX_address
	; 		jmp loop2

	; endloop2:
	; 	stx lo_bit

	lda lo_bit
	clc
	adc temp_dstX_address
	sta lo_bit

		; draw a base line
		lda PPU_STATUS
		lda hi_bit
		sta PPU_VRAM_ADDRESS2
		lda lo_bit
		sta PPU_VRAM_ADDRESS2

		lda #$0
		sta PPU_VRAM_IO

	;jsr ppu_update
	;vram_clear_address
rts
.endproc


;*****************************************************************
; Our default palette table has 16 entries for tiles and 16 entries for sprites
;*****************************************************************

.segment "RODATA"
default_palette:
.byte $0F,$19,$39,$29 ; bg0 green
.byte $0F,$19,$29,$39 ; bg1 green
.byte $0F,$11,$21,$31 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$09,$19,$29 ; body
.byte $0F,$26,$05,$09 ; apple
.byte $0F,$08,$25,$29 ; head
.byte $0F,$12,$22,$32 ; sp3 marine