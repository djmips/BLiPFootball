;----------------------------------------------------------------------------
;
; Classic Football on Atari 2600
; Game created 2005/2006
;
; Derived from LEDhead
; Copyright 2001, Peter Hirschberg
; Author: Peter Hirschberg
;
; Game code by David Galloway, Bob Montgomery and Peter Hirschberg
;

;---------------------------------------------------------------------------
;
; Based on the handheld electronic games by Mattel Electronics.
; All trademarks copyrighted by their respective owners. This
; program is not affiliated or endorsed by Mattel Electronics.
;
;---------------------------------------------------------------------------
; License agreement:
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program (license.txt); if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;---------------------------------------------------------------------------
;
; The current version of this SOURCE CODE as well as the official
; versions of the BLiP Football APPLICATION are available from
; http://www.atariage.com/2600/archives/source/BLiPFootball_source/index.html
; and http://www.peterhirschberg.com
;

    processor 6502

;----------------------------------------------------------------------------
; Compile switches

NTSC                    = 0
PAL                     = 1
COMPILE_VERSION         = NTSC
NO_ILLEGAL_OPCODES      = 0

;----------------------------------------------------------------------------
;Kernel
;-------------------------Constants Below---------------------------------

BANK_0 = $fff8
BANK_1 = $fff9

;-------------------------End Constants-----------------------------------

;----------------------------------------------------------------------------
; Defines

TRUE                            = 1

NUM_DEFENSEPLAYERS              = 5
MAX_YARD                        = $00       ;100    ;converted to bcd

FOOTBALL_BLIP_ROWS              = 3
FOOTBALL_BLIP_COLUMNS           = 9

;FOOTBALL_SOUND_TICK             = 0
;FOOTBALL_SOUND_ENDPLAY          = 1
;FOOTBALL_SOUND_ENDPOSSESSION    = 2
;FOOTBALL_SOUND_SCORE            = 3

FSM_FORMATION                   = 0
FSM_INPLAY                      = 1
FSM_ENDPLAY                     = 2
FSM_PLAYENDED                   = 3
FSM_GAMEOVER                    = 4


; Display Flags
bDISPLAYSCORE = 1
bDISPLAYTIME =  2
bDISPLAYYARD =  4
bDISPLAYDOWN =  8
bDISPLAYBLIPS = 16

; Game Flags
bHOMETEAM =     1
bINFRAME =      2               ; FALSE
bPOWER =        4
bPRO2 =         8               ; FALSE
bGOTFIRSTDOWN = 16


SWITCHESMASK    = %00000011
FIREBUTTONMASK  = %00001000
JOYSTICKMASK    = %11110000
STICKPLUSBUTTON = %11111000
JOYRIGHTMASK    = %10000000
JOYLEFTMASK     = %01000000
JOYDOWNMASK     = %00100000
JOYUPMASK       = %00010000

;----------------------------------------------------------------------------
; Variables

                SEG.U vars_bank0
                ORG $0080

;---- Game
rand            ds 4

Temp  ds 1
temp1 ds 1 ;used by kernel.  can be used in program too, but
temp2 ds 1 ;are obliterated when drawscreen is called.
temp3 ds 1
temp4 ds 1
temp5 ds 1
temp6 ds 1

fsm             ds 1    ; game state fsm
displayFlag     ds 1    ; see Display Flags above
gameFlag        ds 1    ; see Game Flags above
noWait		ds 1    ;

flashingPlayer  ds 1    ; instead of a bit per player, just store the player number (-1 if none)

score        = .
scoreL      .byte
scoreM      .byte
scoreR      .byte
ptrScore    ds 12

;--note: the following 4 vars changed to bcd by bob
hScore_hundreds ds 1    ;added by bob to allow 3-digit scores
hScore          ds 1
vScore_hundreds ds 1    ;added by bob
vScore          ds 1

aiTick          ds 1
secondTick      ds 1
gameTime        ds 1
gameTenths      ds 1
;--need to convert firstDownYard, currentYardLine, and ballYard to bcd.  ...done, I think.
firstDownYard   ds 1    ;converted to bcd
down            ds 1
quarter         ds 1
currentYardLine ds 1    ;converted to bcd

ballCol         ds 1
ballLane        ds 1
ballYard        ds 1    ;converted to bcd

column          ds 5
lane            ds 5

joystick        ds 1
newjoy          ds 1

tempoCount      ds 1
tempoDelay      ds 1
soundPointer    ds 2
sfxPlay         ds 1

blipCol		ds 1
fieldCol	ds 1
endGameTimer	ds 1

selectFlag	ds 1

;nBright        ds NUM_DEFENSEPLAYERS

stack1 = $f6
stack2 = $f7
stack3 = $f8
stack4 = $f9
; the stack bytes above may be used in the kernel
; stack = F6-F7, F8-F9, FA-FB, FC-FD, FE-FF

;----------------------------------------------------------------------------
;

;----------------------------------------------------------------------------
; Code
                SEG     Bank0
                ORG  $1000
                RORG $D000

;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
start           subroutine
                jsr InitGame
	
;-------------------------------------------------------------------------
;--------------GAME MAIN LOOP---------------------------------------------
;-------------------------------------------------------------------------
MainGameLoop
    jsr FootballRun                     ; Takes around 16 scanlines
    jmp MainGameLoop

;-------------------------------------------------------------------------
;-------------------VBLANK Routine----------------------------------------
;-------------------------------------------------------------------------

	.align 256
	
;----------------------------------------------------------------------------

randomize      subroutine
                rts

bin_2_bcd       subroutine
		rts

;----------------------------------------------------------------------------


fsmFLow:    .byte <FSMformation, <FSMInPlay, <FSMEndPlay, <FSMPlayEnded, <FSMGameOver
fsmFHigh:   .byte >FSMformation, >FSMInPlay, >FSMEndPlay, >FSMPlayEnded, >FSMGameOver

;----------------------------------------------------------------------------

;// functions for dealing with the players

;TODO
;           lda BLIP_OFF
;           sta bright,x

; used in Football II
MoveBallUpField   subroutine
;                inc ballYard
            ;convert to bcd
            sed
            lda ballYard
            clc
            adc #$01
            sta ballYard
            cld
                ldy ballCol
                lda gameFlag
                and #bHOMETEAM
                eor #bHOMETEAM          ;  if (!bHomeTeam)
                jmp CheckBallPos


; ball carrying player can only move down field in Football I
MoveBallDownField subroutine
;                dec ballYard
            ;converted to bcd
            sed
            lda ballYard
            sec
            sbc #$01
            sta ballYard
            cld
                ldy ballCol
                lda gameFlag
                and #bHOMETEAM          ;  if (bHomeTeam)

CheckBallPos    beq .other               ; false, branch to away

                iny                     ; true
                cpy #FOOTBALL_BLIP_COLUMNS
                bcc .posok
                ldy #00
                beq .posok

.other          dey
                bpl .posok
                ldy #(FOOTBALL_BLIP_COLUMNS-1)

.posok          sty ballCol
                rts

; a: column
; y: lane
;
; returns: eq if ball or ne if not
;
IsBall:         subroutine
                cmp ballCol
                bne .fail
                cpy ballLane
.fail           rts

; a: column
; y: lane
; returns: eq if defense or ne if not
IsDefense:      subroutine
                sty temp5
                ldx #4
.checkd         cmp column,x
                bne .nod
                ldy lane,x
                cpy temp5
                beq .hit
.nod            dex
                bpl .checkd
                ldy temp5
                dex
.hit            rts
;----------------------------------------------------------------------------

InitGame:       subroutine

                lda #$00
                sta score
                sta score+1
                sta score+2
                sta displayFlag

;                ldx #23 ;47
;                lda #0
;.loop
;                sta playfield,x
;                dex
;                bpl .loop

                lda #bHOMETEAM      ; hometeam = true,  pro2 = false,  got first down = false
                sta gameFlag

                lda #<(~FIREBUTTONMASK)
                sta joystick

                lda #0
            sta hScore_hundreds ;-bob
            sta vScore_hundreds ;-bob
                sta hScore
                sta vScore
                sta quarter

InitGameHalfTime               
                lda #0
                sta down
;                lda #100-20
            lda #$80    ;--converted to bcd
                sta currentYardLine
                sta ballYard
;                lda #(100-20) - 10
            lda #$70    ;--converted to bcd
                sta firstDownYard

InitEndQuarter  
		lda gameFlag		; clear first down
		and #<(~bGOTFIRSTDOWN)
		sta gameFlag
		
		lda #$15            ;--note: converted to bcd
                sta gameTime            ; 150 seconds per quarter
                lda #$00            ;--note: converted to bcd
                sta gameTenths

                lda #60
                sta secondTick

InitEndPlay     lda #2
                sta aiTick

                lda #<-1
                sta flashingPlayer

                lda #FSM_FORMATION       ; set game state
                sta fsm

                rts

;----------------------------------------------------------------------------
FootballRun:    subroutine
	
                ; run the game
                ldx fsm
                lda fsmFLow,x
                sta temp1
                lda fsmFHigh,x
                sta temp2
                jmp (temp1)         ; will return to caller

;----------------------------------------------------------------------------

;// FINITE STATE MACHINE STUFF

InitialSetX     .byte 3,3,3,5,8
InitialSetY     .byte 0,1,2,1,1

;----------------------------------------------------------------------------
FSMformation:   subroutine
;
                ldx #bDISPLAYBLIPS
;                stx displayFlag

                ldy joystick
                tya
                and #JOYLEFTMASK
                bne .noleft
                txa
                ora #(bDISPLAYSCORE + bDISPLAYTIME)
                tax
.noleft
                tya
                and #JOYRIGHTMASK
                bne .noright
                txa
                ora #(bDISPLAYYARD + bDISPLAYDOWN)
                tax
.noright        stx displayFlag


                lda gameFlag
                and #bHOMETEAM
                beq .visitorp
                lda #0
                beq .storpos            ;branch always - bob
.visitorp       lda #8
.storpos        sta ballCol
                lda #1
                sta ballLane

                ; init defense positions
                ldx #(NUM_DEFENSEPLAYERS-1)
.setloop        ;stx temp1
                lda gameFlag
                and #bHOMETEAM
                bne .homedefp
                lda #8
                sec
                sbc InitialSetX,x
                bpl .stordefp
.homedefp       lda InitialSetX,x
.stordefp       sta column,x
                tay
                lda InitialSetY,x
                sta lane,x
;                tax
;                jsr SetCollision
;                ldx temp1
                dex
                bpl .setloop

; wait for kickoff or movement, then start play

; BUGBUG - check this
; if 4th down player can attempt fieldgoal/punt
; If player is viewing downs and presses fire button then can kick field goal / punt

                lda down
                cmp #3
                bne .notkickon4th
                lda joystick
                ; if viewing downs and press fire button
                and #(JOYRIGHTMASK + FIREBUTTONMASK)
                bne .notkickon4th

                ; kicking ball on 4th

		lda #1
		sta noWait	; make sure FSMPlayEnded doesn't wait

                lda #6
                jsr randomize
                and #63
                jsr bin_2_bcd

            	sed               
                cmp #$30	; if over 30 yards then don't add 5
                bcs .nochange

                clc
                adc #$5		; gimme from 5 yards or less
.nochange
                sta Temp

                lda ballYard
                sec
                sbc Temp
            	cld
            	           	
            	beq .FG
                bcs .punt

        ; scored a field goal
.FG
                lda #3
                jmp SCOREPOINTS

.punt           sta ballYard
	
                jmp TURNOVER

.notkickon4th
;Check input and if movement then transition to next state

                lda newjoy
                eor #(JOYDOWNMASK + JOYUPMASK + FIREBUTTONMASK)
                and #(JOYDOWNMASK + JOYUPMASK + FIREBUTTONMASK)
                beq .playnotstarted

                lda #FSM_INPLAY
                sta fsm

                jmp FSMInPlay           ; go execute next state immediately

.playnotstarted
                rts

;----------------------------------------------------------------------------
FSMInPlay:      subroutine
                ;bDisplayTime  = FALSE
                ;bDisplayScore = FALSE
                ;bDisplayYard  = FALSE
                ;bDisplayDown  = FALSE
                ;bDisplayBlips = TRUE

                lda #bDISPLAYBLIPS
                sta displayFlag

                lda newjoy
                and #FIREBUTTONMASK
                bne .donemoveplayer
                jsr MoveBallDownField

                lda ballCol
                ldy ballLane
                jsr IsDefense
                bne .donemoveplayer
                jsr MoveBallUpField         ; undo
                jmp .tackled
.donemoveplayer
                lda newjoy
                and #JOYUPMASK
                bne .attop
                ldy ballLane
                beq .attop
                dey
                lda ballCol
                jsr IsDefense
                bne .okup
                jmp .tackled
.okup
                sty ballLane
.attop
                lda newjoy
                and JOYDOWNMASK
                bne .atbot
                ldy ballLane
                cpy #2
                bcs .atbot
                iny
                lda ballCol
                jsr IsDefense
                bne .okdown
                jmp .tackled
.okdown         sty ballLane
.atbot
                lda newjoy
                and #JOYRIGHTMASK
                bne .noright
.noright
                lda newjoy
                and #JOYLEFTMASK
                bne .noleft
.noleft

; temp1 = yardsToGo = ballYard - firstDownYard

            ;converted to bcd
            sed
                lda ballYard
                sec
                sbc firstDownYard
                sta temp1           ; yardsToGo
            cld
                bne .noFirstDown

                lda gameFlag
                ora #bGOTFIRSTDOWN
                sta gameFlag
.noFirstDown
                lda ballYard
                bne .noTouchDown

; TOUCHDOWN!!
                lda #7
                
SCOREPOINTS     sta temp6
                clc
                lda gameFlag
                and #bHOMETEAM
                beq .visitorTeam

                lda hScore
    ;--note: changed to bcd.
            sed
            clc
            adc temp6
                sta hScore
            ;following added for 3-digit score:
            lda hScore_hundreds
            adc #0
            sta hScore_hundreds
            ;--added by bob.
            cld
                jmp .sscore     ;branch always - bob

.visitorTeam
                lda vScore
    ;--note: changed to bcd.
            sed
            clc
            adc temp6
                sta vScore
            ;following added for 3-digit score:
            lda vScore_hundreds
            adc #0
            sta vScore_hundreds
            ;--added by bob.

            cld
.sscore

                ;lda #<FOOTBALL_SOUND_SCORE
                ;sta soundPointer
                ;lda #>FOOTBALL_SOUND_SCORE
                ;sta soundPointer+1
                ;lda #4
                ;sta tempoDelay
                ;sta sfxPlay             ; starts the SFX
                ;lda #0
                ;sta tempoCount

;                lda #100-20
            lda #$80        ;converted to bcd
                sta currentYardLine
                sta ballYard
;                lda #(100-20) - 10
            lda #$70        ;converted to bcd
                sta firstDownYard

                lda gameFlag
                eor #bHOMETEAM
                and #<(~bGOTFIRSTDOWN) 	; clear firstdown flag
                sta gameFlag
                lda #0
                sta down


                lda #FSM_ENDPLAY
                sta fsm

                rts
.noTouchDown

                dec aiTick
                bne .nomovedef

                lda #2
                sta aiTick

.moveloop       lda #4
                jsr randomize
                and #7
                cmp #5
                bcs .nomovedef
                                ; 62% chance of making it here
                ; Picked a random defensive player (0-4) in A
                sta temp1
                tax
                ldy #40
                lda gameFlag
                and #bPRO2
                bne .pro1
                ldy #20
.pro1           sty temp2
                lda #8
                jsr randomize
                cmp temp2
                bcs .nomovedef

                lda column,x
                ldy lane,x
                tax

                lda #1
                jsr randomize
                and #$80
                bne .moveVertical

                cpx ballCol
                beq .nomovedef
                bcs .moveleft

                inx             ; test move right
                bpl .checkmove
.moveleft       dex             ; test move left
                bpl .checkmove  ;

.moveVertical   cpy ballLane
                beq .nomovedef
                bcs .movedown
                iny
                bpl .checkmove
.movedown       dey

.checkmove      ; check for tackle

                txa
                jsr IsBall
                beq .tackledby



                ; check for teammate
                jsr IsDefense
                beq .nomovedef          ; can't move

                ldx temp1
                sta column,x
                sty lane,x

.nomovedef      jmp .nottackled

; TODO should move to draw state BLIP_DIMBLINK
.tackledby      ldx temp1
.tackled        stx flashingPlayer
                lda gameFlag
                and #bGOTFIRSTDOWN
                bne .madefirstdown

                ldy down
                iny
                cpy #4
                bcc .downvalid

TURNOVER:
                ; turnover on downs
                ldy #0
                sty down
            ;converted to bcd
            sed
                lda #MAX_YARD
                sec
                sbc ballYard
                sta ballYard
;                sbc #10
            sec
            sbc #$10
                sta firstDownYard
            cld

                lda gameFlag
                eor #bHOMETEAM
                sta gameFlag

                ; play sound endpossesion


                ;lda #<FOOTBALL_SOUND_ENDPOSSESSION
                ;sta soundPointer
                ;lda #>FOOTBALL_SOUND_ENDPOSSESSION
                ;sta soundPointer+1
                ;lda #1
                ;sta tempoDelay
                ;sta sfxPlay             ; starts the SFX
                ;lda #0
                ;sta tempoCount

                lda #FSM_ENDPLAY
                sta fsm
                rts

.madefirstdown
            ;converted to bcd
            sed
                lda ballYard
                sec
;                sbc #10
            sbc #$10
            cld
                sta firstDownYard       ; yard to make next first down
                lda gameFlag
                and #<(~bGOTFIRSTDOWN)
                sta gameFlag            ; clear 'made first down' flag
                ldy #0

.downvalid      sty down

                ; play sound end of play
                ;lda #<FOOTBALL_SOUND_ENDPLAY
                ;sta soundPointer
                ;lda #>FOOTBALL_SOUND_ENDPLAY
                ;sta soundPointer+1
                ;lda #1
                ;sta tempoDelay
                ;sta sfxPlay             ; starts the SFX
                lda #0
                sta tempoCount

                lda #FSM_ENDPLAY
                sta fsm
                rts

.nottackled
                dec secondTick
                bne .notick

                lda #60                 ; BUGBUG NTSC assumption
                sta secondTick

                lda gameTime
                ora gameTenths
            	beq .notick         	; end of quarter

            ;--note: converted to use bcd
            	sed
            	sec
            	lda gameTenths
            	sbc #1
                bcs .tcounting
            ;--note: converted to bcd
            	lda gameTime
            	sbc #0
            	sta gameTime

                lda #$09
.tcounting      sta gameTenths
            	cld

                lda sfxPlay             ; lo priority
                bne .notick

                ;lda #<FOOTBALL_SOUND_TICK
                ;sta soundPointer
                ;lda #>FOOTBALL_SOUND_TICK
                ;sta soundPointer+1

                lda #1
                sta tempoDelay
                sta sfxPlay             ; starts the SFX
                lda #0
                sta tempoCount

.notick         rts

;----------------------------------------------------------------------------
FSMEndPlay	
                lda gameTime
                ora gameTenths
                bne .notendq
                
                inc quarter
.notendq                
		lda #FSM_PLAYENDED
		sta fsm
		rts

;----------------------------------------------------------------------------
FSMPlayEnded:
                lda #bDISPLAYBLIPS
                sta displayFlag

                lda gameTime
                ora gameTenths
                bne .notendq1

                lda quarter
                cmp #4
                bne .notEndGame

		lda #120
		sta endGameTimer
		
                ; game over
                lda #FSM_GAMEOVER
                sta fsm
                rts

.notEndGame
                ; display '00' if clock ran out
                lda #(bDISPLAYTIME)
                sta displayFlag
.notendq1:
                ; press left/right to continue  ( SCORE/STATUS ) TODO
		lda noWait
		bne .nowaitOnPlayEnded 	; used by kicking
		
                lda newjoy
                eor #(JOYLEFTMASK + JOYRIGHTMASK)
                and #(JOYLEFTMASK + JOYRIGHTMASK)
                beq .nofire

.nowaitOnPlayEnded:
		lda #0
		sta noWait
		
                lda gameTime
                ora gameTenths
                bne .notendq2

                ; end of quarter
                lda quarter
                cmp #2
                bne .nothalftime

                ; halftime - force kickoff
                ; visitor team gets possession
                lda gameFlag
                and #<(~bHOMETEAM)
                sta gameFlag
                jmp InitGameHalfTime

.nothalftime
                jmp InitEndQuarter
.notendq2
                lda ballYard
                sta currentYardLine
                jmp InitEndPlay
.nofire
                rts

;----------------------------------------------------------------------------
FSMGameOver:    subroutine

                lda #(bDISPLAYBLIPS+bDISPLAYSCORE)
                sta displayFlag
                lda endGameTimer
                beq .checkStick
                dec endGameTimer
                rts
                
.checkStick     lda newjoy
		and #FIREBUTTONMASK
		bne .nostick
                
                jmp InitGame
.nostick
                rts


;*************************************************************************

;-------------------------------------------------------------------------
;-------------------------Data Below--------------------------------------
;-------------------------------------------------------------------------

;---------------------------------------------------------
;---------------------------------------------------------
   ORG $2FF0
   RORG $FFF0

GoGame:
    bit BANK_0
    jmp start

   ORG $2FFA
   RORG $FFFA


   .word start          ; NMI
   .word start          ; RESET
   .word start          ; IRQ
