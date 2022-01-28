TITLE snake

; snake.asm
; ecnolan@hamilton.edu
; Eva Nolan
; Spring 2021

INCLUDE cs240.inc

.8086

TERMINATE = 4C00h
DOS = 21h

.code

;; ========================================================================================
;; stuff for sound
SpeakerMuted	BYTE	0

Mute PROC
	mov	cs:SpeakerMuted, 1
	ret
Mute ENDP

UnMute PROC
	mov	cs:SpeakerMuted, 0
	ret
UnMute ENDP

stall PROC
	push cx
	mov cx, 100
	top:
		call delay
		loop top

	pop cx
	ret
stall ENDP

stall2 PROC
	push cx
	mov cx, 30
	top:
		call delay
		loop top

	pop cx
	ret
stall2 ENDP

delay PROC
  push cx
  mov cx, 900
  top:
    push cx
    pop cx
    loop top

  pop cx
  ret
delay ENDP

PlayCount PROC
  ; dx contains a count (ratio of freqency of clock to desired freqency)
  push ax

  ; mov dx, 012 ; get rid of this later, this is mimicing mario
  mov al, 0B6h
  out 43h, al
  mov al, dl
  out 42h, al
  mov al, dh
  out 42h, al

  pop ax
  ret
Playcount ENDP

SpeakerOn PROC ; from class
  push ax
  in al, 61h
  or al, 03h
  out 61h, al

  pop ax
  ret
SpeakerOn ENDP

SpeakerOff PROC

  push ax

  in al, 61h
  and al, 0FCh
  out 61h, al

  pop ax
  ret
SpeakerOff ENDP

PlayFrequency PROC
	;; Frequency is found in DX

	pushf
	push	ax

	call	FreqToCount 		; now dx = count
	call	PlayCount				; plays note

	pop		ax
	popf
	ret
PlayFrequency ENDP

; song BYTE "cdefgab"
song BYTE "cecececeegegegegcecececeegegegeg", 0
lowsound BYTE "c e ", 0
highsound BYTE "e g ", 0
foodsound BYTE "ga", 0
gameoversound BYTE "gfedccc", 0
funregsound BYTE "cdefggggffffecc ", 0 ; if space, rest

; freqlist WORD 262, 294, 329, 349, 392, 440, 494, 523
freqlist WORD 440, 494, 262, 294, 329, 349, 392 ; abcdefg

NoteToFreq PROC
	push	bx
	push 	ax
	push	si
	push 	cx
	pushf
	; note passed in dx

	mov		bx,		OFFSET freqlist
	sub		dx,	 	61h ; now dx = index
	mov		ax,	 	dx
	mov 	cx,		2
	mul 	cx
	mov 	si, 	ax ; for indexing freqlist (*double b/x WORD)
	mov 	dl, 	[bx + si] ; move note's frequency into dx
	mov 	dh, 	[bx + si + 1]

	popf
	pop 	cx
	pop 	si
	pop 	ax
	pop 	bx
	ret
NoteToFreq ENDP

; get song string offset in bx
playsong PROC
	push bx
	push dx
	push si

	mov si, 0

	top:
	mov 		dx, [bx+si] ; note letter in dx
	mov			dh, 0 ; clear top of dx
	cmp dx, 0h
	je done
	call 		stall
	call		NoteToFreq ; now dx = freqency ; c should have freq 262
	call		SpeakerOn
	call	 	PlayFrequency
	call 		stall
	call 		SpeakerOff
	inc 		si

	; cmp 		si, 7
	; je 			done
	jmp 		top

	done:
	pop si
	pop dx
	pop bx
	ret
playsong ENDP

freqtocount PROC
	;; dx contains freqency
	;; changes to to count
	push ax
	push cx
	pushf

	mov 	ax, 4DAEh ; dividend = clockfreq / 20
	mov 	cx, dx
	mov 	dx, 0
	div  	cx       ; divide ax by dx (al = result, ah = remainder)
	mov 	ah, 0 ; clear remainder
	mov		cx, 60
	mul 	cx
	mov 	dx, ax ; put count into dx

	popf
	pop cx
	pop ax
	ret
FreqToCount ENDP

PlayNote PROC
	; dl = note letter ascii char
	call		NoteToFreq ; now dx = freqency ; c should have freq 262
 	call		SpeakerOn
	call	 	PlayFrequency
	call 		stall
	call 		SpeakerOff
	ret
PlayNote ENDP

PlayRegSound PROC
	push bx
	push dx
	push si

	cmp songticks, 3
	jne InRange
	mov songticks, 0

	InRange:
	mov bx, OFFSET lowsound
	cmp songticks, 0
	je zero
	cmp songticks, 1
	je done
	cmp songticks, 2
	jmp two
	cmp songticks, 3
	je done

	zero:
		mov	dx, 'c' ; note letter in dx
		jmp play

	two:
		mov	dx, 'd' ; note letter in dx
		jmp play

	; one: ; one plays no sound
	;
	; three: ; three plays no sound, resets count


	mov si, 0
	play:
	cmp keypresses, 0
	je play2
	; otheriwse add 2 to note before playing
	add dx, 2
	play2:
	call 		PlayNote

	done:
	inc songticks
	pop si
	pop ds
	pop bx
	ret
PlayRegSound ENDP

PlayGameoverSound PROC
	push bx
	push dx
	push si

	mov bx, OFFSET gameoversound
	mov si, 0
	call stall
	call stall
	top:
	mov 		dx, [bx+si] ; note letter in dx
	mov			dh, 0 ; clear top of dx
	cmp dx, 0h
	je done
	call 		stall
	call		NoteToFreq ; now dx = freqency ; c should have freq 262
	call		SpeakerOn
	call	 	PlayFrequency
	call 		stall
	call 		SpeakerOff
	inc 		si

	jmp 		top

	done:
	pop si
	pop ds
	pop bx
	ret
PlayGameoverSound ENDP


PlayFoodSound PROC
	push bx
	push dx
	push si

	inc songticks
	cmp songticks, 3
	je next
	mov songticks, 0
	next:

	mov bx, OFFSET foodsound
	mov si, 0
	call stall
	top:
	mov 		dx, [bx+si] ; note letter in dx
	mov			dh, 0 ; clear top of dx
	cmp dx, 0h
	je done
	call 		stall2
	call		NoteToFreq ; now dx = freqency ; c should have freq 262
	call		SpeakerOn
	call	 	PlayFrequency
	call 		stall2
	call 		SpeakerOff
	inc 		si

	jmp 		top

	done:
	pop si
	pop ds
	pop bx
	ret
PlayFoodSound ENDP

;; ======================================================================================


SnakeAry BYTE 4000 DUP(?)
MAXARY = 4000

;; register passed in ax, prints contents of register
PrintReg PROC
	push ax
	push dx
	pushf

	mov dl,  ah ; look at first two digits first
	shr dl, 1
	shr dl, 1
	shr dl, 1
	shr dl, 1  ; shift to print first digit
	call PrintHexDigit

	mov dl, ah ; first two digits: print second digit
	call PrintHexDigit

	mov dl,  al ; look at lower two digits (digit 3 and 4)
	shr dl, 1
	shr dl, 1
	shr dl, 1
	shr dl, 1 ; shift to print 3rd digit
	call PrintHexDigit

	mov dl, al ; print digit 4
	call PrintHexDigit

	popf
	pop dx
	pop ax
	ret
PrintReg ENDP

;; preserves all registers and flags
;; string OFFSET parameter is in dx (same as PrintString)
PrintString PROC
	pushf
	push ax
	push bx
	push cx
	push dx
	push si

	mov bx, dx ; move string's offset into bx for memory []
	mov si, 0 ; si starts at 0, will increment in [] in loop

	top:
		;; each iteration prints one letter of string
		;; exits loop when it reaches the end 00h character
		mov dh, 0
		mov 	dl, [bx+si] ; Put the ASCII value of the character you want written into DL
		cmp dl, 0 ;; if ASCII hex is equal to 00h...
		jz finished ;; reached end of string, exit loop

		mov 	ah, 02h ; Set AH to the DOS code for write-character.
		int 	21h ; DOS!
		inc 	si ; will go to next character

	 	jmp top

	finished:

	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf

	ret
PrintString ENDP

hexary BYTE "0123456789abcdef"

;; prints lowest hex digit in dx (lowest 4 bits)
PrintHexDigit PROC
	;; num stored in dl
	pushf
	push ax
	push bx
	push cx
	push dx
	push si
	;;using hexary BYTE 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66, 67, 68, 69, 70

	AND 	dx, 1111b ; bitmask all bits except for last hex digit
	mov 	si, dx ; put last hex digit into si to index hexary
	mov		bx, OFFSET hexary ; bx locates the hexary with values corresponding to wanted ascii output

	mov 	dl, [bx + si] ; move ascii code of hexdigit into dl
	mov 	ah, 02h ; Set AH to the DOS code for write-character.
	int 	21h ; DOS!

	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf

	ret
PrintHexDigit ENDP

; prints a char passed in dl
PrintChar PROC
	push ax
	push dx
	pushf

	mov 	ah, 02h ; Set AH to the DOS code for write-character.
	int 	21h ; DOS!

	popf
	pop dx
	pop ax
	ret
PrintChar ENDP

;; =======================================================================================
; stuff for rand
FIRST_SEED = 0100110001110000b
Random16Seed WORD FIRST_SEED

Random16 PROC
		;; returns:
		;; ax - a 16-bit random number
	.386
		pushf
		push	edx
		push	eax

		cmp	Random16Seed, FIRST_SEED
		jne	good
		call	Randomize
	good:
		add	Random16Seed, 0FC15h
		movzx	eax, Random16Seed
		mov	edx, 02ABh
		mul	edx
		mov	edx, eax
		shr	edx, 16
		xor	eax, edx
		and	eax, 0FFFFh
		mov	edx, eax

		pop	eax
		mov	ax, dx
		pop	edx
		popf
		ret
	.8086
Random16 ENDP

Randomize PROC
	;; sets seed to current hundreths of seconds
	pushf
	push	ax
	push	bx
	push	cx
	push	dx

	mov	ah,2Ch
	int	21h		; ch (hrs), cl (mins), dh (sec), dl (hsec)

	mov	bh, 0
	mov	bl, dl

	mov	dh, 0
	mov	dl, dh
	mov	ax, 100
	mul	dx
	add	bx, ax

	mov	dh, 0
	mov	dl, cl
	mov	ax, 6000
	mul	dx
	add	bx, ax

	mov	Random16Seed, bx
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	popf
	ret
Randomize ENDP

RandRange PROC
	;; ax - maximum value + 1
	;; returns:
	;; ax - a value between 0 - (ax - 1)
	pushf
	push	bx
	push	dx

	mov	bx, ax
	call	Random16
	mov	dx, 0
	div	bx
	mov	ax, dx

	pop	dx
	pop	bx
	popf

	ret
RandRange ENDP
;; ========================================================================================

;; row passed in ch
;; col passed in cl
;; returns offset in ax
RC2Offset PROC
	push cx
	pushf

	mov ax, 80
	mul ch ; ax = 80 * row = ax * ch
	mov ch, 0 ; now cx = cl = col
	add ax, cx ; ax is index
	shl ax, 1 ; mul by 2: ax = offset

	popf
	pop cx
	ret
RC2Offset ENDP

;; takes row and col in ch and cl
;; takes char in al
;; takes attributes in ah
PlaceChar PROC
	push ax
	push di
	push es
	pushf

	mov di, 0B800h
	mov es, di
	push ax
	call RC2Offset
	mov di, ax
	pop ax

	; mov ah, 10001100b
	mov es:[di], ax

	popf
	pop es
	pop di
	pop ax
	ret
PlaceChar ENDP

padding1 BYTE "                          ", 0
welcome1 BYTE "     Welcome to snake     ", 0
welcome2 BYTE "     Use WASD to move     ", 0
welcome3 BYTE "  Press any key to start  ", 0
welcome4 BYTE "     Press c to exit      ", 0

WelcomeWrite PROC
	push bx
	push cx
	pushf

	mov cx, 0919h

	mov bx, OFFSET padding1
	call WriteStringToMem
	inc ch

	mov bx, OFFSET welcome1
	call WriteStringToMem
	inc ch

	mov bx, OFFSET welcome2
	call WriteStringToMem
	inc ch

	mov bx, OFFSET welcome3
	call WriteStringToMem
	inc ch

	mov bx, OFFSET welcome4
	call WriteStringToMem
	inc ch

	mov bx, OFFSET padding1
	call WriteStringToMem
	inc ch

	popf
	pop cx
	pop bx
	ret
WelcomeWrite ENDP

WriteStringToMem PROC
	; cx = starting location
	; bx = offset of bfr
	push si
	push ax
	push cx
	pushf

	mov si, 0
	mov ah, 01110000b

	top:
	mov al, [bx+si]
	cmp al, 0h
	je done

	call PlaceChar
	inc si
	inc cl
	jmp top ; increase indices and keep reading

	done:
	popf
	pop cx
	pop ax
	pop si
	ret
WriteStringToMem ENDP

edges PROC
	push ax
	pushf

	mov ah, 01000100b ;; change to  01000100b
	mov al, 'e'
	call topedge
	call bottomedge
	call rside
	call lside

	popf
	pop ax
	ret
edges ENDP

topedge PROC
	push cx
	push ax
	pushf
	;; col is in cl
	mov cx, 0100h

		jmp cond
	top:
		call PlaceChar
		inc cl
	cond:
		cmp cl, 80
		jl top

	mov cx, 0009h

		jmp cond2
	top2:
		push ax
		mov ah, 00010001b
		call PlaceChar
		pop ax
		inc cl
	cond2:
		cmp cl, 80
		jl top2




	popf
	pop ax
	pop cx
	ret
topedge ENDP

bottomedge PROC
	push cx
	push ax
	pushf
	;; col is in cl
	mov cx, 1800h

		jmp cond
	top:
		call PlaceChar
		inc cl
	cond:
		cmp cl, 80
		jl top

	popf
	pop ax
	pop cx
	ret
bottomedge ENDP

rside PROC
	push cx
	push ax
	pushf
	;; col is in cl
	mov cx, 0200h

		jmp cond
	top:
		call PlaceChar
		inc ch
	cond:
		cmp ch, 24
		jl top

	popf
	pop ax
	pop cx
	ret
rside ENDP

lside PROC
	push cx
	push ax
	pushf
	;; col is in cl
	mov cx, 024fh

		jmp cond
	top:
		call PlaceChar
		inc ch
	cond:
		cmp ch, 24
		jl top

	popf
	pop ax
	pop cx
	ret
lside ENDP

background PROC
	push cx
	push ax
	pushf
	;; col is in cl
	mov cx, 0101h
	mov ah, 00011100b
	mov al, ' '

	jmp cond1
	top1:
				call bluerow
			inc ch

	cond1:
		cmp ch, 24
		jl top1

	popf
	pop ax
	pop cx
	ret
background ENDP

bluerow PROC
	pushf

	mov cl, 01h
		jmp cond2
	top2:
		call PlaceChar
		inc cl
	cond2:
		cmp cl, 79
		jl top2

	popf
	ret
bluerow ENDP

placesnake PROC
	push ax
	push cx
	push bx
	pushf

	mov cx, 0303h
	mov al, 's'
	mov ah, 01110111b
	call PlaceChar

	mov bx, OFFSET SnakeAry
	mov [bx+0], ch
	mov[bx+2], cl ;; RC in first 4 bytes of snakeary

	mov si, 0 ; initialize indexing for head and tail
	mov di, 0

	popf
	pop bx
	pop cx
	pop ax
	ret
placesnake ENDP

placefood PROC
	push ax
	push cx
	push bx
	pushf

	mov ax, 22
	call RandRange
	add ax, 2 ;; to account for walls
	mov ch, al ;; put num < 25 into Row value

	mov ax, 78
	call RandRange
	inc ax
	mov cl, al ;; put num < 80 into col value

	mov al, 'f'
	mov ah, 00100010b
	call PlaceChar

	popf
	pop bx
	pop cx
	pop ax
placefood ENDP

UpdateAry PROC
	pushf
	;; si currently points to old head

	; call dumpregs
	mov bx, OFFSET snakeary
	add si, 4
	cmp si, MAXARY ;; if we are at end of array, move pointer back to beginning of buffer
	jne keepSI ;; if equal, reset si to beginning of buffer , otherwize
		mov si, 0
		;; for some reason this adds another snake character
		call PlaceSpace
		jmp AddHeadToAry

	keepSI:
	mov [bx+si], ch
	mov [bx+si + 2], cl

	AddHeadToAry:
	popf
	ret
UpdateAry ENDP

;; takes no parameters, no return
placescore PROC ;; tested
	push ax
	push cx
	pushf

	mov ah, 00011111b ; white text on blue
	mov cx, 0000h

	mov al, 's'
	call PlaceChar
	inc cl
	mov al, 'c'
	call PlaceChar
	inc cl
	mov al, 'o'
	call PlaceChar
	inc cl
	mov al, 'r'
	call PlaceChar
	inc cl
	mov al, 'e'
	call PlaceChar
	inc cl
	mov al, ':'
	call PlaceChar
	inc cl
	mov al, ' '
	call PlaceChar
	inc cl
	mov al, '0'
	call PlaceChar
	inc cl
	mov al, '0'
	call PlaceChar

	popf
	pop cx
	pop ax
	ret
placescore ENDP

;; passed next location in cx
;; changes si to point to new head
movehead PROC
	push ax
  push cx
	pushf

	mov al, 's'
	mov ah, 01110111b
	call PlaceChar

  done:
	popf
  pop cx
	pop ax
	ret
movehead ENDP

PlaceSpace PROC
	push ax
  push cx
	pushf

	mov al, ' '
	mov ah, 00011111b
	call PlaceChar

  done:
	popf
  pop cx
	pop ax
	ret
PlaceSpace  ENDP

;; bx is offset of snakeary
;; di is offset of tail in snakeary
removetail PROC
  push ax
  push cx
	pushf

  ;; get tail offset

  mov ch, [bx + di]
  add di, 2
  mov cl, [bx + di] ;; now cx contains location of old tail to remove
  add di, 2 ;; now di points to new tail

  mov al, ' '
  mov ah, 00011001b
  call PlaceChar

  ;; di currently points to old head
  cmp di, MAXARY ;; if we are at end of array, move pointer back to beginning of buffer
  je ResetDI

  done:
	popf
  pop cx
  pop ax
  ret

  ResetDI:
  mov di, 0
  jmp done
removetail ENDP

;; takes direction in al : 'w', 'a', 's', 'd'
;; cx is RC loc of current head
;; returns intended RC loc of head
;WORKS
NextHeadLoc PROC
	pushf

	; call dumpregs
	; call dumpregs
	cmp al, 'w' ;; up
		je up
	cmp al, 's' ;; down
		je down
	cmp al, 'a' ;; left
		je left
	cmp al, 'd' ;; right
		je right

	jmp done


	up:
		dec ch; decrease row num
		jmp done
	down:
		inc ch
		jmp done
	left:
		dec cl
		jmp done
	right:
		inc cl
		jmp done

	done:
	; call dumpregs
	popf
	ret
NextHeadLoc ENDP

;; bx, si index head in snakeary, set cx = head RC
GetCurrHeadLoc PROC
	pushf
  mov ch, [bx + si]
  mov cl, [bx + si + 2]
	popf
  ret
GetCurrHeadLoc ENDP

;; takes row and col in ch and cl
;; returns char in al
GetChar PROC ;;works - tested
	push di
	push es
	pushf

	mov di, 0B800h
	mov es, di
	push ax
	call RC2Offset ;; offset returned in ax
	mov di, ax ;; offset in di
	pop ax

	mov ax, es:[di]

	popf
	pop es
	pop di
	ret
GetChar ENDP

;; nextloc passed in cx
CheckNextLoc PROC
	push ax
	push cx
	push dx
	pushf
	call getchar ;get char at nextloc - load into al
	cmp al, 'e' ; if hit edge, gameover
		je dead
	cmp al, 's' ; if hit snake, gameover
		je dead

	cmp al, 'f' ; if food, eat
	je food

	jmp regmove  ;; otherwise a valid regular move

	food:
		call movehead ;; movehead happens on every valid move, cx is next RC
		call placefood
		call incscore
		call PlayFoodSound

		popf
		pop dx
		pop cx
		pop ax
		ret

	regmove:
		; call regsong
		call UpdateAry
		call movehead
		call removetail
		call PlayRegSound
		popf
		pop dx
		pop cx
		pop ax
		ret

	dead:
		popf
		pop dx
		pop cx
		pop ax
		call gameover ;; gameover terminates
		ret
CheckNextLoc ENDP

updatesnake PROC
	pushf

	call NextHeadLoc ;; now cx is next index
	call CheckNextLoc ;; calls movehead and removetail accordingly

	popf
	ret
updatesnake ENDP

makeboard PROC
	call background
	call edges
	call placescore
	call placesnake

	ret
makeboard ENDP

IncScore PROC
	push ax
	push bx
	push cx
	push dx
	pushf

	mov cx, 0008h
	call getchar ; get second digit of score
	cmp al, '9';; if second digit is 9
	jne updateD2
		;; otherwise digit is 9, increase first digit
		mov al, '0'
		mov ah, 00011111b
		call PlaceChar ; set 2nd digit to 0
		mov cx, 0007h
		call GetChar ; cx is first digit of score
		inc al
		mov ah, 00011111b
		call PlaceChar
		jmp done

	updateD2:
	inc al
	mov ah, 00011111b
	call PlaceChar

	done:
	popf
	pop dx
	pop cx
	pop bx
	pop ax
	ret
IncScore ENDP

padding BYTE "                           ", 0
finalscore1 BYTE "         Game Over!        ", 0
finalscore2 BYTE "  Your final score is: 00  ", 0
finalscore3 BYTE "   Press c to exit game!   ", 0

getfinalscore PROC
	push ax
	push bx
	push cx
	push si
	pushf

	mov bx, OFFSET finalscore2
	mov si, 23
	mov cx, 0007h
	call getchar
	mov [bx + si], al
	inc si
	inc cl
	call GetChar
	mov [bx+si], al

	popf
	pop si
	pop cx
	pop bx
	pop ax
	ret
getfinalscore ENDP

fuckflags PROC
	push ax
	pushf

	pop ax ; flags copied into ax
	or ax, 0111111111111111b

	push ax ; new flags on stack
	popf ; pop flags into f
	pop ax
	ret
fuckflags ENDP

gameover PROC

	; call gameoversound
	call getfinalscore ;; loads score into finalscore buffer

	mov cx, 0A18h
	mov bx, OFFSET padding
	call WriteStringToMem
	inc ch
	mov bx, OFFSET finalscore1
	call WriteStringToMem
	inc ch
	mov bx, OFFSET finalscore2
	call WriteStringToMem
	inc ch
	mov bx, OFFSET finalscore3
	call WriteStringToMem
	inc ch
	mov bx, OFFSET padding
	call WriteStringToMem

	call PlayGameoverSound
	mov cx, 1203h

	mov al, 'c'
	ret
gameover ENDP

UpdateScreen PROC
	;; checked dumpregs, init cx = 0303h
	pushf
	cmp al, 0
	je done ;; only start updating screen when a direction is given

	push cx
	push ax
	mov cx, 0B21h
	call getchar ; char in al
	cmp al, 'G'
	pop ax
	pop cx
	je done

	call updatesnake ; this seems to be whats tripping up the program
	mov ticks, 0

	done:
	popf
	ret
UpdateScreen ENDP
;; ========================================================================================

;; interrupt number passed in AL
;; Sets vector in ds:dx
SetInterruptVector PROC
  push ax

  mov ah, 25h
  int DOS ; set interrupt vector to DS:DX

  pop ax
  ret
SetInterruptVector ENDP

; interrupt number passed in AL (for Set call)
; Sets vector in cs:dx
InstallHandler PROC
  push bx
  push ds

  mov bx, cs ;; intermediate step to mov ds, cs
  mov ds, bx
	call SetInterruptVector ; Sets vector to what was in cs (instead of ds)

  pop ds
  pop bx
  ret
InstallHandler ENDP

;; interrupt number passed in AL
;; returns interrupt vector in ES:BX
GetInterruptVector PROC
  push ax

  mov ah, 35h
  int DOS ; set interrupt vector to ES:BX

  pop ax
  ret
GetInterruptVector ENDP

;; interrupt number passed in AL
;; pass offset for DWORD in DX to store vector
SaveVector PROC
  push bx
	push cx
	push dx
	push es

  call GetInterruptVector ;; returns vector in ES:BX
	mov cx, bx ;; now VectorOffset in cx
	mov bx, dx ;; DWORD offset in bx for memory access

	mov [bx + 0], cx ; mov cx offset into VectorOffset
	mov [bx + 2], es; mov es seg into VectorSegment

	pop es
	pop dx
	pop cx
	pop bx
  ret
SaveVector ENDP

;; pass interrupt number in AL -- do I actually need this? I think no
;; pass offset for DWORD in DX to store vector
RestoreVector PROC
	push bx
	push bx
	push ds

	mov bx, dx ;; for indexing DWORD
	mov dx, [bx + 0] ;; move offset into dx
	mov ds, [bx + 2] ;; move segment into ds
	call SetInterruptVector

	pop ds
	pop bx
	pop bx
	ret
RestoreVector ENDP

;; ES:BX contains vector to write
WriteInterruptVector PROC
  push bx
  push ax

  mov ax, es
  call printreg ;; print es

  mov ax, bx ; move contents of register into ax
  call PrintReg ;; print bx

  pop ax
  pop bx
  ret
WriteInterruptVector ENDP

WriteSavedVector PROC
	;; DX - offset of DWORD containing vector
	call writestring

  ret
WriteSavedVector ENDP

;; ========================================================================================
score BYTE 0
ticks BYTE 0
songticks BYTE 0
keypresses BYTE 0

Tick PROC
	inc	ticks
	sti
	; push	ax
	push	dx

	; mov	dl, '!'
	; mov	ah, 02h
	; int	DOS

	cmp ticks, 2
	jl done
	call updatescreen

	done:
	;	pushf
	;	call	ds:ClockVector
	;	popf
	pop	dx
	; pop	ax
	iret
Tick ENDP

KBD_INTERRUPT = 09h
CLK_INTERRUPT = 1Ch
BIOS_CLK_INTERRUPT = 1Ch
PROG_END_INTERRUPT = 22h

INTERRUPT = BIOS_CLK_INTERRUPT

ClockVector LABEL DWORD
ClockOffset WORD 0
ClockSegment WORD 0

main PROC
	;	mov	ax, @data		; Setup data segment
	mov	ax, cs
	mov	ds, ax

	call makeboard
	call WelcomeWrite
	push ax
	mov ah, 10h ;; read single character
	int 16h ;; character is in AL

	cmp al, 'c'
	je done

	pop ax
	call makeboard
	call placefood


	mov	al, INTERRUPT
	mov	bx, 0
	mov	es, bx
	call	GetInterruptVector
	;; Save the vector
	mov	al, INTERRUPT
	mov	dx, offset ClockVector
	call	SaveVector

	mov	dx, offset ClockVector

	mov	al, INTERRUPT
	mov	dx, Tick
	call	InstallHandler

	mov	al, INTERRUPT
	mov	bx, 0
	mov	es, bx
	call	GetInterruptVector

	mov si, 0 ;; start these at 0, they will index head
	mov di, 0
	mov cx, 0303h

	; call SetCarry

	mov ax, 'd'
	top:
		;; first check flag for gameover
		cmp al, 'c'
			je quit

		mov dx, ax
		mov ah, 10h ;; read single character
		int 16h ;; character is in AL
		cmp al, 'c'
			je quit

		cmp al, 'w'
			je valid
		cmp al, 'a'
			je valid
		cmp al, 's'
			je valid
		cmp al, 'd'
			je valid

		mov ax, dx ;; if not a valid character, we restore ax to its prev value

		valid:
		; every time a valid key is pressed, change high to low sound
		cmp keypresses, 0
		je already0
		mov keypresses, 0 ; otherwise make it 0
		jmp doneWkeypress
		already0:
			mov keypresses, 1

		doneWkeypress:
		jmp top

	quit:
	mov	al, INTERRUPT
	mov	dx, offset ClockVector
	call	RestoreVector

	;; Display the active vector
	mov	al, INTERRUPT
	mov	bx, 0
	mov	es, bx
	call	GetInterruptVector

	done:
	mov	ax, TERMINATE		; Signal DOS we are done
	int	DOS
main ENDP


END main
