
%if 0

Resident code of TSR example
 2020 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

%if 0

Supported Int2D functions:

AMIS - Installation check
INP:	al = 00h
OUT:	al = 0FFh
	cx = Private version number (currently 0100h)
	dx:di-> signature: "ecm     ","TSR     "

AMIS - Get private entry point - NOP: no private entry point
INP:	al = 01h
OUT:	al = 00h

AMIS - Uninstall - NOP: no resident uninstaller or NOP: can't uninstall
INP:	al = 02h
OUT:	If installed from command line,
	 al = 03h
	 bx = memory block of resident TSR (cs)
	(If internal (pre-installed),
	 al = 01h
	If with resident uninstaller,
	 al = 01h if unsuccessful
	 al = 0FFh if successful)

AMIS - Request pop-up - NOP: no pop-up
INP:	al = 03h
OUT:	al = 00h

AMIS - Determine chained interrupts
INP:	al = 04h
OUT:	al = 04h
	dx:bx-> interrupt hook list (Int2D always.)

AMIS - Get hotkeys - NOP: no hotkeys
INP:	al = 05h
OUT:	al = 00h

AMIS - Get device driver information - NOP: no device
INP:	al = 06h
OUT:	al = 00h

AMIS - Reserved for AMIS
INP:	al = 07h..0Fh
OUT:	al = 00h

TSR - Get compatible version
INP:	al = 10h
OUT:	al = 0FFh
	cx = compatible version number (currently 0100h)

TSR - Get ctrl0
INP:	al = 20h
OUT:	al = size of returned control data (not 0)
	dx:bx-> control data
	Unsupported bits switched off

TSR - Reserved for TSR
INP:	al = 11h..1Fh, 21h..FFh
OUT:	al = 00h

%endif

%if TSR_VERP != ver(1,20)		; if defined one doesn't match current
 %error "TSR version doesn't match"
%endif

handler:
				; AMIS stuff
amissig:
.ven:	fill 8,32,db "ecm"		; vendor
.prod:	fill 8,32,db "TSR"		; product
%if 0
.desc:	asciz ;"Example TSR"		; save memory by omitting the description
%else
.desc:	asciz "Example TSR"		; description
%endif
%if $ - .desc > 64
 %error AMIS description too long
%endif

amisintr:
.i21:	db 21h
	dw i21
.i2D:	db 2Dh
	dw i2D

				; TSR data
ctrl0:	db 1100_0000b			; control byte
					; bit 6: state 1
					;     7: state 2

i2D.uninstall:
	inc ax				; (= 03h) safe to remove but no resident uninstaller
	mov bx, cs			; = segment
i2D.hwreset equ $-1		; (second byte of mov bx, cs is same as the retf opcode)
	iret

iispentry i2D, 0, i2D
	cmp ah, 0
amisnum equ $-1				; AMIS multiplex number (data for cmp opcode)
	je .handle			; our multiplex number -->
	jmp far [cs:.next]		; else go to next handler -->

.handle:
	test al, al
	jz .installationcheck		; installation check -->
	cmp al, 02h
	je .uninstall			; uninstallation -->
	cmp al, 04h
	je .determineinterrupts		; determine hooked interrupts -->
	cmp al, 10h
	je .getver			; get compatible version -->
	cmp al, 20h
	je .getctrl0			; get ctrl0 -->
				; all other functions are reserved or not supported by TSR
.nop:
	mov al, 0			; show not implemented
	iret

.installationcheck:
	dec al				; (= FFh) show we're here
	mov cx, TSR_VERP		; = version
	xor di, di			; dx:di -> AMIS signature strings of this program
.iret_dx_cs:
	mov dx, cs
.iret:
	iret

.determineinterrupts:			; al = 04h, always returns list
	mov bx, amisintr		; dx:bx -> hooked interrupts list
	jmp short .iret_dx_cs

.getver:
	mov al, 0FFh			; show call supported
	mov cx, TSR_VERC		; = compatible version
	iret

.getctrl0:
	mov bx, ctrl0			; dx:bx -> ctrl0
	mov al, 1			; size of ctrl0: 1 byte
	jmp short .iret_dx_cs


iispentry i21, 0, i2D
	jmp far [cs:.next]


	align 16
	endarea handler
