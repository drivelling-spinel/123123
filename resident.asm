
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

%if 0
%if TSR_VERP != ver(0,1)               ; if defined one doesn't match current
 %error "TSR version doesn't match"
%endif
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
.i9:    db 9h
        dw i9
.i70:   db 70h
        dw i70
.i2D:	db 2Dh
	dw i2D

				; TSR data
ctrl0:	db 1100_0000b			; control byte
					; bit 6: state 1
					;     7: state 2

i2D.uninstall:
        inc ax                          ; (= 03h) safe to remove but no resident uninstaller
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

%idefine KEY_MAKE_B 30h
%idefine KEY_MAKE_N 31h
%idefine KEY_MAKE_1 02h
%idefine KEY_BREAK_1 82h
%idefine KEY_BREAK_B 0B0h
%idefine KEY_BREAK_N 0B1h

iispentry i9, 0
        push ax
        push bx
        push dx
        push cx
        push es

        mov ah, 35h            ;chained to with out original offset
        mov al, 9h             ;either we installed ourselves, or another ISR has 
        int 21h                ;gracefully added us as next in the chain
                               ;and is ceeding control, let's see...
        mov ax, es
        mov dx, cs
        cmp ax, dx
        jnz .skippush          ;definitely not us, so proceed as normal
        cmp bx, i9.override    ;us but via the force override entry point (below)
        jz .force_chain        ;not safe to continue
        cmp bx, i9
        jnz .skippush          ;not us, proceed as normal
        xor bx, bx             ;otherwise clean up avoid going to a non-address
        mov word [cs:i9.hijacker], bx
        mov word [cs:i9.hijacker + 2], bx
        jmp .skippush
        
.override:                     ;called with the forced override entry point
        push ax                ;always act normally, as override is only applied once
        push bx
        push dx
        push cx
        push es
        
.skippush:
        mov bl, [cs:.ctr]
        in al, 60h
        mov dx, ax
        mov dh, KEY_MAKE_1

.check_b:
        cmp dl, KEY_MAKE_B
        jnz .check_n
        test bl, bl
        jnz .ctr_dec
        mov bl, [cs:.lim]
.ctr_dec:
        dec bl
        jmp .meddle

.check_n:
        cmp dl, KEY_MAKE_N
        jnz .check_b_br
        inc bl
        cmp bl, [cs:.lim]
        jl .meddle
        xor bx, bx
        jmp .meddle

.check_b_br:
        mov dh, KEY_BREAK_1
        cmp dl, KEY_BREAK_B
        jz .meddle

.check_n_br:
        cmp dl, KEY_BREAK_N
        jz .meddle

.chain:
        mov ax,  word [cs:.hijacker]
        or ax, word [cs:.hijacker + 2]
.force_chain:
        pop es
        pop cx
        pop dx
        pop bx
        pop ax
        jnz .to_hijacker
        jmp far [cs:.next]
.to_hijacker:
        jmp far [cs:.hijacker]

.meddle:
        mov [cs:.ctr], bl
        mov al, 0ADh
        call .8024_write
        jnz .panic
       
        mov al, 0D2h  
        call .8024_write
        jnz .panic
        mov al, dh
        add al, bl
        out 60h, al
.panic:
        mov al, 0AEh
        call .8024_write
        mov al, 20h
        out 20h, al
        
        pop es
        pop cx
        pop dx
        pop bx
        pop ax
        iret

.8024_write:
        push ax
        mov cx, 0fffh
.8024_full:
        in al, 64h
        test al, 2
        loopnz .8024_full
        pop ax
        jnz .8024_fail
        out 64h, al
.8024_fail:
        retn

.ctr:
 db 0
.lim:
 db 7
.hijacker:
 dd 0

iispentry i70, 0
        push ax
        push bx
        push cx
        push dx
        push es
        push ds
        
        mov ah, 35h
        mov al, 9h
        int 21h
        
        mov ax, es
        mov dx, cs
        cmp ax, dx
        jnz .fixup
        cmp bx, i9.override
        jz .done
        cmp bx, i9
        jnz .fixup
        jmp .done

.fixup:
        mov ax,  word [cs:i9.hijacker]      ;but don't fixup if have fixed up already once
        or ax, word [cs:i9.hijacker + 2]    ;so as to avoid circular loops, since this TSR
        jnz .done                           ;does not maintain a stack of overrides

        mov word [cs:i9.hijacker], bx
        mov word [cs:i9.hijacker + 2], es
        mov byte [cs:i9.ctr], 0

         xor dx, dx
         mov ds, dx
         mov dx, i9.override
         mov word [ds:(9 * 4)], dx
         mov cx, cs
         mov word [ds:(9 * 4) + 2], cx

.done:
        pop ds
        pop es
        pop dx
        pop cx
        pop bx
        pop ax
        jmp far [cs:.next]



	align 16
	endarea handler
