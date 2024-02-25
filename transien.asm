
%if 0

Transient code of TSR example; command line parser and setup
 2020 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

	%assign __lMACROS1_MAC__DEBUG_DEFAULTS	1
%include "lmacros3.mac"
%include "AMIS.MAC"

	defaulting

	verdef TSR,C, 1,00		; Version number of last compatible
	verdef TSR,P, 1,20		; Private version

	numdef PSPRELOC, 1		; relocate PSP
	numdef UPXPAD, 0		; pad executable to >=3062 bytes for UPX

		; Option flags (hold in bp)
_onlystate		equ 0001h
_installlow		equ 0002h
_installnew		equ 0004h
_uninstall		equ 0008h
_state1			equ 0040h		; must match bit in ctrl0
_state2			equ 0080h		; must match bit in ctrl0
_state			equ _state1 | _state2
_1set			equ _state1<<2
_2set			equ _state2<<2
_foundresident		equ 1000h
_incompatresident	equ 2000h

%ifdef _MAP
	[map all _MAP]
%endif

	cpu 8086
	org 100h

	section TRANSIENT align=1	; Transient program part (command line parser, setup)

transient:
	cld
	mov dx, msg.name
	call disp_msg		; Start with message

			; The UMB link and memory allocation strategy is now always safed here
			; and restored by abort_msg as well as any other termination code. This
			; saves any code changing the strategy the effort to restore it proper.
	mov ax, 5802h
	int 21h
	xor ah, ah		; some "DOS" only return al
	mov word [ restored.umblink ], ax	; save UMB link

	mov ax, 5800h
	int 21h
	mov word [ restored.strategy ], ax	; save strategy

	mov dx, i23
	mov ax, 2523h
	int 21h			; set Int23 so that we restore link/strategy on Ctrl-C as well
	mov dx, i24
	mov ax, 2524h
	int 21h			; set Int24 so all hard errors are changed to the soft error


checkhandlers:
	mov ax, 352Dh
	int 21h
	cmp bx, -1
	je .inv
	mov ax, es
	test ax, ax
	jz .inv
	mov ax, 352Fh
	int 21h
	cmp bx, -1
	je .inv
	mov ax, es
	test ax, ax
	; jz .inv
	jnz @F
.inv:
	mov dx, msg.handlersbad
	jmp abort_msg
@@:


getfirstumcb:
			; We try to get the first UMCB for various uses later. Note that we
			; never use the result as an MCB address so if it's wrong we won't
			; crash or something. (We compare memory block or MCB addresses to
			; the value to check whether they're in the UMA.)
	mov ax, 1261h		; PTS-DOS: Get first UMCB
	stc
	int 2Fh
	jc .determine		; not supported -->
	inc ax
	cmp ax, byte 2		; -1, 0, 1 ?
	jbe .determine		; not supported (or none) -->
	dec ax
	mov word [ firstumcb ], ax	; set UMB
	jmp short .got		; got it -->

.determine:
	mov ax, 5803h
	xor bx, bx
	stc
	int 21h			; disable UMB link, leave only LMA chain
	jc .none		; that isn't supported either -->

	mov bx, -1
	mov ah, 52h
	int 21h			; get Lists of Lists
	cmp bx, -1		; unchanged ?
	je .none		; yes -->
	cmp bx, 1		; bx - 2 would be FFFFh ?
	je .none		; yes, avoid fault -->
	mov ax, word [ es:bx - 2 ]
	mov dx, ax		; first MCB
	xor bx, bx		; use offsets from bx, not addresses
.looplmb:
	mov ds, ax
	inc ax
	add ax, word [ bx + 3 ]	; next MCB's address
	cmp byte [ bx ], 'M'
	je .looplmb		; not last -->
	cmp byte [ bx ], 'Z'
	jne .none		; corrupted -->
	xchg ax, dx		; dx = what we assume to be the first UMA chain MCB
				; ax = first MCB

	push ax
	inc bx			; = 1
	mov ax, 5803h
	int 21h			; enable UMB link, include UMA chain
	pop ax
	jc .none		; so we can disable it but not enable? -->

	dec bx			; = 0
	xor cx, cx		; flag if assumed first UMCB found
.loopumb:
	cmp ax, dx
	jne .notlastlmb
	inc cx			; there it is
.notlastlmb:
	mov ds, ax
	cmp byte [ bx ], 'M'
	jne .islastumb?		; last or corrupted -->
	inc ax
	add ax, word [ bx + 3 ]
	jmp short .loopumb	; process next -->
.islastumb?:
	cmp byte [ bx ], 'Z'
	jne .none		; corrupted -->
	jcxz .none		; didn't find that UMCB -->
			; The MCB at dx which was behind the one that contained the 'Z'
			; signature when we disabled the UMB link is now a valid MCB in
			; the MCB chain after we enabled the UMB link. All previous MCBs
			; are now 'M'.
	mov word [ cs:firstumcb ], dx
.none:
.got:
				; no need to restore es here since we do that below
				; no need to restore ds since we set it below

scancl:			; bp = selected options
	xor bp, bp		; initial option flags

	push cs
	pop es
	push cs
	pop ds			; ds, es changed while searching UMCB
	mov si, 81h		; PSP command line
.loop:	lodsb
	cmp al, 9		; tab ?
	je .loop
	cmp al, 32		; blank ?
	je .loop
	jb .end			; zero, cr, lf ? -->
	cmp al, '-'
	je .switch
	cmp al, '/'
	jne .invalid		; unknown character -->
			; parse switch
.switch:
	lodsb			; get next character
	call capitalise

			; check against known switches
	push si
	mov si, parameters
	xchg al, ah
.loopswitch:
	lodsb
	xchg ax, dx
	lodsw			; get function to call
	xchg ax, dx
	xchg ax, cx
	lodsw
	xchg ax, cx		; cx = option flags
	cmp al, 0FFh
	je .invalid		; not found!
	cmp al, ah
	jne .loopswitch		; try next -->
	pop si
	call dx
	jmp .loop

.switch_j:
	lodsb
	cmp al, '-'
	je .j_plusminus
	cmp al, '+'
	je .j_plusminus
	dec si
	cmp al, '/'
	je .clear_umb
	cmp al, 13
	je .clear_umb
	cmp al, 9
	je .clear_umb
	cmp al, 32
	je .clear_umb
	mov al, '+'
.j_plusminus:
	mov ah, 0
	mov byte [old_j_switch], ah
	cmp al, '+'
	je @F
	mov ah, -1
@@:
	lodsb
	cmp al, '-'
	je .j_plusminus
	cmp al, '+'
	je .j_plusminus
	cmp al, 13
	je .dec_retn
	cmp al, 9
	je .dec_retn
	cmp al, 32
	je .dec_retn
	call capitalise
	mov di, j_options
	mov cx, j_options.amount
	repne scasb
	jne .invalid
	mov byte [di - (j_options + 1) + j_flags], ah
	jmp @B


.clear_umb:
	mov byte [j_flags.umb], 0
	retn

.set_in_bp:
	test cl, _state		; state ?
	jz .normalbp		; no, normal option --> (sets bit in bp)

		; cx = bit to set or clear in bp
		; cx<<2 = bit to set in bp to show the cx bit was set
		; ds:si-> '+' to set, '-' to clear
	lodsb
	cmp	al,'-'		; OFF ?
	jne	.notminus
	call	flagset
	not	cx		; reverse bits
	and	bp,cx		; do bp &= ~cx
	retn

.notminus:
	cmp	al,'+'		; ON ?
	jne	.invalid
	call	flagset
				; then fall through to do bp |= cx
.normalbp:
	or bp, cx
	retn			; ds:si-> character after switch character. just continue

.dec_retn:
	dec si
	retn


.invalid:			; display help if invalid
	mov dx, msg.help
	jmp abort_msg

.multiplexnumber:
	lodsb
	cmp al, '='
	je @FF
	cmp al, ':'
	je @FF

	db __TEST_IMM8
@@:
	lodsb
	cmp al, 9
	je @B
	cmp al, 32
	je @B
	jb .invalid

	db __TEST_IMM8
@@:
	lodsb
	call getnybble
	jc .invalid
	xor bx, bx
	mov bl, al
	lodsb
	call getnybble
	jc .check_end_multiplexnumber
	add bx, bx
	add bx, bx
	add bx, bx
	add bx, bx
	add bl, al
	lodsb
.check_end_multiplexnumber:
	cmp al, 9
	je @F
	cmp al, 32
	ja .invalid
@@:

	xchg bh, bl
	mov word [trymultiplexnumber], bx
	dec si
	retn


.end:


		; Search for installed debugger and our resident
findinstalled:
	lframe none
	lenter

	mov ax, cs
	add ax, 10h+transient_size_p
	lvar dword, signature_us
	 push ax
	mov bx, amissig
	 push bx

	lvar dword, signature_debugger
	 push ds
	mov bx, msg.debuggeramissig
	 push bx

	lvar word, function
	mov bx, .checkboth
	rol byte [j_flags.detectdebugger], 1
	jc @F
	mov bx, .checkus
@@:
	 push bx

	lvar dword, our_ctrl0
	mov bx, ctrl0		; ds:bx -> resident segment's ctrl0
	 push ax
	 push bx

	lvar word, our_multiplex_high
	 push ax		; (uninitialised)


		; This loop is optimised so that the minimum
		;  number of instructions is run, aiding the
		;  debugger when it traces this loop.
		; An indirect function call is used to decide
		;  which checks to run, either the debugger
		;  only, our resident only, or both. This
		;  starts out as both, but will change if the
		;  both handler finds one of them.
		; If a single handler subsequently also finds
		;  its multiplexer it won't return here and
		;  instead branch to the end, discarding the
		;  near return address to here.
		; We can check both multiplexers using only
		;  one int 2Dh call, avoiding the need for
		;  querying the same multiplexer twice.
	mov ax, word [trymultiplexnumber]
	test al, al
	jnz @F
		; This changes the behaviour slightly: The
		;  try number now is used for the debugger,
		;  as well. This is considered acceptable.
	call near [bp + ?function]
@@:

	mov ah, 0FFh		; start with multiplex number 0FFh
.loop:
	call near [bp + ?function]
	sub ah, 1		; search is backward (to find latest installed first), from 0FFh to 00h including
	jnc .loop		; try next if we didn't check all yet -->

	jmp .end		; if not both found, continue -->


		; INP:	ah = multiplex number to check
		;	word [bp + ?function] -> .checkboth
		; OUT:	always returns
		;	ah = multiplex number (unmodified)
		;	word [bp + ?function] -> function to call for next check
		;	if debugger found,
		;	 word [debuggerfunction] = multiplex number high, 30h low
		;	if resident instance of us found,
		;	 word [bp + ?frame_bp] = flags set appropriately
		;	 dword [bp + ?our_ctrl0] -> ctrl0 byte
		;	 byte [bp + ?our_multiplex_high + 1] = multiplex number
		;	if neither found,
		;	 variables unmodified
		; CHG:	al, ds, si, es, di, dx, cx, bx
.checkboth:
	mov al, 00h		; AMIS installation check
	int 2Dh			; AMIS (or "DOS reserved" = iret if no AMIS present)
	cmp al, 0FFh
	jne .notfound
	lds si, [bp + ?signature_debugger]
				; ds:si -> debugger AMIS name strings
	mov es, dx		; es:di -> name strings of AMIS multiplexer that just answered
	mov cx, 8		; Ignore description, only compare vendor and program name
	push di
	repe cmpsw
	pop di
	jne @F
	mov al, 30h		; al = 30h to indicate found, ah = multiplex number
		; ! ds from ?signature_debugger same as for debuggerfunction
	mov word [debuggerfunction], ax
	mov word [bp + ?function], .checkus
				; next only check for our resident
@@:
	lds si, [bp + ?signature_us]
				; ds:si -> our AMIS name strings
	mov cx, 8		; Ignore description, only compare vendor and program name
	repe cmpsw
	je .found_from_both	; if match -->
.notfound:			; No match, try next
	retn

.found_from_both:
%if 0
		; Commented because we know that the debugger
		;  and our resident cannot both match the same
		;  multiplexer as the signatures differ. Uncomment
		;  if that is not true.

	call .foundus		; handle our resident
	cmp word [bp + ?function], .checkus
				; debugger matched for same multiplexer ?
	je .end_pop		; yes, do not continue search -->
	mov word [bp + ?function], .checkdebugger
				; next only check for debugger
	retn
%else
	mov word [bp + ?function], .checkdebugger
				; next only check for debugger
	jmp .foundus		; (tail call)
%endif


		; INP:	ah = multiplex number to check
		;	word [bp + ?function] -> .checkus
		; OUT:	returns if multiplex number unused or no signature match,
		;	 ah = multiplex number (unmodified)
		;	if resident instance of us found,
		;	 does not return and branches to .end instead
		;	 word [bp + ?frame_bp] = flags set appropriately
		;	 dword [bp + ?our_ctrl0] -> ctrl0 byte
		;	 byte [bp + ?our_multiplex_high + 1] = multiplex number
		;	if not found,
		;	 variables unmodified
		; CHG:	al, ds, si, es, di, dx, cx, bx
.checkus:
	mov al, 00h		; AMIS installation check
	int 2Dh			; AMIS (or "DOS reserved" = iret if no AMIS present)
	cmp al, 0FFh
	jne .notfound
	mov es, dx		; es:di -> name strings of AMIS multiplexer that just answered
	lds si, [bp + ?signature_us]
				; ds:si -> our AMIS name strings
	mov cx, 8		; Ignore description, only compare vendor and program name
	repe cmpsw
	jne .notfound

.found_from_us:
	call .foundus
	jmp .end_pop


		; INP:	ah = multiplex number to check
		;	word [bp + ?function] -> .checkdebugger
		; OUT:	returns if multiplex number unused or no signature match,
		;	 ah = multiplex number (unmodified)
		;	if debugger found,
		;	 does not return and branches to .end instead
		;	 word [debuggerfunction] = multiplex number high, 30h low
		;	if not found,
		;	 variables unmodified
		; CHG:	al, ds, si, es, di, dx, cx, bx
.checkdebugger:
	mov al, 00h		; AMIS installation check
	int 2Dh			; AMIS (or "DOS reserved" = iret if no AMIS present)
	cmp al, 0FFh
	jne .notfound
	lds si, [bp + ?signature_debugger]
				; ds:si -> debugger AMIS name strings
	mov es, dx		; es:di -> name strings of AMIS multiplexer that just answered
	mov cx, 8		; Ignore description, only compare vendor and program name
	repe cmpsw
	jne .notfound
	mov al, 30h		; al = 30h to indicate found, ah = multiplex number
		; ! ds from ?signature_debugger same as for debuggerfunction
	mov word [debuggerfunction], ax
	jmp .end_pop


.foundus:
	mov word [bp + ?our_ctrl0 + 2], dx
	mov word [bp + ?our_multiplex_high], ax
	mov al, 10h		; get compatible TSR version
	int 2Dh
	cmp al, 0FFh		; al = 0FFh if supported
	jne .unknown		; doesn't support this question -->
	cmp cx, TSR_VERC	; Compatible version?
	jne .unknown
	mov al, 20h		; Get TSR ctrl0
	int 2Dh
	cmp al, 00h		; al = size of returned ctrl data. at least one byte if supported
	jne .known		; if size ok, known -->
.unknown:
	setopt [bp + ?frame_bp], _incompatresident
				; indicate resident copy is incompatible
.known:
	setopt [bp + ?frame_bp], _foundresident
				; indicate resident copy found
	mov word [bp + ?our_ctrl0], bx
	retn

.end_pop:
	pop cx			; (discard near return address)

.end:

	pop ax			; pop from ?our_multiplex_high
	pop bx
	pop ds			; pop from ?our_ctrl0
				; set ds to our resident copy's segment
	lleave

			; ah = AMIS multiplex number of resident copy if bp&_foundresident (else 0FFh)
			; ds:bx-> ctrl0 of either resident copy or stored handler
process:
	rol byte [ss:j_flags.umb], 1
	jc @F
	or bp, _installlow
@@:

	test bp, _uninstall
	jz .dontuninstall
	rol byte [ss:old_j_switch], 1
	jnc @F
	test bp, _installlow
	jnz scancl.invalid	; old /J switch invalid if /u selected
@@:
	test bp, _installnew|_1set|_2set
	jnz scancl.invalid	; all invalid if /u selected
	jmp uninstall

.dontuninstall:
	test bp, _foundresident	; no resident found ?
	jz .install		; yes, install -->
	test bp, _installnew	; forced to install new ?
	jz .dontinstall		; no -->
	test bp, _onlystate
	jnz scancl.invalid	; invalid combination -->

.install:
	test bp, _onlystate	; don't install ?
	jz .installallowed
	mov dx, msg.notyet
	jmp short .abort_msg
.installallowed:
	jmp install

.dontinstall:
	rol byte [ss:old_j_switch], 1
	jnc .changeflags
	test bp, _installlow
	jz .changeflags
	mov dx, msg.already	; can't install new, already resident
	jmp short .abort_msg

.changeflags:
	test bp, _incompatresident
	jz .compatible
	mov dx, msg.incompatible ; can't change flags of incompatible version
.abort_msg:
	jmp abort_msg

.compatible:
	push ax
	call changeflags
	pop ax			; AMIS multiplex number
	mov al, 20h
	int 2Dh			; clear unsupported bits

		; Display status
	mov dx, msg.state1	; "state 1: "
	call disp_msg
	test byte [bx], 40h
	call onoff		; "ON (+)" or "OFF (-)" depending on ZF
	mov dx, msg.state2	; ", 2: "
	call disp_msg
	test byte [bx], 80h
	call onoff
	mov al, ah
	mov dx, msg.multiplex.1
	call disp_msg
	call disp_amisnum
	call restorestate
	mov ax, 4C00h
	int 21h

flagset:
	push cx
	rol cx, 1
	rol cx, 1
	or bp, cx
	pop cx
	retn


capitalise:
	cmp al, 'a'
	jb .notlowercase
	cmp al, 'z'
	ja .notlowercase
	and al, ~ 20h		; Uppercase if required
.notlowercase:
	retn


getnybble:
	cmp al, '0'
	jb .invalid		; unknown character -->
	cmp al, '9'
	jbe @F
	call capitalise
	cmp al, 'A'
	jb .invalid
	cmp al, 'F'
	ja .invalid
	sub al, 'A' - 10 - '0'
@@:
	sub al, '0'
	db __TEST_IMM8		; (NC, skip stc)
.invalid:
	stc
	retn


		; INP:	ds:si -> source IISP header (or pseudo header)
		;	es:di -> destination IISP header
		; OUT:	EI
		;	si and di both incremented by 6
		; CHG:	-
		; STT:	UP
update_iisp_header:
	push ax
	mov ax, word [cs:debuggerfunction]
	test ax, ax			; found the debugger ?
	jz @F				; no -->
	int 2Dh				; call its Update IISP Header function
	cmp al, 0FFh			; supported ?
	pop ax
	je .ret				; yes. done -->
	db __TEST_IMM8			; (skip pop)
@@:
	pop ax				; restore ax, then do manual update
	cli				; try to rest while updating chain
	cmpsw				; skip over first word (entrypoint)
					;  (generally xxEBh or 0EA90h)
	movsw
	movsw				; transfer source ieNext to dest ieNext
	sti
.ret:
	retn


		; INP:	al = interrupt number
		;	ds:si-> interrupt entry
		; OUT:	CY if unhooking failed
		;	NC if unhooking successful
		; CHG:	ah, es, di, si
UnhookInterrupt:
			; UnhookInterruptSim (below) only checks if it's possible to unhook this interrupt.
			; This function really unhooks the interrupt if possible.
			;
			; This is to cover the situation when some of the hooked interrupts can unhook,
			; but some can't. If the uninstaller would start to unhook the interrupts and then
			; catch the interrupt that can't be unhooked the user would end up with a dead TSR
			; that's uninstalled halfway. Very bad.
			;
			; "Simulating" the unhooking first and checking if all interrupts can unhook
			; usually will not return such a state.
	call UnhookInterruptSim
	jc .ret				; bad. --> (CY)
	jz .easy
.hard:
				; "hard" case: UnhookInterruptSim has however already done the work,
				; so the hard case is here indeed easier than the easy case.
	call update_iisp_header		; copies our stored pointer into the other's entry
	clc
	retn
.easy:
	push ds
	push dx
	lds dx, [ si + 2 ]		; get what we stored in the entry
	mov ah, 25h			; easy case - just reset to the value stored
	int 21h				; doesn't alter CF (leaves NC from UnhookInterruptSim) or sets NC
	pop dx
	pop ds
.ret:
	retn

		; INP:	ds:si-> IISP entry
		;	al = interrupt number
		; OUT:	NC if no error (either hard or easy case),
		;	 ZR if easy case,
		;	  ds:si-> our IISP entry, containing stored interrupt
		;	 NZ if hard case,
		;	  ds:si-> our IISP entry
		;	  es:di-> IISP entry to modify
		;	CY if error (not first handler and no IISP chain to this handler)
		; CHG:	ah, es, di
UnhookInterruptSim:
	push bx

	; harden this, check we are an IISP entry
	 push ds
	 pop es				; es => our handler segment
	mov bx, si			; es:bx -> our handler
	call IsIISPEntry?		; does it have an IISP header ?
	jne .fail			; fail if not

	mov ah, 35h			; get current vector
	int 21h				; es:bx-> current interrupt handler
	cmp si, bx			; our pointer ?
	jne .hard

	push ax
	push si
	mov si, ds
	mov ax, es
	 cmp si, ax			; our segment ?
	pop si
	pop ax
	jne .hard

	and ah, 00h			; NC, ZR
	pop bx
	retn
.hard:
		; INP:	ds:si-> IISP entry
		;	es:bx-> current interrupt entry
		; OUT:	CY if error
		;	NC, NZ if no error,
		;	 ds:si-> our IISP entry
		;	 es:di-> IISP entry to modify
		; CHG:	ah, es, di, (bx)
	call SearchIISPChain
	jne .harder
.found:				; found reference to our interrupt handler
	mov di, bx			; es:di-> IISP entry that references our's
	or ah, 0FFh			; NC, NZ
	pop bx
	retn

.harder:
	; Desperate attempt to find IISP entry that references ours by
	; searching through the interrupts hooked by other AMIS TSRs. Note
	; that the plexer loop will find and search through the list of
	; hooked interrupts of the uninstalling TSR itself, but this causes
	; no trouble.

		; INP:	ds:si-> IISP entry
		; OUT:	CY if error
		;	NC, NZ if no error,
		;	 ds:si-> our IISP entry
		;	 es:di-> IISP entry to modify
		; CHG:	ah, es, di, (bx)
	d4bp
	push cx
	push dx
	push ax				; register with interrupt number last
	xor ax, ax
.loopplex:
	mov al, 00h			; AMIS installation check
	int 2Dh				; enquire whether there's anyone
					;  but we don't care who it might be
	inc al
	jz .search
.nextplex:
	inc ah
	jnz .loopplex			; try next multiplexer -->
	pop ax
	pop dx
	pop cx
.fail:					; IISP incompatible TSR between current interrupt entry and our entry
					;  and no AMIS compatible TSR installed on top of our entry
	stc
	pop bx
	retn

		; INP:	ah = multiplex number of AMIS TSR to search through
		;	ss:sp-> interrupt number (byte), must be preserved
		; CHG:	es, di, dx, bx
.search:
	mov al, 04h
	pop bx
	push bx				; low byte is the interrupt number
	int 2Dh
	cmp al, 03h			; returned its interrupt entry ?
				; RBIL doesn't explicitly state that this interrupt entry has to
				; be IISP compatible. But I'm too lazy to look up the older AMIS,
				; and SearchIISPChain checks the interrupt entry anyway.
	je .search_dxbx
	cmp al, 04h			; returned list of hooked interrupts ?
	jne .nextplex			; no, try next multiplexer -->
	mov di, bx
	pop bx
	push bx				; bl = interrupt number
	mov al, bl
.search_intlist_seg:
	mov es, dx			; es:di-> list
.search_intlist:		; Search the returned list for the required interrupt number.
	scasb				; our interrupt number ?
	je .search_found_intlist
	cmp byte [es:di-1], 2Dh		; was last in list ?
	je .nextplex
	scasw				; skip pointer
	jmp short .search_intlist	; try next entry -->
.search_found_intlist:
	mov bx, word [es:di]		; dx:bx-> IISP entry
	scasw				; skip pointer
	call SearchIISPChain
	je .search_found		; found entry -->
		; This specific jump supports TSRs that hook the same
		; interrupt more than once; jumping to .nextplex instead
		; (as previously) aborts the search after the first match
		; in the interrupt list. This support might become useful.
	cmp al, 2Dh			; was last in list ?
	je .nextplex
	jmp short .search_intlist_seg

.search_dxbx:
	mov es, dx			; es:bx-> (IISP) interrupt entry
				; The entry we found now is possibly behind the non-IISP entry that
				; terminated our first SearchIISPChain call (at .hard). We then
				; possibly might find our entry in this hidden part of the chain.
	call SearchIISPChain
	jne .nextplex			; didn't find our entry in the chain -->
.search_found:
	pop ax
	pop dx
	pop cx
	jmp short .found


SearchIISPChain.next:
	les bx, [es:bx +2]		; get next interrupt entry

		; INP:	ds:si-> IISP entry
		;	es:bx-> current interrupt entry
		; OUT:	NZ if reference to ds:si not found in IISP chain es:bx->
		;	ZR if reference found,
		;	 es:bx-> IISP (or uninstalled iHPFS) interrupt entry with reference
		; CHG:	es, bx
SearchIISPChain:
	call IsIISPEntry?			; that an IISP entry ?
	jnz .return				; nope --> (NZ)
	cmp si, word [ es:bx + ieNext ]		; our pointer ?
	jne .next				; no, try next -->
	push ax
	mov ax, ds
	cmp ax, word [ es:bx + ieNext + 2]	; our segment ?
	pop ax
	jne .next				; no, try next -->
.return:					; yes, found (ZR)
	retn


		; If unhooking (or hooking) failed after the simulation succeeded for all
		; interrupts, at least one interrupt changed between the simulation and the
		; attempt to really (un)hook that interrupt. This is kind of a critical
		; error because it'll happen very rarely, and I'd be interested to hear
		; about software with which it does happen.
unhookerror:
	xor cx, cx			; (don't display critical failure message)
unhookerrorcritical:
	mov dx, msg.cantuninstall
.install:
	call disp_msg

	 pop dx
	cmp dx, byte -1			; at least another interrupt failed ? (at least two interrupts failed ?)
	 push dx
	je .single
	mov dx, msg.interrupts		; first message for multiple interrupts
	jmp short .first
.single:
	mov dx, msg.interrupt		; first message for single interrupt
.first:
	mov bx, dx

.loop:
	call disp_msg			; msg.interrupts or msg.hcomma
	call disp_al_hex
	pop ax				; get next value from stack
	 pop dx
	cmp dx, byte -1			; at least two more ?
	 push dx
	mov dx, msg.hcomma		; message if at least two more interrupts to display
	jne .notlast			; yes -->
	mov dx, msg.hand		; only one more interrupt to display
.notlast:
	cmp ax, byte -1			; last error marker?
	jne .loop			; no, keep looping -->

	mov dx, msg.hooked		; last message for single interrupt
	cmp bx, msg.interrupt		; was first message for single interrupt ?
	je .abort			; yes -->
	mov dx, msg.hookeds		; else use last message for multiple interrupts
.abort:
	jcxz .abort_msg			; not critical (or a critical installation failure) -->
	call disp_msg
	mov dx, msg.criticalunins	; else display critical failure message
.abort_msg:
	jmp abort_msg


uninstall:
	d4bp
	test bp, _foundresident
	jnz .found
	mov dx, msg.notinstalled	; nothing to uninstall there
.abort_msg:
	jmp abort_msg

.found:
	test bp, _incompatresident
	jz .compat
	mov dx, msg.incompatible	; resident, but incompatible version
	jmp short .abort_msg

.compat:
				; ds:bx-> ctrl0 of resident copy (unused yet)
				; ah = AMIS multiplex number of resident copy
	mov al, 02h
	mov dx, cs
	mov bx, .done			; dx:bx = return address if successful
	int 2Dh				; AMIS uninstall TSR
	cmp al, 0FFh			; 0FFh successful
	je .done			; TSR has already done everything -->
	test al, al			; 00h not implemented
	jz .continue_noseg		; do it myself -->
	cmp al, 03h			; 03h safe, no resident uninstaller (still enabled). bx=segment
	je .continue_seg		; expected -->
	cmp al, 04h			; 04h safe, no resident uninstaller (now disabled). bx=segment
	je .continue_seg		; unexpected, but continue -->
		; (other values: 01h unsuccessful or internal, 02h can't uninstall yet,
		; but will do so when able, 05h not safe, 06h,07h device driver; all fail)
.fail:
	mov dx, msg.cantuninstall	; some error occured
	call disp_msg
	mov dl, '.'
	mov ah, 02h
	int 21h				; display dot to end message
	mov dx, msg.crlf
.abort_msg_1:
	jmp short .abort_msg

.continue_noseg:
	xor bx, bx			; = 0

.continue_seg:
	push bx

	mov al, 04h
	mov bl, 2Dh			; Dummy, for the API. We only accept code 04h.
	int 2Dh				; AMIS determine hooked interrupts
	cmp al, 04h
	jne .fail		; General uninstallers should be prepared for at least 04h or 03h
				; as return code here.
	mov ds, dx			; ds:bx-> interrupt table
	push bx
	mov dx, UnhookInterruptSim
	mov cx, .simulated
	jmp short loopamisintr
.simulated:
	jc unhookerror
	pop bx				; restore ds:bx->

	mov dx, UnhookInterrupt
	mov cx, .unhooked
	jmp short loopamisintr
.unhooked:
	jc unhookerrorcritical		; (cx non-zero: display "critical error")

	push ds
	pop es				; es = segment of interrupt list
	pop ax				; stacked segment
	or ax, ax			; marker to use interrupt list segment ? (zero)
	jz .uselistsegment		;  yes -->
	mov es, ax			; else use segment returned by uninstall function

.uselistsegment:
	mov ah, 49h			; Free memory
	int 21h
				; General deinstallation code should not assume the TSR was just in
				; that single memory block. If the deinstallation code doesn't know
				; the TSR, it should search (and if it finds any, free) memory blocks
				; that have the MCB owner either that's now in es (code segment of TSR)
				; or the same MCB owner value that the TSR's code segment's MCB had.
				; (Only if the TSR's code segment was a valid memory block with MCB.)
	jc .fail			; if that causes an error, still report "fail"
					; (but interrupts are already unhooked now)

.done:
	mov dx, msg.removed
	call disp_msg			; "removed."
..@stack equ $ - (($-$$) % 2)		; (above is overwritten by stack while installing)
..@exit:				; terminate successful, re-used by some code (install too)
	call restorestate
	mov ax, 4C00h
	int 21h


abort_msg:
	call disp_msg
	call restorestate
	mov ax, 4CFFh
	int 21h
	int 20h


hookerror:
	mov cx, abort_msg
	; jmp short displayhookerror
displayhookerror:
	mov dx, msg.cantinstall
	call disp_msg

	 pop dx
	cmp dx, byte -1			; at least another interrupt failed ? (at least two interrupts failed ?)
	 push dx
	je .single
	mov dx, msg.interrupts		; first message for multiple interrupts
	jmp short .first
.single:
	mov dx, msg.interrupt		; first message for single interrupt
.first:
	mov bx, dx

.loop:
	call disp_msg			; msg.interrupts or msg.hcomma
	call disp_al_hex
	pop ax				; get next value from stack
	 pop dx
	cmp dx, byte -1			; at least two more ?
	 push dx
	mov dx, msg.hcomma		; message if at least two more interrupts to display
	jne .notlast			; yes -->
	mov dx, msg.hand		; only one more interrupt to display
.notlast:
	cmp ax, byte -1			; last error marker?
	jne .loop			; no, keep looping -->

	mov dx, msg.invalid		; last message for single interrupt
	cmp bx, msg.interrupt		; was first message for single interrupt ?
	je .ret				; yes -->
	mov dx, msg.invalids		; else use last message for multiple interrupts
.ret:
	jmp cx


		; INP:	ds:bx -> AMIS interrupt list
		;	dx = code called for each interrupt (with ds:si-> interrupt entry, al = interrupt number)
		;	cx = code to return to
		; OUT:	CY if any interrupt failed,
		;	 ax = first failed interrupt
		;	 ss:sp-> 0FFFFh terminated list of failed interrupts on stack
		;	NC if all interrupts successfully uninstalled
		; CHG:	si, ax, bx, what called code (dx) changes
		;
		; Called code (dx) has to preserve dx, bx, cx, al.
loopamisintr:

	mov ax, 0FFFFh
	push ax
.loop:
	mov al, byte [ bx ]
	mov si, word [ bx+1 ]

	call dx
	jnc .noerror			; no error -->
	xor ah, ah
	push ax				; else remember number of interrupt, but continue looping

	cmp dx, HookInterrupt
	je .abort			; if during hooking (!!), abort now -->

.noerror:
	add bx, byte 3
	cmp al, 2Dh
	jne .loop			; do until 2Dh done -->

.abort:
	pop ax
	cmp ax, byte -1			; if it's below (CY), there were errors
	jmp cx


changeflags:
				; bp = option flags
				; ds:bx-> ctrl0
	mov ax, bp
	test ah, _1set>>8
	jz .no1
	push ax
	and al, _state1
	and byte [bx], ~_state1
	or byte [bx], al
	pop ax
.no1:
	test ah, _2set>>8
	jz .no2
	and al, _state2
	and byte [bx], ~_state2
	or byte [bx], al
.no2:
	retn


install:		; ds = handler segment inside cs
			; bp = option flags
	d4bp
	mov ax, cs
	add ax, 10h+transient_size_p
	mov ds, ax		; ds = handler segment (could have changed with /n switch)

	mov bx, ctrl0		; ds:bx -> ctrl0
	call changeflags	; set flags from bp

			; This is a simple MS-DOS 1.x detection.
	rol byte [ss:j_flags.systemcheck], 1
	jnc .dosgood
	mov ah, 4Dh		; the only function I found that doesn't need much setup or cleanup
	stc			; but that uses MS-DOS 2.x error reporting. clears CF (Int21.30 does not)
	int 21h
	jnc .dosgood		; it is MS-DOS 2.x+ or compatible -->
	mov dx, msg.dosbad
	jmp abort_msg
.dosgood:

	xor ax, ax
	xchg ax, word [ cs:2Ch ]; set PSP field to zero
	mov es, ax
	mov ah, 49h
	int 21h			; Free our environment
	mov sp, ..@stack	; change to stack inside now unused code

	mov ah, 4Ah
	push cs
	pop es			; Resize the segment allocated to this program
	mov bx, 10h+transient_size_p+resident_size_p+msg_size_p	; 10h is the size of the PSP
%if _PSPRELOC
	push bx
%endif
	int 21h

%if _PSPRELOC
	xor bx, bx		; disable UMB link
	mov dx, 2		; last fit, low then high/low only
	call setstrat
	mov dx, msg.mcbcorrupted
	jc .abort_msg
	pop bx

	push bp			; preserve on stack to harden 21.4C call
	rol byte [ss:j_flags.psprelocate], 1
	jnc .no_psp_relocate

	; The next instruction tries to work around a bug in NTVDM's COMMAND.COM.
	; It is disabled in all actual releases. Just ignore it for now.
	; add bx, 0029h		; +64 KiB to try leaving command shell transient in memory
	mov ah, 48h
	stc
	int 21h			; allocate new memory for process
	jc .cantrelocate	; not possible, try to allocate resident -->
		; This interrupt 21h call should get the same stack as
		;  the function 4Ch call that relocates the process.
		;  In case we do not make a function 4B7Fh call and in
		;  case the function 335Dh call is not handled as usual
		;  then this is the last int 21h call before the terminate.

	mov dx, cs		; dx = old process
	mov es, ax		; newly allocated block
	mov ds, dx		; (no cs prefix to avoid an 8086 bug)
	xor di, di
	xor si, si
	mov cx, 80h+transient_size_w+resident_size_w+msg_size_w

			; This used to have a cs prefix, but it was removed
			;  to ensure proper operation on 8086 implementations
			;  with a bug when rep and another prefix occurred.
			; Instead ds is set to dx = cs, costing one byte.
	rep movsw		; copy process into newly allocated block

	call setmcb		; make the block own itself
				; ds = old PSP

	push ax			; new cs
	call retf_inst		; "push ip". the retf jumps to our new cs
.relocated:
	mov ss, ax		; change stack, too. sp is still valid!
%if 1
	mov es, ax		; es = new PSP
	mov di, 18h
	mov cx, 20
	mov word [ es:34h+2 ], ax
	mov word [ es:34h ], di	; fix the new PSP's PHT pointer
	mov word [ es:32h ], cx	;  and the count of PHT entries field
	 push ax
	mov al, -1
	 push di
	mov bx, cx		; = 20
	rep stosb		; initialise new PHT with empty entries
	 pop di
	mov cx, word [ 32h ]	; cx = count of PHT entries
	cmp cx, bx		; >= 20 ?
	jb .shortertable	; no -->
	mov cx, bx		; limit to 20
.shortertable:
	lds si, [ 34h ]		; ds:si-> old PHT
	push si
	push cx
	rep movsb		; get all entries
	pop cx
	pop di
	 push ds
	 pop es			; es:di-> old PHT
	rep stosb		; fill moved entries with -1 (closed)
	 pop ax
	mov ds, dx		; ds = old PSP
	mov word [ 0Ah ], .terminated
	mov word [ 0Ah+2 ], ax
	mov word [ 0Eh ], i23
	mov word [ 0Eh+2 ], ax
	mov word [ 12h ], i24
	mov word [ 12h+2 ], ax	; set interrupt vectors to ours
	mov word [ 16h ], ax	; set parent PSP to the relocated one
	mov word [ cs:2Eh+2 ], ax
				; set SS used by process termination

	xchg ax, bx		; bx = new location, dx = old location
	mov ax, 335Dh
	int 21h			; PSP relocated call-out

		; In order to set the correct stack address here,
		; the last Int21 call to a usual function (such as
		; Int21.48) must've been made with the same stack
		; pointer as the Int21.4C call below gets.
		;
		; Update: dosemu2 does weird things to the stack.
		;  In particular, it inserts an additional iret
		;  frame depending on some conditions.
		; Only the interrupt 21h subfunctions 00h, 26h,
		;  31h, 4Bh, and 4Ch are handled differently.
		;  As a workaround we can call service 4Bh as the
		;  last interrupt 21h function before terminating.
		; Refer to https://github.com/dosemu2/dosemu2/blob/d7402eec84478c051d25e7b26dd8515514c186e2/src/base/core/int.c#L1633-L1639

	rol byte [j_flags.invalidexec], 1
	jnc @F

	mov dx, .nullbyte	; just in case, ds:dx -> zero value byte
	mov ax, 4B7Fh		; 21.4B with invalid subfunction in al
				;  (note that FreeDOS masks off 80h)
	int 21h
@@:

	push word [ 2Eh ]
	pop word [ cs:2Eh ]	; set SP used by process termination
.nullbyte: equ $ - 1

	mov ax, 4C00h
	int 21h			; terminate, and make the new PSP active
				; also handles freeing all memory allocated to the old PSP
				; also closes any handles >20 if PHT larger
				; also relocates Int23, Int24
				; also notifies resident software old PSP is no longer valid
.terminated:			; (ax, bx, es, ds, bp might be changed)
	push cs
	pop ds			; reset ds
%else
	mov word [ cs:34h+2 ], ax	; fix up the PHT pointer
	xchg ax, bx
	mov ah, 50h
	int 21h			; set current PSP to new
				; bx = new PSP location
				; dx = old PSP location
	mov ax, 335Dh
	int 21h			; PSP relocated call-out

	push dx
	 push cs
	 pop ds
	mov dx, i23
	mov ax, 2523h		; set Int23 to relocated position
	int 21h
	mov dx, i24
	mov ax, 2524h		; set Int24 to relocated position
	int 21h
	pop es			; former process
	mov ah, 49h
	int 21h			; free memory of former process location
%endif
	mov ah, 1Ah
	mov dx, 80h
	int 21h			; set DTA

.no_psp_relocate:
.cantrelocate:		; We could display an error message here.
	mov bx, cs
	add bx, 10h+transient_size_p
	mov ds, bx		; ds = handler segment
	pop bp			; restore bp from stack after relocation
%endif

	test bp, _installlow	; try UMA installation ?
	jnz .allocatelow	; nope -->
	mov bx, 1		; enable UMB link
	mov dx, 41h		; best fit, high only strategy
	call setstrat
	jc .allocatelow

	mov ah, 48h
	mov bx, resident_size_p	; required size (paragraphs)
	stc
	int 21h			; try to allocate in upper memory
	jnc .allocated		; successfully allocated -->
	cmp ax, byte 8		; "not enough memory" ?
	je .allocatelow		; yes, retry in low memory -->
.corrupted:			; else: MCBs corrupted
	mov dx, msg.mcbcorrupted
.abort_msg:
	jmp abort_msg

.allocatelow:
	xor bx, bx		; disable UMB link
	mov dx, 1		; best fit, low then high/low only
	call setstrat
	mov dx, msg.mcbcorrupted
	jc .abort_msg

	mov ah, 48h
	mov bx, resident_size_p	; required size (paragraphs)
	stc
	int 21h			; try to allocate in low memory
	jnc .allocated		; successfully allocated -->
	cmp ax, byte 8		; "not enough memory" ?
	jne .corrupted		; no, assume corrupted chain -->
	mov dx, msg.nomemory	; else report no memory
	jmp short .abort_msg

.allocated:
			; ax = allocated memory block
			; ds = segment of resident (behind transient)
	mov es, ax
			; es = ax
	mov cx, (resident_size_p)*8
	xor di, di		; es:di-> start of allocated block
	xor si, si		; ds:si-> source inside this block
%if 0
	push cx
%endif
	rep movsw

	mov ax, word [cs:trymultiplexnumber]
	test al, al
	jnz @F			; if zero = 00h AMIS installation check
	int 2Dh
	test al, al		; still 00h if free
	jz .foundamis
@@:

	mov ah, 00h		; start with multiplex number 00h
.loopamis:
	mov al, 00h		; AMIS installation check
	int 2Dh
	test al, al		; still 00h if free
	jz .foundamis
	inc ah			; search from bottom to top
	jnz .loopamis		; loop if zero not yet reached -->
	mov dx, msg.noamisnumber; abort if no free AMIS multiplex number left
	jmp short .abort_msg

.foundamis:
			; ah = free AMIS multiplex number
	mov byte [ es:amisnum ], ah	; Set multiplex number

			; Check that all the interrupt handlers we'll chain to are valid.
			; We don't do these checks at run-time for speed and resident code
			; size reasons. Doing them here instead isn't perfect (since other
			; software could modify our IISP entry at run time to contain an
			; invalid address) but still is better than nothing.
	 push es
	 pop ds
	mov bx, amisintr	; ds:bx-> interrupt list of image
	mov dx, HookInterruptSim
	mov cx, .simulated
	push bx
	jmp short .loopamisintr	; do the hook simulation
.simulated:
	jc hookerror		; failed -->
	pop bx

			; All error conditions must be checked above because we'll now
			; modify the MCB so that it won't be deallocated when terminating.
	mov ax, es
	call setmcb
			; (Note that in case of critical hook failure, we still want the
			; handler to stay allocated in case some of the already hooked
			; interrupts can't be unhooked anymore. Otherwise, the code to
			; handle a nullified critical failure will free the MCB itself.)

	mov ds, ax		; restore ds !!
	; mov bx, amisintr	; ds:bx -> AMIS interrupt list (bx still valid)
	mov dx, HookInterrupt
	mov cx, .hooked
.loopamisintr:
	jmp loopamisintr	; really hook all interrupts in list
.hooked:
	jc .handle_crithookfailure

			; Now that everything is fine, display the final message.
	mov ax, ds		; ax = segment of resident copy
	cmp ax, word [ cs:firstumcb ]	; is that high ?
	mov dx, msg.highloadedusing
	ja .highloaded		; of course -->
	mov dx, msg.loadedusing	; no, other message

.highloaded:
	call disp_msg		; "[high ]loaded using " "xxxx bytes."
	mov ds, ax		; ds = allocated block
%if 0
 %if 0
	pop ax			; restore copied handler's size in words
	add ax, ax		; get size in byte
	add ax, byte 16		; add 16 for the MCB
 %else
	mov ax, resident_size +16	; +16 is for the MCB (most MEMs count it)
 %endif
	call disp_ax_dec	; "xxxx"
	mov dx, msg.byte
	call disp_msg		; " bytes."
%endif
	mov al, byte [amisnum]
	call disp_amisnum
	jmp ..@exit		; done if we ever were -->


		; Advanced Critical Failure Management(TM)
		;
		; In case of a critical installation failure (see docs and commentary),
		; we'll try to unhook all the interrupts that we successfully hooked.
		; If that succeeded, the whole TSR can simply be freed again and the
		; problem of the partway installed TSR has been nullified. This is still
		; considered a case to report, but isn't critical in itself because
		; there's no TSR lingering around.
.handle_crithookfailure:
	mov si, bx		; save address of failed interrupt's list entry
	mov cx, .critintdisplayed
	jmp displayhookerror
.critintdisplayed:
	call disp_msg		; display the last message (displayhookerror expects
				; to jump to abort_msg)
	mov cx, si		; restore address (don't use the stack here)

		; We now don't simulate the unhooking because we just want to unhook
		; these interrupts that will unhook no matter what. Additionally, it's
		; very likely that all the unhooking succeeds.
		;
		; This code uses an unrolled form of loopamisintr copied from there
		; because I just can't be bothered to add special checking for a
		; shorter list there. I also considered just calling loopamisintr
		; then filtering out the errors of the interrupts that we didn't hook,
		; but that's somewhat hacky too. So copied the code be.
	mov bx, amisintr
	mov ax, 0FFFFh
	cmp bx, cx
	je .nullifiedcrit	; the very first interrupt failed to hook, none to unhook -->
	push ax
.lai_loop:
	mov al, byte [ bx ]
	mov si, word [ bx+1 ]

	call UnhookInterrupt
	jnc .lai_noerror	; no error -->
	xor ah, ah
	push ax			; else remember number of interrupt, but continue looping
.lai_noerror:
	add bx, byte 3
	cmp bx, cx
	jne .lai_loop		; do until the next one would be the one that didn't hook -->

	pop ax
	cmp ax, byte -1		; if it's below (CY), there were errors
	jc .critical		; there were errors -->

		; One of the interrupts critically failed to hook, but we were able to
		; unhook everything again.
.nullifiedcrit:
	inc ax
	mov bx, ds
	dec bx
	mov ds, bx
	mov word [ 1 ], ax	; free the MCB

	mov dx, msg.criticalundone
	jmp abort_msg

		; One of the interrupts critically failed to hook. Additionally, at
		; least one other interrupt failed to unhook.
.critical:
	mov dx, msg.criticalins
	xor cx, cx
	jmp unhookerrorcritical.install	; this TSR be damned -->




		; This one works similar to UnhookInterruptSim. Here we check that
		; the interrupt currently contains a valid address since we want to
		; chain to the previous handler.
		;
		; INP:	ds:si-> IISP entry
		;	al = interrupt number
		; OUT:	NC if current interrupt handler valid, or we install a non-chaining handler
		;	CY if current interrupt handler invalid (offset FFFFh or segment 0000h)
		; CHG:	ah
HookInterruptSim:
	push es
	push bx

	; harden this, check we are an IISP entry
	 push ds
	 pop es				; es => our handler segment
	mov bx, si			; es:bx -> our handler
	call IsIISPEntry?		; does it have an IISP header ?
	jne .error			; fail if not

	test byte [ si + ieEOI ], 80h		; should be last handler ?
	jnz .valid				; yes, handler doesn't matter if we don't chain --> (NC)
	mov ah, 35h
	int 21h
	inc bx					; offset FFFFh ?
	jz .error
	mov bx, es
	test bx, bx				; or segment 0000h ?
	jnz .valid				; no, valid --> (NC)
.error:
	stc					; invalid, CY
.valid:
	pop bx
	pop es
	retn


		; INP:	ds:si-> IISP entry
		;	al = interrupt number
		; OUT:	NC if current interrupt handler valid, or we install a non-chaining handler
		;	CY if current interrupt handler invalid (offset FFFFh or segment 0000h),
		;	 though this SHOULD never happen
		; CHG:	ah, es
HookInterrupt:
	call HookInterruptSim
	jc .ret
	push bx
	push dx
	mov ah, 35h
	int 21h					; get current handler
	test byte [ si + ieEOI ], 80h		; should be last handler ?
	jz .notlast
	rol byte [ss:j_flags.hookbottom], 1
	jnc .notlast

		; If we are a last (non-chaining) handler
		;  then HookInterruptSim always shortcuts
		;  to succeeding. However, the family of
		;  IsIISPEntry? functions doesn't check
		;  for invalid handler addresses the same
		;  way as HookInterruptSim. (An offset of
		;  FFFFh happens to be rejected by both,
		;  but that is not true of a zero segment.)
		; If we were to install a chaining handler
		;  then the invalid address would be reason
		;  not to install. Consequently, when we
		;  install a last handler then it should
		;  not be installed into an IISP chain that
		;  is reachable from the first handler,
		;  the address of which was invalid.
		; Instead it should install atop the handler
		;  that has an invalid address. Therefore,
		;  check for a segment of zero here.
		; (This check could be added to the function
		;  IsIISPEntry? but it may be useful to have
		;  them otherwise allow addresses with a
		;  segment of zero.)
	push ax
	mov ax, es				; get the segment
	test ax, ax				; is it zero ?
	pop ax
	jz .notlast				; yes -->
	call IsChainingIISPEntry?		; is the first handler a chaining IISP ?
	jne .notlast				; no, can't hook below it -->
	db __TEST_IMM16				; (skip 2 pop ax)
.loop:
	pop ax
	pop ax					; discard last entry's address
	push es
	push bx
	les bx, [ es:bx + ieNext ]		; load next entry
	call IsChainingIISPEntry?		; this one also a chaining IISP ?
	je .loop
		; This write need not be atomic because
		;  the handler is not yet in the chain.
	mov word [ si + ieNext ], bx
	mov word [ si + ieNext + 2 ], es	; store address of next entry (non-IISP or non-chaining)
	pop bx
	pop es
		; The next write needs to be atomic,
		;  so build a header on the stack and
		;  call the update function here.
	push di
	mov dx, "NH"				; "New Handler"
	push dx					; fake IISP ieSignature
	push ds
	push si					; fake IISP ieNext
	mov dx, 0FEEBh				; "jmp short $"
	push dx					; fake IISP ieEntry
	 push ss
	 pop ds
	mov si, sp				; ds:si -> fake IISP header
	mov di, bx				; es:di -> IISP header to update
	call update_iisp_header			; store our handler's address in this chaining IISP entry
	pop dx					; discard
	pop si
	pop ds					; restore
	pop dx					; discard
	pop di					; restore
	jmp short .return

.notlast:
		; This write need not be atomic because
		;  only once we call 21.25 the handler
		;  is linked into the chain.
	mov word [ si + ieNext ], bx
	mov word [ si + ieNext + 2 ], es	; store
	mov ah, 25h
	mov dx, si				; ds:dx-> interrupt entry
	int 21h					; set new handler
.return:
	pop dx
	pop bx
	clc
.ret:
	retn


		; INP:	es:bx-> interrupt entry
		; OUT:	NZ if non-IISP entry,
		;	 or IISP entry that doesn't chain
		;	ZR if IISP entry that chains
IsChainingIISPEntry?:
	test byte [ es:bx + ieEOI ], 80h	; this one a non-chaining handler ? (or non-IISP)
	jnz IsIISPEntry?.return			; yes -->
						; otherwise fall through to check if really an IISP entry

		; INP:	es:bx-> interrupt entry
		; OUT:	NZ if non-IISP entry
		;	ZR if IISP entry
IsIISPEntry?:
	cmp bx, - (ieSignature + 2)		; may access word at offset FFFFh ?
	ja .return				; yes, avoid --> (NZ)
	cmp word [ es:bx + ieSignature ], "KB"	; "KB"/424Bh ? ("BK" in MASM)
	jne .return
	cmp word [ es:bx + ieEntry ], 0EA90h	; nop\jmp far imm16:imm16 ?
	je .return				; unused IISP entry (created by iHPFS) -->
	cmp byte [ es:bx + ieEntry ], 0EBh	; jmp short ... ?
		; (This opcode should strictly be jmp short $+18 but there's programs
		; that save an additional jmp opcode by jumping directly into their
		; code even though it's not right behind the header.)
	jne .return
	cmp byte [ es:bx + ieJmphwreset ], 0EBh	; jmp short ... ?
	je .return				; usual IISP entry -->
	cmp byte [ es:bx + ieJmphwreset ], 0CBh	; retf ?
	je .return				; a shorter variant -->
	cmp byte [ es:bx + ieJmphwreset ], 0CFh	; iret ?
.return:
	retn


		; INP:	ax = memory block
		; OUT:	es = MCB, name and owner set
		;	ds = cs
		; CHG:	di, si
setmcb:
	dec ax
	mov es, ax		; es = MCB of allocated block
	inc ax			; ax = allocated block!
	mov di, 8		; es:di-> MCB name field
	push cs
	pop ds
	mov si, msg.mcbname	; ds:si-> content for field
	movsw
	movsw
	movsw
	movsw			; Force MCB string
	mov word [ es:1 ], ax	; Set owner to itself
	retn


			; The DOS memory allocation strategy classes are actually "Low then high",
			; "High only" and "High then low". RBIL tells trash regarding that. We've
			; to disable the UMB link to insure only low memory is allocated. Normally,
			; the UMB link is already disabled when the program starts. But some
			; programs, like CMDEDIT 3.21, are buggy and leave the UMB link enabled
			; after they used UMBs.
		; INP:	bx = UMB link status to use
		;	dx = memory allocation strategy to use
		; OUT:	CF/ax error status
setstrat:
	mov ax, 5803h
	int 21h			; set UMB link
	jnc .linkdone		; no error
	cmp ax, byte 1		; "invalid function" ?
	stc
	jne .return		; no, actual error -->
	test bx, bx		; wanted to disable anyway ?
	stc
	jnz .return		; no, regard as error -->
.linkdone:
	mov ax, 5801h
	mov bx, dx
	int 21h			; set strategy
.return:
	retn


i24:
	mov al, 3			; always return fail, to handle the error as a soft one
	iret

i23:
	mov word [ cs:$ ], (__JMP_REL8|__REL16__(.return)<<8)	; don't reenter
	call restorestate
.return:
	stc				; always abort program (what default DOS handler also does)
retf_inst:
	retf

		; Restore modified DOS data
		;
		; CHG:	-
		; USE:	Int21
restorestate:
	push ax
	push bx
	mov bx, word [ cs:restored.strategy ]
	mov ax, 5801h
	int 21h				; restore strategy

	mov bx, word [ cs:restored.umblink ]
	mov ax, 5803h
	int 21h				; restore UMB link
	pop bx
	pop ax
	retn


disp_amisnum:
	call disp_al_hex
	mov dx, msg.multiplex.2
	jmp short disp_msg


onoff:
	mov dx, msg.off
	jz .off
	mov dx, msg.on
.off:				; fall through and display either
disp_msg:
	 push ds
	 push ax
	push cs
	pop ds
	mov ah, 09h
	int 21h
	 pop ax
	 pop ds
	retn


disp_al_hex:
		push cx
		mov cl, 4
		ror al, cl
		call .nibble
		ror al, cl
		pop cx
.nibble:
		push ax
		and al, 0Fh
		add al, '0'
		cmp al, '9'
		jbe .isdigit
		add al, 'A'-('9'+1)
.isdigit:
		xchg dl, al
		mov ah, 02h
		int 21h
		pop ax
		retn

%if 0
	; Following call: Display number in ax decimal
	; all registers preserved except dx
disp_ax_dec:			; ax (no leading zeros)
	; In: number in ax
	; Out: displayed
		push bx
		xor bx, bx
.pushax:
		push ax
.pushend:
		or bl, bl
		jz .nobl
		sub bl, 5
		neg bl
.nobl:
		push cx
		mov cx, 10000
		call .divide_out
		mov cx, 1000
		call .divide_out
		mov cx, 100
		call .divide_out
		mov cl, 10
		call .divide_out
							; (Divisor 1 is useless)
		add al, '0'
		xchg dl, al
		mov ah, 02h
		int 21h
		pop cx
		pop ax
		pop bx					; Caller's register
		retn

.divide_out:
	; In: ax = number
	;     cx = divisor
	; Out: ax = remainder of operation
	;      result displayed
		push dx
		xor dx, dx
		div cx				; 0:ax / cx
		push dx				; remainder
		dec bl
		jnz .nobl2
		or bh, 1
.nobl2:
		or bh, al
		jz .leadingzero
		add al, '0'
		xchg dl, al
		mov ah, 02h
		int 21h				; display result
 .leadingzero:
		pop ax				; remainder
		pop dx
		retn
%endif

	align 2, db 0
firstumcb:	dw 9FFFh			; guess of where UMBs start (A000h+)
trymultiplexnumber:
		dw -1		; low byte = 0 if set
				; high byte = multiplex number to try first
debuggerfunction:
		dw 0				; = 0 if unused
restored:
.umblink:	dw 0
.strategy:	dw 0

		; Format:
		; 1 byte switch name (end of table if 0FFh)
		; 1 word code to dispatch to
		; 1 word what to initialise cx to (0 if unused)
	align 2, db 0
parameters:
	db "1"
	dw scancl.set_in_bp, _state1
	db "2"
	dw scancl.set_in_bp, _state2
	db "O"
	dw scancl.set_in_bp, _onlystate
	db "J"
	dw scancl.switch_j, 0
	db "N"
	dw scancl.set_in_bp, _installnew
	db "X"
	dw scancl.multiplexnumber, 0
	db "U"
	dw scancl.set_in_bp, _uninstall
	db "R"
	dw scancl.set_in_bp, _uninstall
	db 0FFh			; table end marker
			; The help switches are not in this table (and neither in the option
			; flags) because on invalid switches, we'll show the help anyway.

j_options:
.:
	db "U"
	db "P"
	db "D"
	db "E"
	db "B"
	db "S"
.end:
.amount equ .end - .

j_flags:
.:
.umb:		db -1
.psprelocate:	db -1
.detectdebugger:db -1
.invalidexec:	db -1
.hookbottom:	db -1
.systemcheck:	db -1
.end:
.amount equ .end - .

%if .amount != j_options.amount
 %error Tables mismatch
%endif

old_j_switch:	db -1

	align 16
	endarea transient


	section RESIDENT vstart=0 align=16 follows=TRANSIENT
				; The handler is later copied into a seperate memory block.
				; The vstart= value tells NASM to use all addresses as if this
				; location is actually at vstart. (In this case, like if it's
				; at the start of the segment, not at 100h+transient_size.)
resident:

%include "resident.asm"

	align 16
	endarea resident

	section MESSAGE align=1 follows=RESIDENT
				; Putting the messages behind the handler is a kludge to omit
				; the disp_ax_dec function. We can only use the right _xdigits
				; macro conditionally if the handler's size is already known to
				; the preprocessor, i.e. the handler has been assembled.

msg:
.debuggeramissig:
	fill 8,32,db "ecm"	; vendor
	fill 8,32,db "lDebug"	; product
			; These strings are international.
.mcbname:	fill 8,0,db "TSR"
.name:		db "TSR example ",TSR_VERP_STR,": ",36
	%include "messages.asm"

	endarea msg

%if _UPXPAD && (transient_size + resident_size + msg_size) < 3062
	section UPXPADDING align=1 follows=MESSAGE
		times (3062 -(transient_size + resident_size + msg_size))/ 8 db "UPXPAD  "
		times (3062 -(transient_size + resident_size + msg_size))% 8 db 'U'
%endif
