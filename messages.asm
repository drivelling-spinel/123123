%if 0

English messages for TSR example
 2020 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

.highloadedusing:db "high "
%if 0
.loadedusing:	db      "loaded using "
.byte:		db " byte"
%else
_autodigitsdef SIZESTR, handler_size_p*16 +16
.loadedusing:	db      "loaded using ",_SIZESTR," byte"
%undef _SIZESTR
%endif
.multiplex.1:	db ", on multiplex ",36
.multiplex.2:	db "h.",13,10,36
.notyet:
.notinstalled:	db "not installed.",13,10,36
.cantuninstall:	db "can't remove",36
.criticalunins:	db "CRITICAL removal failure (report!).",13,10,36
.criticalundone:db "Critical installation failure nullified (report!), "
                db "TSR fully removed.",13,10,36
.criticalins:	db "CRITICAL installation failure (report!)",36
.cantinstall:	db "can't install",36
.interrupts:	db ", interrupts ",36
.interrupt:	db ", interrupt ",36
.hand:		db "h and ",36
.hcomma:	db "h"
.comma:		db  ", ",36
.hookeds:
.hooked:	db "h hooked AMIS incompatible.",13,10,36
.invalids:
.invalid:	db "h invalid.",13,10,36
.incompatible:	db "incompatible version installed.",13,10,36
.dosbad:	db "error, bad DOS version detected.",13,10,36
.handlersbad:	db "error, invalid interrupt handlers. Use inst2d2f.",13,10,36
.already:	db "already installed.",13,10,36
.removed:	db "removed.",13,10,36
.nomemory:	db "out of memory.",13,10,36
.mcbcorrupted:	db "MCB chain corrupted.",13,10,36
.noamisnumber:	db "no free AMIS multiplex number.",13,10,36
.state1:	db "State 1 ",36
.state2:	db ", 2 ",36
.on:		db "ON (+)",36
.off:		db "OFF (-)",36
.help:          db "keyboard manipulation utility, AMIS v3.6 compliant.",13,10
		db "Optimal installation, advanced deinstallation method.",13,10
                db "Free software by ludicrous_peridot ",13,10
                db "Based on works by C. Masloch",13,10
		db "Options: (no)    Install or show state",13,10
		db "        /1{+|-}  1 state on/off, default ON",13,10
		db "        /2{+|-}  2 state on/off, default ON",13,10
		db "        /o       Only show or set state, don't install",13,10
		db "        /j       Install into low memory area (LMA)",13,10
		db "        /n       Install new even if already installed",13,10
		db "        /x=NN    Try multiplex number NNh first",13,10
		db "        /u, /r   Remove from memory",13,10
		db 13,10
		db "        /l=N     Number of keyboard buttons to manipulate (7 by default)",13,10
		db 13,10
		db "        /?, /h   This help message",13,10
		db 13,10
		db "The options /1, /2, /j and /n can be combined.",13,10
		db "The switch character - is also accepted."
.crlf:		db 13,10,36
