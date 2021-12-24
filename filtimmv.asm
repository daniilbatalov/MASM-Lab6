			title		filetimemov
			assume		cs:code, ss:s, ds:d

print		macro		string
			lea			dx, string
			mov			ah, 09
			int			21H
			endm

gtlin		macro		mes, string
			print		mes
			mov			ah, 0AH
			lea			dx, string
			int			21H
			lea			di, string+2			;DI = first symbol of msg
			mov			al, -1[di]				;AL = number of read symbols
			xor			ah, ah
			add			di, ax
			mov			[di], ah				;set null-terminator
			endm

s			segment		stack
			dw			128 dup (?)
s			ends

d			segment		
string		db			255, 0, 255 dup (?)
errmsg		db			'Error! Invalid character!', 0DH, 0AH, '$'
negflag		dw			?
fname		db			255, 0, 255 dup (?)
inhan		dw			?
outhan		dw			?
buf			dw			?
dels		dw			?
first		dw			?
bytesread	dw			?
msg1		db			'Enter the name of input file: $'
msg2		db			'Enter the name of output file: $'
msgd		db			'Enter the delay of processing: $'
ermsgo		db			'The file was not opened!$'
ermsgc		db			'The file was not created!$'
ermsgr		db			'Error reading the file$'
ermsgw		db			'Error writing the file$'
msgred		db			'+$'
err4ah		db			'Error 4AH$'
err48h		db			'Error 48H$'
currentad	db			'Current address: $'
ok			db			'The program ended successfully!$'
d			ends

code		segment
old1c		dw			0,0
delay		dw			?
cr = 0DH
lf = 0AH

IntegerOut	proc
			xor			cx, cx
			mov			bx,	10
			cmp			ax, 0
			jge			m0
			neg			ax
			push		ax
			mov			ah,	2
			mov			dl,	'-'
			int			21H
			pop			ax

m0:			inc			cx
			xor			dx, dx
			div			bx
			push		dx
			or			ax, ax
			jnz			m0

m11:		pop 		dx
			add			dx, '0'
			mov			ah,	2
			int			21H
			loop		m11
			ret
IntegerOut	endp

IntegerIn	proc
startp:		push		dx
			push		si
			push		bx

			mov			ah, 0AH
			lea			dx, string
			int			21H

			xor			ax, ax
			lea			si, string+2
			mov			negflag, ax
			cmp			byte ptr [si], '-'
			jne			m2

			not			negflag
			inc			si
			jmp			m
m2:			cmp			byte ptr [si], '+'
			jne			m
			inc			si
m:			cmp			byte ptr [si], cr
			je			exl
			cmp			byte ptr [si], '0'
			jb			err
			cmp			byte ptr [si], '9'
			ja			err

			mov			bx, 10
			mul			bx

			sub			byte ptr [si], '0'
			add			al, [si]
			adc			ah, 0

			inc			si
			jmp			m

err:		lea 		dx, errmsg
			mov			ah, 9
			int			21H
			jmp			startp

exl:		cmp			negflag, 0
			je			ex
			neg			ax

ex:			pop			bx
			pop			si
			pop			dx
			
			ret
IntegerIn	endp

NewLine		proc
			push		ax
			push		dx

			mov			ah, 02H
			mov			dl, 0AH
			int			21H

			mov			ah, 02H
			mov			dl, 0DH
			int			21H

			pop			dx
			pop			ax
			ret
NewLine		endp	

start:		mov 		bx, seg z
			mov 		ax, es
			sub			bx, ax
			mov 		ah, 4AH
			int 		21H						;free the memory after the program

			jnc			freed
			mov			ax, d
			mov			ds, ax
			print		err4ah

bad_exit1:	mov 		ah, 4CH
			int 		21H	

freed: 		mov			ax, d
			mov			ds, ax
		

oread:		gtlin		msg1, fname
			mov			ah, 3DH
			lea			dx, fname+2
			xor			al, al					;AL = 0 => Access mode = read
			int			21H
			Call		NewLine
			jnc			mvinhnd					;check if carry flag was set - means there was an error
			print		ermsgo
			jmp			oread

mvinhnd:	mov			inhan, ax

cwrite:		gtlin		msg2, fname
			mov			ah, 3CH
			lea			dx, fname+2
			xor			cx, cx					;CX = 0 => not read-only, not hidden, not system, not a label, not a directory, not an archive
			int			21H
			Call		NewLine
			jnc			mvothnd
			print		ermsgc
			jmp			cwrite

mvothnd:	mov			outhan, ax

			mov			bx, 2					;allocate 2 paragraphs for buffer
			mov			ah, 48H
			int			21H

			jnc			bufer
			print		err48h
			jmp			bad_exit1

bufer:		mov			buf, ax	

delent:		print		msgd
			Call		IntegerIn
			Call		NewLine
			cmp			ax, 0
			jz			delent

			mov			dels, ax
			mov			cs:delay, ax

getint:		push		es
			mov			ax, 351CH				;get interrupt vector 1c
			int			21H
			mov			word ptr cs:old1c, bx
			mov			word ptr cs:old1c+2, es
			pop			es

			mov			first, 1				;

print_add:	print		currentad
			mov			ax, cs
			Call		IntegerOut
			Call		NewLine

			cmp			first, 0
			jnz			on_frt_run
			mov			ax, cs
			sub			ax, code
			add			ax, s
			push		ax						

on_frt_run:	push		cs						;if (m) STACK = old CS else STACK = old CS, old SS 
			mov			bx, seg z
			mov			ax, s
			sub			bx, ax					;BX = size from stack up to z segment
			mov			ah, 48H
			int			21H
			jc			no_mem_lft
			
			mov			es, ax
			mov			cl, 3
			shl			bx, cl					;BX = size of program in words

			mov			cx, bx
			xor			bp, bp					;copying the whole program from stack up to z seg


copy:		mov			ax, word ptr [bp]
			mov			word ptr es:[bp], ax
			add			bp, 2
			loop		copy

			mov			ax, es
			add			ax, code
			sub			ax, s
			lea			si, run_in_cpy			;start from the same offset but in new copy
			pushf
			push		ax
			push		si
			iret

run_in_cpy:	mov			cs:delay, 0

			mov			ax, cs
			sub			ax, code
			add			ax, d
			mov			ds, ax

			mov			ax, cs
			sub			ax, code
			add			ax, s
			mov			ss, ax					;setting SS and DS up

			mov			ax, 251CH
			lea			dx, tim					;set new interrupt 1c vector, interrupt manager in copy
			push		ds
			push		cs
			pop			ds						;25H ds has to be c segment 
			int			21H
			pop			ds						;get ds back

			pop			es						;ES = old CS
			mov			ax, es:delay
			add			cs:delay, ax			;copy the old delay

			cmp			first, 0
			jnz			read_han				

			pop			es						;ES = old SS (the first paragrah of an old copy)
			mov			ah, 49H
			int			21H						;free the old block
			jmp			read_han

no_mem_lft:	print		ok
			lea			si, close_han
			push		ss
			pushf
			push		cs
			push		si
			iret		

close_han:	mov			ax, d
			mov			ds, ax
			mov			ax, s
			mov			ss, ax

			pop			es
			mov			ah, 49H
			int			21H

			push		es
			mov			es, buf
			mov			ah, 49H
			int			21H
			pop			es			

			push		ds
			mov			dx, word ptr cs:old1c
			mov			ds, word ptr cs:old1c + 2
			mov			ax, 251CH
			int			21H
			pop			ds

bad_exit:	mov			ah, 3EH
			mov			bx, inhan
			int			21H

			mov			ah, 3EH
			mov			bx, outhan
			int			21H

			mov			ah, 4CH
			int			21H

aux_jmp:	jmp			print_add

read_han:	mov			bx, inhan
			mov			ah, 3FH
			push		ds
			mov			si, buf
			mov			ds, si
			xor			dx, dx
			mov			cx, 32
			int			21H
			pop			ds
			jnc			write_han
			print		ermsgr
			jmp			bad_exit

aux_jmp2:	jmp			aux_jmp	

write_han:	cmp			ax, 0
			jz			no_mem_lft
			mov			bytesread, ax

			mov			cx, bytesread
			push		es
			mov			es, buf
			xor			si, si


shift_l:	cmp			cs:delay, 0
			jg			shift_l

			mov			ax, dels
			add			cs:delay, ax

			print		msgred		
			cmp			byte ptr es:[si], 'A'
			jb			skip

			cmp			byte ptr es:[si], 'Z'
			ja			skip

			mov			al, 32
			add			al, byte ptr es:[si]
			mov			byte ptr es:[si], al

skip:		inc			si
			loop		shift_l

			pop			es
			Call		NewLine
			mov			cx, bytesread
			mov			ah, 40H
			mov			bx, outhan
			push		ds
			mov			si, buf
			mov			ds, si
			xor			dx, dx
			int			21H
			pop			ds
			mov			first, 0

			jnc			aux_jmp2
			print		ermsgw
			jmp			bad_exit

tim:		pushf
			call 		dword ptr cs:old1c		;call the old interrupt handler
			sti									;allow external interrupts
			dec			cs:delay
			iret
code		ends
z			segment
z			ends
			end			start