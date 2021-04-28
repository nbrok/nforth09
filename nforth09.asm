	cpu	6809
;************************************************************************
;* Nforth stands for Nick's Forth, it's a Forth interpreter created     *
;* from scratch for the Scrumpel 8 6809 Single Board Computer           *
;*                                                                      *
;* All the basic words are built in. If you like to make it a complete  *
;* compliant ANSI Forth, go ahead... but keep my name in the source!    *
;* Nforth is donated as public domain under the GNU Licence             *
;* Copyright 2021 by N. Brok nick@avboek.nl  Cross assembler version    *
;* Optimized for 6809, using user SP in stead of IX                     *
;************************************************************************

stack_ptr	equ	$7f00		;Change this to your free RAM location!
;Here the return-pointer is situated used by the subroutines used in this Forth.

dsp		equ	$7d00		;The same as above.
;This is the data stack-pointer, here you can find the user's data.

scdr		equ	$8301		;ACIA Data register
scsr		equ	$8300		;ACIA Status register
ledadres	equ	$8400		;PPORT leds and switches

cr		equ	$d
lf		equ	$a
bs		equ	$8

;* Flags used in the word length field (first 3 bits)

immed		equ	%10000000		;Immediate flag
compo		equ	%01000000		;Compile only flag
hide		equ	%00100000		;Smudge flag

ftrue		equ	-1
ffalse		equ	0

	org	$0100

entry0	sts	return			;Save old SP for bye
	jmp	cold			;Start Forth
entry1	sts	return			;Save old SP for bye
	jmp	warm			;Warm re-entry

;These are the system variables, this Forth is written for RAM-use.

line_buffer	rmb	128
return		rmb	2
numcnt		rmb	2
ibvar_flags	rmb	1
var_tib		rmb	2
var_cline	rmb	2
var_nchar	rmb	2
var_base	rmb	2
var_state	rmb	2
var_here	fdb	einde		;First free code space
var_link_end	fdb	link999		;Last link in vocabulary
var_chr_cnt	rmb	2
var_acpt_cnt	rmb	2
var_cnt1	rmb	1
sign_flag	rmb	1
var_numberh	rmb	2
var_numberl	rmb	2
var_position	rmb	2
var_counter	rmb	2
var_hp		rmb	2
ccount		rmb	1
scratch1	equ	500		;Scratchpad for find
scratch2	equ	scratch1+32	;Scratchpad for number

;*	Some 6809 register conventions used in Nforth:
;*	USP is used as the data stack pointer (DSP).
;*	SP is used as the return stack pointer (RSP).
;*	IY and IX are used for saving and manupulating addresses on
;*	the return stack.
;*	D is used as a general purpose 16 bits accumulator.

;* Nforth's dictionary structure:
;* The dictionary is formed by a linked list of the following data.
;*
;*	<Start-address of word's machinecode>		(16 bits)
;*	<link to a previous word>			(16 bits)
;*	<status+length of word>				(8 bits)
;*	The first 3 bits (b7-b5) are used for the word's status.
;*	the length of the word is defined in the last 5 bits.
;*	<Word's name> 			string[length of word] 
;*	The max wordlength is 512 bytes, long enough ;-)
;*	Begin of the word's machine code

	fdb	dolit
	fdb	0
link1	fcb	compo+5,"dolit"
dolit	puls	y			;Get address after jsr
	ldd	0,y++			;This is the litteral
	pshs	y			;New return address
	pshu	d			;Push it on data-stack
	rts

	fdb	dovar
	fdb	link1
link1a	fcb	5,"dovar"
dovar	puls	y			;Get address of variable
	pshu	y			;Push address on data-stack
	rts	

	fdb	docon
	fdb	link1a
link1b	fcb	5,"docon"
docon	puls	y			;Get address of constant
	ldd	0,y			;Get the constant
	pshu	d			;Push it on the data-stack
	rts

	fdb	call
	fdb	link1b
link1c	fcb	compo+4,"call"		;Push the opcode for a call
call	jsr	dolit			;instruction on Data Stack
	fdb	$bd
	rts

	fdb	var
	fdb	link1c
link1d	fcb	8,"VARIABLE"
var	jsr	create
	jsr	dolit
	fdb	0
	jmp	comma
		
	fdb	const
	fdb	link1d
link1e	fcb	8,"CONSTANT"
const	jsr	create
	jsr	dolit
	fdb	-3
	jsr	allot
	jsr	call
	jsr	ccomma
	jsr	dolit
	fdb	docon
	jsr	comma
	jmp	comma

	fdb	comcom
	fdb	link1e
link1f	fcb	immed+8,"COMPILE,"
comcom	jsr	call
	jsr	ccomma
	jmp	comma

	fdb	smudge			;This is an old word to disable
	fdb	link1f			;a word when during compliling an
link1g	fcb	6,"SMUDGE"		;error occured.
smudge	ldy	var_link_end
	lda	0,y
	ora	#hide			;Set hide-bit to on.
	sta	0,y
	rts

	fdb	usmudge			;This is an old word to make a word
	fdb	link1g			;visible.
link1h	fcb	8,"UNSMUDGE"
usmudge	ldy	var_link_end
	lda	0,y
	anda	#~hide			;Reset hide-bit to off.
	sta	0,y
	rts
	
	fdb	literal
	fdb	link1h
link1i	fcb	compo+7,"LITERAL"
literal	jsr	dolit
	fdb	dolit
	jsr	comcom
	jmp	comma

	fdb	branch
	fdb	link1i
link1j	fcb	compo+immed+6,"branch"
branch	jsr	dolit
	fdb	$7e			;Opcode for JMP
	jsr	ccomma
	jsr	here			;Address from jmp-address on
	jsr	dolit			;data-stack
	fdb	0
	jmp	comma

	fdb	qbranch
	fdb	link1j
link1k	fcb	compo+immed+7,"qbranch"
qbranch	jsr	dolit
	pulu	d			;The pulu D from datastack instruction
	jsr	comma
	jsr	dolit
	fdb	$1083			;Opcode for cmpd #$0
	jsr	comma
	jsr	dolit
	fdb	0			;The operand of the cpd instruction
	jsr	comma
	jsr	dolit
	fdb	$2603			;bne $+3
	jsr	comma
	jmp	branch

	fdb	backsl
	fdb	link1k
link1l	fcb	immed+1,$5c		;Create a comment
backsl	jsr	dolit
	fdb	lf
	jsr	_word			;Skip rest of line
	jmp	drop

	fdb	paren			;Create a comment
	fdb	link1l
link1m	fcb	immed+1,"("
paren	jsr	dolit
	fdb	")"
	jsr	_word			;Skip until ')' found
	jmp	drop

	fdb	ddrop
	fdb	link1m
link2	fcb	5,"2DROP"
ddrop	leau	2,u			;Drop RODS
drop	leau	2,u			;Drop TODS
	rts

	fdb	xdo
	fdb	link2
link2a	fcb	4,"(DO)"
xdo	puls	y			;Save returnaddress on RP
	ldd	#$8000
	subd	2,u
	addd	0,u			;Get operand
	pshs	b
	pshs	a			;On return stack
	ldd	#$8000
	subd	2,u
	pshs	b
	pshs	a
	pshs	y			;Return address back on return stack
	leau	4,u			;USP:=USP+4 (Forth : 4 USP @ + USP !)
	rts

	fdb	xloop
	fdb	link2a
link2b	fcb	6,"(LOOP)"
xloop	tfr	s,y			;Get RP into IY
	ldd	4,y
	addd	#1
	bvs	exit_loop
	std	4,y
	rts
	
exit_loop

	ldd	0,y			;This is the return address
	puls	y			;Bump it from stack
	puls	y			;Bump xdo parameters
	puls	y			;This was the xdo return loop!
	exg	d,y
	leay	3,y			;Skip the jump
	jmp	0,y			;return

	fdb	ploop
	fdb	link2b
link2b1	fcb	7,"(+LOOP)"
ploop	tfr	s,y			;Get RP into IY
	ldd	4,y
	addd	0,u
	bvs	exit_loop1
	std	4,y
	leau	2,u
	rts

exit_loop1

	leau	2,u
	bra	exit_loop

	fdb	_hex
	fdb	link2b1
link2c	fcb	3,"HEX"
_hex	jsr	dolit
	fdb	16			;Set base in HEX
	jsr	_base
	jmp	store

	fdb	_dec
	fdb	link2c
link2d	fcb	7,"DECIMAL"
_dec	jsr	dolit
	fdb	10			;Set base to decimal
	jsr	_base
	jmp	store

	fdb	do
	fdb	link2d
link2e	fcb	immed+2,"DO"
do	jsr	dolit
	fdb	xdo
	jsr	comcom
	jmp	here
	
	fdb	loop
	fdb	link2e
link2f	fcb	immed+4,"LOOP"
loop	jsr	dolit
	fdb	xloop
	jsr	comcom
	jsr	branch	
	jmp	store

	fdb	loopp
	fdb	link2f
link2f1 fcb	immed+5,"+LOOP"
loopp	jsr	dolit
	fdb	ploop
	jsr	comcom
	jsr	branch
	jmp	store
	
	fdb	i
	fdb	link2f1
link2g	fcb	1,"I"
i	tfr	s,y
	ldd	4,y
	subd	2,y
	pshu	d
	rts

	fdb	j
	fdb	link2g
link2h	fcb	1,"J"
j	tfr	s,y
	ldd	8,y
	subd	6,y
	pshu	d
	rts

	fdb	_if
	fdb	link2h
link2i	fcb	immed+2,"IF"
_if	jmp	qbranch

	fdb	_then
	fdb	link2i
link2j	fcb	immed+4,"THEN"
_then	jsr	here
	jsr	swap
	jmp	store

	fdb	_else
	fdb	link2j
link2k	fcb	immed+4,"ELSE"
_else	jsr	branch
	jsr	swap
	jsr	here
	jsr	swap
	jmp	store

	fdb	_begin
	fdb	link2k
link2l	fcb	immed+5,"BEGIN"
_begin	jmp	here

	fdb	_again
	fdb	link2l
link2m	fcb	immed+5,"AGAIN"
_again	jsr	branch
	jmp	store

	fdb	_until
	fdb	link2m
link2n	fcb	immed+5,"UNTIL"
_until  jsr	qbranch
	jmp	store

	fdb	_repeat
	fdb	link2n
link2o	fcb	immed+6,"REPEAT"
_repeat	jsr	swap
	jsr	_again
	jmp	_then

	fdb	_while
	fdb	link2o
link2p	fcb	immed+5,"WHILE"
_while	jmp	qbranch

	fdb	keyq
	fdb	link2p
link2q	fcb	4,"KEY?"		;Check if key was pressed
keyq	ldb	scsr
	bitb	#%00000001
	beq	keyq1
	lda	scdr
	ldd	#ftrue
keyq2	pshu	d
	rts
keyq1	clra
	clrb
	bra	keyq2

	fdb	bye
	fdb	link2q
link2r	fcb	3,"BYE"
bye	lds	return			;Return to original caller
	rts

	fdb	exec
	fdb	link2r
link2s	fcb	4,"EXEC"
exec	pulu	d
	exg	d,x
	jsr	0,x
	jmp	warm

	fdb	reset
	fdb	link2s
link2t	fcb	5,"RESET"
reset	jsr	dolit
	fdb	$FFFE
	jsr	at
	jmp	exec

	fdb	drop
	fdb	link2t
link3	fcb	4,"DROP"

	fdb	dup
	fdb	link3
link4	fcb	3,"DUP"
dup	ldd	0,u			;Get TODS
	pshu	d			;It's duplicated now
	rts

	fdb	ddup
	fdb	link4
link4a	fcb	4,"2DUP"
ddup	jsr	over
	jmp	over

	fdb	emit
	fdb	link4a
link5	fcb	4,"EMIT"
emit	pulu	d			;Get TODS

emit_loop

ot	lda	scsr			;Get status of serial I/O
	bita	#2
	beq	emit_loop		;Wait till empty
	stb	scdr			;Transmit character
	rts

	fdb	key
	fdb	link5
link6	fcb	3,"KEY"
key	bsr	in			;Get character
	pshu	d			;Push it on data-stack
	rts
in	lda	scsr			;Get status of serial I/O
	bita	#1
	beq	in			;Wait till ready
	clra
	ldb	scdr			;Get the character
	rts

	fdb	_led
	fdb	link6
link6a	fcb	3,"LED"
_led	jsr	docon			;Push PPORT address on stack
	fdb	ledadres		;This is the address of PPORT

	fdb	ledsto
	fdb	link6a
link6b	fcb	4,"LED!"
ledsto	jsr	_led
	jmp	cstore			;Store it into PPORT

	fdb	ledat
	fdb	link6b
link6c	fcb	4,"LED@"
ledat	jsr	_led
	jmp	cat

	fdb	spat
	fdb	link6c
link7	fcb	3,"SP@"
spat	pshs	u			;Save DSP temporary
	exg	d,u
	puls	u
	pshu	d			;DSP onto data-stack
	rts

	fdb	spstor
	fdb	link7
link8	fcb	compo+3,"SP!"
spstor	ldu	0,u			;Init DSP
	rts

	fdb	rpstor
	fdb	link8
link9	fcb	compo+3,"RP!"
rpstor	puls	y
	lds	0,u			;Init RP
	leau	2,u
	pshs	y
	rts

	fdb	tor
	fdb	link9
link10	fcb	2,">R"
tor	puls	y			;Save return address
	pulu	d			;Get TODS
	pshs	b
	pshs	a
	jmp	0,y			;This is your return address

	fdb	fromr
	fdb	link10
link11	fcb	2,"R>"
fromr	puls	y			;Get return address
	puls	a
	puls	b			;Get TORP
	pshu	d			;Push onto data-stack
	jmp	0,y			;This is your return address

	fdb	rat
	fdb	link11
link11a	fcb	2,"R@"
rat	puls	y			;Get return address
	puls	a			;Get TORP
	puls	b
	pshs	b			;Put it back on TORP
	pshs	a
	pshu	d			;This is the top value on RP
	jmp	0,y			;This is the return address

	fdb	less
	fdb	link11a
link11b	fcb	1,'<'
less	ldd	0,u++
	cmpd	0,u
	bgt	lesst
lessf	ldd	#ffalse
lessc	std	0,u
	rts
lesst	ldd	#ftrue
	bra	lessc

	fdb	equal
	fdb	link11b
link11c	fcb	1,'='
equal	jsr	sub
	bra	zequal

	fdb	nequal
	fdb	link11c
link11c1
	fcb	2,'<>'
nequal	ldd	0,u++
	cmpd	0,u
	bne	lesst
	bra	lessf
	
	fdb	great
	fdb	link11c1
link11d	fcb	1,'>'
great	jsr	swap
	bra	less

	fdb	zequal
	fdb	link11d
link12	fcb	2,"0="
zequal	ldd	0,u			;Get TODS
	cmpd	#0
	beq	is_zero

is_false

	ldd	#ffalse
onstk	std	0,u
	rts
is_zero	ldd	#ftrue
	bra	onstk			;Push flag on Data-stack

	fdb	zless
	fdb	link12
link12a	fcb	2,"0<"
zless	ldd	0,u			;Get TODS
	cmpd	#0
	bmi	is_zero
	bra	is_false

	fdb	_max
	fdb	link12a
link12b	fcb	3,"MAX"
_max	jsr	ddup
	jsr	less
	pulu	d
	cmpd	#0
	beq	_mx1
	jsr	swap
_mx1	jmp	drop

	fdb	_min
	fdb	link12b
link12c	fcb	3,"MIN"
_min	jsr	ddup
	jsr	great
	pulu	d
	cmpd	#0
	beq	_mn1
	jsr	swap
_mn1	jmp	drop

	fdb	_or
	fdb	link12c
link13	fcb	2,"OR"
_or	pulu	d			;Get first operand
	ora	0,u			;Logical or with second operand
	orb	1,u
	std	0,u			;Result on data stack
	rts

	fdb	_and
	fdb	link13
link14	fcb	3,"AND"
_and	pulu	d			;Get first operand
	anda	0,u			;Logical and with second operand
	andb	1,u
	std	0,u			;Result on data stack
	rts

	fdb	_xor
	fdb	link14
link15	fcb	3,"XOR"
_xor	pulu	d			;Get first operand
	eora	0,u			;Logical xor with second operand
	eorb	1,u
	std	0,u			;Result on data stack
	rts

	fdb	_invert
	fdb	link15
link15a	fcb	6,"INVERT"
_invert	jsr	dolit			;Push -1 on datastack
	fdb	-1
	jmp	_xor			;Do a XOR so the cell is inverted

	fdb	charpl
	fdb	link15a
link16	fcb	5,"CHAR+"
charpl	ldd	0,u			;Get TODS
	addd	#1			;Increment it
	std	0,u			;Result on data-stack
	rts

	fdb	charm
	fdb	link16
link16a	fcb	5,"CHAR-"
charm	ldd	0,u			;Get TODS
	subd	#1			;Decrement it
	std	0,u			;Result on data-stack
	rts

	fdb	cellpl
	fdb	link16a
link16b	fcb	5,"CELL+"
cellpl	ldd	0,u
	addd	#2
	std	0,u
	rts

	fdb	cellm
	fdb	link16b
link16c	fcb	5,"CELL-"
cellm	ldd	0,u
	subd	#2
	std	0,u
	rts

	fdb	plus
	fdb	link16c
link17	fcb	1,'+'
plus	pulu	d			;Get first operand
	addd	0,u
	std	0,u
	rts

	fdb	dplus
	fdb	link17
link17a	fcb	2,"D+"			;32 bit addition
dplus	ldd	2,u
	addd	6,u
	std	6,u
	ldd	0,u
	adcb	5,u
	adca	4,u
	std	4,u
	leau	4,u
	rts

	fdb	dmin
	fdb	link17a
link17b	fcb	2,"D-"			;32 bit substraction
dmin	ldd	6,u
	subd	2,u
	std	6,u
	ldd	4,u
	sbcb	1,u
	sbca	0,u
	std	4,u
	leau	4,u
	rts

	fdb	dnegate
	fdb	link17b
link17c	fcb	7,"DNEGATE"		;Negate 32 bits
dnegate	clra
	clrb
	leau	-2,u
	std	0,u
	leau	-2,u
	std	0,u
	jsr	dswap
	jmp	dmin

	fdb	dabs
	fdb	link17c
link17d	fcb	4,"DABS"		;Remove sign from 32 bit word
dabs	jsr	ddup
	jsr	swap
	jsr	drop
	jsr	zless
	ldd	0,u
	leau	2,u
	cmpd	#0
	beq	dabs1
	jsr	dnegate
dabs1	rts

	fdb	oneplus
	fdb	link17d
link17e	fcb	2,"1+"			;Increment by one
oneplus	ldd	0,u
	addd	#1
	std	0,u
	rts

	fdb	twoplus
	fdb	link17e
link17f	fcb	2,"2+"			;Increment by two
twoplus	ldd	0,u
	addd	#2
	std	0,u
	rts

	fdb	onemin
	fdb	link17f
link17g	fcb	2,"1-"			;Decrement by one
onemin	ldd	0,u
	subd	#1
	std	0,u
	rts

	fdb	twomin
	fdb	link17g
link17h	fcb	2,"2-"			;Decrement by two
twomin	ldd	0,u
	subd	#2
	std	0,u
	rts

	fdb	twodiv
	fdb	link17h
link17i	fcb	2,"2/"			;Shift right
twodiv	ldd	0,u
	andcc	#$fe
	lsra
	rorb
	std	0,u
	rts

	fdb	twomul
	fdb	link17i
link17j	fcb	2,"2*"			;Shift left
twomul	ldd	0,u
	andcc	#$fe
	lslb
	rola
	std	0,u
	rts

	fdb	sub
	fdb	link17j
link18	fcb	1,'-'			;16 bits substraction
sub	ldd	2,u                     ;Get first operand
	subd	0,u                     ;Do the substraction
	std	2,u                     ;Store result
	leau	2,u			;Remove old value
        rts

	fdb	negate			;Negate 16 bits
	fdb	link18
link18a	fcb	6,"NEGATE"
negate	ldd	#0
	subd	0,u			;Two's complement item on data-stack
	std	0,u
	rts


	fdb	abs
	fdb	link18a
link18b	fcb	3,"ABS"			;Remove sign from 16 bits
abs	jsr	dup
	jsr	zless
	ldd	0,u++
	cmpd	#0
	beq	abs1
	jsr	negate
abs1	rts

	fdb	over
	fdb	link18b
link19	fcb	4,"OVER"
over	ldd	2,u			;Get second value on data-stack
	pshu	d
	rts

	fdb	swap
	fdb	link19
link20	fcb	4,"SWAP"
swap	ldd	0,u			;Get TODS
swap1	ldy	2,u			;Get second value
	std	2,u			;Exchance it on data-stack
	sty	0,u
	rts

	fdb	dover
	fdb	link20
link20a	fcb	5,"2OVER"
dover	jsr	tor
	jsr	tor
	jsr	ddup
	jsr	fromr
	jsr	fromr
	jmp	dswap

	fdb	dswap
	fdb	link20a
link20b	fcb	5,"2SWAP"
dswap	ldd	0,u
	ldy	4,u
	std	4,u
	sty	0,u
	ldd	2,u
	ldy	6,u
	std	6,u
	sty	2,u
	rts

	fdb	rot
	fdb	link20b
link20c	fcb	3,"ROT"
rot	jsr	tor
	jsr	swap
	jsr	fromr
	jmp	swap
	
	fdb	at
	fdb	link20c
link21	fcb	1,'@'
at	ldy	0,u			;Get address from TODS
	ldd	0,y			;Get value at this address
	std	0,u			;Put in on data_stack
	rts

	fdb	dat
	fdb	link21
link21a	fcb	2,"2@"
dat	jsr	dup
	jsr	cellpl
	bsr	at
	jsr	swap
	bra	at

	fdb	dstore
	fdb	link21a
link21b	fcb	2,"2!"
dstore	jsr	swap
	jsr	over
	jsr	store
	jsr	cellpl
	jmp	store

	fdb	cat
	fdb	link21b
link22	fcb	2,"C@"
cat	ldy	0,u			;Get address from TODS
	clra
	ldb	0,y			;Get the byte
	std	0,u			;On data-stack
	rts

	fdb	store
	fdb	link22
link23	fcb	1,'!'
store	pulu	d			;Get address from TODS
	exg	d,y
	pulu	d			;Get the data from TODS
	std	0,y			;Store it
	rts


	fdb	pstore
	fdb	link23
link23a	fcb	2,"+!"
pstore	pulu	d
	exg	d,y
	ldd	0,y
	addd	0,u++
	std	0,y
	rts

	fdb	cstore
	fdb	link23a
link24	fcb	2,"C!"
cstore	pulu	d			;Get address from TODS
	exg	d,y
	pulu	d			;Get the data
	stb	0,y			;Store that byte
	rts

	fdb	do_cr
	fdb	link24
link25	fcb	2,"CR"			;Print CRLF
do_cr	ldd	#lf
	pshu	d
	ldd	#cr
	pshu	d
	jsr	emit
	jmp	emit

	fdb	dotp
	fdb	link25
link25a	fcb	immed+2,".("
dotp	jsr	dolit			;Print the string directly.
	fdb	$29			;until )
dotp1	jsr	_word
	jsr	count
	jmp	_type

	fdb	dotq
	fdb	link25a
link25b	fcb	immed+2,'."'
dotq	ldd	var_state		;Check the state.
	bne	dotq1			;Compiling? Then compile it.
	jsr	dolit			;Otherwise print the string directly.
	fdb	$22			;until "
	bra	dotp1			;"Steal" some code from .(
dotq1	jsr	dolit
	fdb	dotstr
	jsr	comcom
	jsr	dolit
	fdb	$22
	jsr	_word
	jsr	dup
	jsr	cat
	jsr	charpl
	jsr	here
	jsr	swap
	jsr	dup
	jsr	tor
	jsr	cmove
	jsr	fromr
	jmp	allot

	fdb	space
	fdb	link25b
link26	fcb	5,"SPACE"		;Print a space
space	jsr	dolit
	fdb	$20
	jmp	emit

	fdb	spaces
	fdb	link26
link26a	fcb	6,"SPACES"
spaces	pulu	d			;Get TODS

spaces_loop

	pshs	a			;Save count-value on RP 
	pshs	b
	jsr	dolit
	fdb	$20
	jsr	emit			;Send a space.
	puls	b			;Get it back
	puls	a
	subd	#1
	bne	spaces_loop
	rts

	fdb	_base
	fdb	link26a
link27	fcb	4,"BASE"
_base	ldd	#var_base
	pshu	d
	rts

	fdb	umstar
	fdb	link27
link27a	fcb	3,"UM*"			;16*16=32 multiply
umstar	bsr	ustars
	leau	2,u
	pshu	d
	rts

ustars	lda	#16
	sta	ccount
	clra
	clrb
ustar2	ror	2,u
	ror	3,u
	dec	ccount
	bmi	ustar4
	bcc	ustar3
	addd	0,u
ustar3	rora
	rorb
	bra	ustar2
ustar4	rts

	fdb	ummslash
	fdb	link27a
link27b	fcb	6,"UM/MOD"

ummslash

	lda	#17
	sta	ccount
	ldd	2,u
usl1	cmpa	0,u
	bhi	usl3
	bcs	usl2
	cmpb	1,u
	bcc	usl3
usl2	andcc	#$fe
	bra	usl4
usl3	subd	0,u
	orcc	#1
usl4	rol	5,u
	rol	4,u
	dec	ccount
	beq	usl5
	rolb
	rola
	bcc	usl1
	bra	usl3
usl5	pshu	d
	leau	4,u
	jmp	swap1

	fdb	uslash
	fdb	link27b
link27c	fcb	2,"U/"
uslash	ldd	2,u
	ldx	0,u
	cmpx	#0
	beq	div0error
	bsr	idiv
	exg	d,x
	leau	2,u
	std	0,u
	rts

idiv	std	dvdad
	stx	dvsad
	bsr	divide
	ldx	dvdad
	ldd	remad
	rts

	fdb	divmod
	fdb	link27c
lnk27c1	fcb	4,"UMOD"		;Unsigned divide
divmod	ldd	2,u
	ldx	0,u
	cmpx	#0
	beq	div0error
	bsr	idiv
	leau	2,u
	std	0,u
	rts

divide	lda	#16			;Software replacement of IDIV
	sta	counad
	clra
	clrb
divd	asl	dvdad+1
	rol	dvdad
	rolb
	rola
	cmpd	dvsad
	blo	nosub
	subd	dvsad
	inc	dvdad+1
nosub	dec	counad
	bne	divd
	std	remad
	rts

dvdad	rmb	2
dvsad	rmb	2
counad	rmb	1
remad	rmb	2

div0error

	jsr	dotstr
	fcb	21,"Division by 0 error",cr,lf
	jmp	abort

	fdb	slash
	fdb	lnk27c1
link27d	fcb	1,'/'
slash	jsr	ddup
	jsr	_xor
	jsr	tor
	jsr	abs
	jsr	swap
	jsr	abs
	jsr	swap
	jsr	uslash
	jsr	fromr
	jsr	zless
	ldd	,u++
	cmpd	#0
	beq	sls1
	jsr	negate
sls1	rts

	fdb	_mod
	fdb	link27d
link27e	fcb	3,"MOD"			;Get modulo of division
_mod	jsr	ddup
	jsr	_xor
	jsr	tor
	jsr	abs
	jsr	swap
	jsr	abs
	jsr	swap
	jsr	divmod
	jsr	fromr
	jsr	zless
	ldd	,u++
	cmpd	#0
	beq	sls1
	jsr	negate
	bra	sls1

	fdb	star
	fdb	link27e
link27f	fcb	1,'*'
star	jsr	ustars			;Signed multiply
	leau	2,u
	rts

	fdb	udmod
	fdb	link27f
link27g	fcb	6,"UD/MOD"
udmod	jsr	tor
	jsr	dolit
	fdb	0
	jsr	rat
	jsr	ummslash
	jsr	rot
	jsr	rot
	jsr	fromr
	jsr	ummslash
	jmp	rot

	fdb	udstar
	fdb	link27g
link27h	fcb	3,"UD*"
udstar	jsr	dup
	jsr	tor
	jsr	umstar
	jsr	drop
	jsr	swap
	jsr	fromr
	jsr	umstar
	jsr	rot
	jmp	plus

	fdb	pad
	fdb	link27h
link27i	fcb	3,"PAD"
pad	jsr	here
	jsr	dolit
	fdb	scratch2
	jmp	plus

	fdb	hp
	fdb	link27i
link28	fcb	2,"HP"
hp	ldd	#var_hp
	pshu	d
	rts

	fdb	stod
	fdb	link28
link28a	fcb	3,"S>D"
stod	jsr	dup
	jmp	zless

	fdb	hold
	fdb	link28a
link28b	fcb	4,"HOLD"
hold	jsr	dolit
	fdb	-1
	jsr	hp
	jsr	pstore
	jsr	hp
	jsr	at
	jmp	cstore

	fdb	lessnum
	fdb	link28b
link28c	fcb	2,"<#"
lessnum	jsr	pad
	jsr	hp
	jmp	store

	fdb	todigit
	fdb	link28c
link28d	fcb	6,">digit"
todigit	jsr	dup
	jsr	dolit
	fdb	9
	jsr	great
	jsr	dolit
	fdb	7
	jsr	_and
	jsr	plus
	jsr	dolit
	fdb	$30
	jmp	plus

	fdb	number
	fdb	link28d
link28e	fcb	1,'#'
number	jsr	_base
	jsr	at
	jsr	udmod
	jsr	rot
	jsr	todigit
	jmp	hold

	fdb	numbers
	fdb	link28e
link28f	fcb	2,"#S"
numbers	jsr	number
	jsr	ddup
	jsr	_or
	ldd	0,u++
	cmpd	#0
	bne	numbers
	rts

	fdb	grtnum
	fdb	link28f
link28g	fcb	2,"#>"
grtnum	jsr	ddrop
	jsr	hp
	jsr	at
	jsr	pad
	jsr	over
	jmp	sub

	fdb	sign
	fdb	link28g
link28h	fcb	4,"SIGN"
sign	ldd	0,u++
	cmpd	#0
	bpl	signend
	jsr	dolit
	fdb	$2d
	jsr	hold
signend	rts

	fdb	udot
	fdb	link28h
link28i	fcb	2,"U."
udot	jsr	lessnum
	jsr	dolit
	fdb	0
dott1	jsr	numbers
dott	jsr	grtnum
	jsr	_type
	jmp	space

	fdb	dot
	fdb	link28i
link28j	fcb	1,'.'
dot	jsr	lessnum
	jsr	dup
	jsr	abs
	jsr	dolit
	fdb	0
	jsr	numbers
	jsr	rot
	jsr	sign
	bra	dott

	fdb	uddot
	fdb	link28j
link28k	fcb	3,"UD."
uddot	jsr	lessnum
	bra	dott1

	fdb	ddot
	fdb	link28k
link28l	fcb	2,"D."
ddot	jsr	lessnum
	jsr	ddup
	jsr	dabs
	jsr	numbers
	jsr	rot
	jsr	sign
	jsr	grtnum
	jsr	_type
	jsr	space
	jmp	drop

	fdb	_type
	fdb	link28l
link29	fcb	4,"TYPE"
_type	ldd	0,u
	cmpd	#0
	beq	type_exit
	ldy	2,u
	leau	4,u

type_loop

	pshs	a
	pshs	b
	ldb	0,y
	jsr	emit_loop
	leay	1,y
	puls	b
	puls	a
	subd	#1
	bne	type_loop

type_exit

	rts

	fdb	here
	fdb	link29
link30	fcb	4,"HERE"
here	ldd	var_here
	pshu	d
	rts

	fdb	dotstr
	fdb	link30
link30a	fcb	compo+4,"(.$)"
dotstr	puls	y
	lda	0,y+				;Get length of string
	tsta
	beq	dotstr_exit			;If count equals 0 then do nothing!
ps_loop	ldb	0,y+				;Get character
	pshs	a
	jsr	ot
	puls	a
	deca					;Got All?
	bne	ps_loop				;No? Get next one

dotstr_exit

	pshs	y				;Here is the code, so push it back
	rts

	fdb	prompt
	fdb	link30a
link31	fcb	6,"prompt"
prompt	ldd	var_state
	cmpd	#0
	bne	nopmpt
	jsr	dotstr
	fcb	7," .ok.",cr,lf
nopmpt	rts

_bs	jsr	dotstr
	fcb	3,bs,' ',bs
	rts

	fdb	execute
	fdb	link31
link31a	fcb	7,"EXECUTE"
execute	ldy	0,u++
	jmp	0,y

	fdb	count
	fdb	link31a
link31b	fcb	5,"COUNT"
count	ldy	0,u
	ldb	0,y+
	clra
	sty	0,u
	pshu	d
	rts
		
	fdb	accept
	fdb	link31b
link32	fcb	6,"ACCEPT"
accept	pulu	d			;Get count from TODS
	ldy	0,u			;Get line-buffer address
	std	var_acpt_cnt
	clra
	clrb
	std	var_chr_cnt

accept_loop

	jsr	in			;Get a character	
	cmpb	#bs
	bne	acc00
	cmpy	0,u
	beq	accept_loop		;Begin of line reached?
	leay	-1,y
	pshs	y
	jsr	_bs
	puls	y
	ldd	var_chr_cnt
	subd	#1
	std	var_chr_cnt
	ldd	var_acpt_cnt
	addd	#1
	std	var_acpt_cnt
	bra	accept_loop
acc00	jsr	ot
	cmpb	#cr
	beq	end_of_accept
	stb	0,y+
	ldd	var_chr_cnt
	addd	#1
	std	var_chr_cnt
	ldd	var_acpt_cnt
	subd	#1
	std	var_acpt_cnt
	beq	end_of_accept
	bra	accept_loop

end_of_accept

	ldb	#cr
	stb	0,y
	ldb	#lf
	stb	1,y
	ldd	var_chr_cnt
	addd	#1
	std	0,u			;Length of string on TODS
	jmp	do_cr

	fdb	_word
	fdb	link32
link33	fcb	4,"WORD"
_word	pulu	d			;Get character
	ldy	var_tib			;Get buffer address
	pshs	b
	ldb	var_position+1		;Get curent character position
	exg	x,y
	abx				;Adjust IY
	exg	x,y
	puls	b
	clr	var_cnt1		;#char=0

_word_loop

	lda	0,y			;Eat char
	pshs	b
	cmpa	,s+
	bne	_word_ex1		;Exit when done
	leay	1,y
	inc	var_position+1
	bra	_word_loop
 
_word_ex1

	pshs	y
	pshs	u
	pshs	b
	pshs	a
	ldd	var_here		;Get first free byte
	addd	#scratch1		;Offset defined in scratch1
	exg	d,u
	puls	a
	puls	b
	pshs	u

_word_loop1

	lda	0,y
	pshs	b
	cmpa	,s+
	beq	_word_ex3
	cmpa	#cr
	beq	_word_ex2
	sta	1,u			;Copy character into buffer
	leau	1,u
	inc	var_cnt1
	leay	1,y
	bra	_word_loop1

_word_ex3

	inc	var_position+1

_word_ex2

	puls	u
	ldb	var_cnt1
	stb	0,u
	exg	d,u			;Beginaddress of counted string in D
	puls	u
	puls	y
	leau	-2,u
	std	,u			;Begin of word in buffer
	ldb	var_cnt1
	addb	var_position+1
	stb	var_position+1		;Pointer points at end of word+1
	rts

	fdb	abort
	fdb	link33
link33a	fcb	5,"ABORT"
abort	lds	#stack_ptr
	ldu	#dsp
	clra
	clrb
	std	var_state
	jmp	quit

	fdb	toin
	fdb	link33a
link33b	fcb	3,">IN"
toin	ldd	#var_position
	pshu	d
	rts

	fdb	shptib
	fdb	link33b
link33c	fcb	4,"#TIB"
shptib	ldd	#var_nchar
	pshu	d
	rts

	fdb	allot
	fdb	link33c
link33d	fcb	5,"ALLOT"
allot	ldd	0,u++
	addd	var_here
	std	var_here
	rts

	fdb	comma
	fdb	link33d
link33e	fcb	1,','
comma	jsr	dolit
	fdb	2
	jsr	allot
	jsr	here
	jsr	cellm
	ldy	0,u++
	ldd	0,u++
	std	0,y
	rts

	fdb	ccomma
	fdb	link33e
link33f	fcb	2,"C,"
ccomma	jsr	dolit
	fdb	1
	jsr	allot
	jsr	here
	jsr	charm
	ldy	0,u++
	ldd	0,u++
	stb	0,y
	rts

	fdb	create
	fdb	link33f
link33g	fcb	6,"CREATE"
create	jsr	here			; HERE CELL+ CELL+ BL WORD
	jsr	cellpl
	jsr	cellpl
	jsr	blank
	jsr	_word			
	pshs	u
	ldy	0,u
	ldu	2,u
	lda	0,y
	sta	0,u
	leay	1,y
	leau	1,u
crealp	ldb	0,y+
	stb	0,u+
	deca
	bne	crealp
	exg	y,u
	puls	u
	jsr	ddrop
	leau	-2,u
	sty	0,u
	jsr	comma
	ldd	var_link_end
	leau	-2,u
	std	0,u
	jsr	comma
	jsr	here
	ldd	0,u
	std	var_link_end
	jsr	cat
	jsr	charpl
	jsr	allot
	jsr	call
	jsr	ccomma
	jsr	dolit
	fdb	dovar
	jmp	comma

	fdb	semis
	fdb	link33g
link33h	fcb	2,";S"
semis	jsr	dolit
	fdb	$39
	jmp	ccomma

	fdb	np
	fdb	link33h
link33i	fcb	2,"NP"
np	ldd	#var_link_end
	pshu	d
	rts

	fdb	cp
	fdb	link33i
link33j	fcb	2,"CP"
cp	ldd	#var_here
	pshu	d
	rts

	fdb	find
	fdb	link33j
link34	fcb	4,"FIND"
find	ldy	0,u++			;Get begin of word from TODS
	ldx	var_link_end		;Get end of voc-link
	ldb	0,y+			;Get length of word

find_loop

	lda	0,x
	sta	ibvar_flags		;Save the flags for later
	pshs	a
	lda	0,x
	anda	#hide
	cmpa	#hide
	beq	next_find1a		;Smudge on? Yes then skip word

	puls	a
	anda	#%00011111		;Mask flags for real length
	pshs	b
	cmpa	,s+
	bne	next_find1
	pshs	b
	pshs	x
	pshs	y
	leax	1,x

find_loop1

	lda	0,x
	cmpa	0,y
	bne	next_find
	leax	1,x
	leay	1,y
	decb
	bne	find_loop1
found	puls	y
	puls	x
	puls	b
	leax	-4,x
	ldy	0,x			;Get code-begin address
	lda	ibvar_flags		;Get the flags
	bita	#immed			;Immediate bit set?
	beq	not_immediate
	ldd	#1
fcnt1	leau	-2,u
	sty	0,u			;Execute address on data stack
	leau	-2,u
	std	0,u			;Flag on data stack
	rts

not_immediate

	ldd	#-1
	bra	fcnt1

next_find

	puls	y
	puls	x
	puls	b

next_find1

	leax	-2,x
	ldx	0,x
	cmpx	#0			;End of link reached?
	bne	find_loop		;No, then continue search
	exg	d,x
	leau	-2,u
	leay	-1,y
	sty	0,u			;Store address of counted string
	leau	-2,u
	std	0,u
	rts

next_find1a

	puls	a
	bra	next_find1

get_number

	clr	sign_flag		;No sign in default
	jsr	count			;Get begin of string and its count
	clra
	clrb
	pshu	d
	pshu	d			;Clear double for tonumber
	jsr	dswap
	ldy	2,u			;Get address of string
	lda	0,y
	cmpa	#'-'			;Check for sign
	bne	get_cont		;No sign continue else
	lda	#$ff
	sta	sign_flag		;Sign is on
	leay	1,y
	sty	2,u			;Restore new address
	ldd	0,u
	subd	#1
	std	0,u			;One character less...

get_cont

	jsr	tonumber		;Convert it
	ldy	2,u
	ldd	0,u			;Get #chars left
	beq	normal_integer		;=0 then no double word
	lda	0,y
	cmpa	#'.'			;Was it ment as a double?
	bne	error			;This means error
	ldd	0,u
	subd	#1
	bne	error			;Check if there is only one char left
	jsr	drop			;Drop count
	jsr	drop			;Drop ADDRESS
	lda	sign_flag
	bne	is_negatived
	rts

is_negatived

	jmp	dnegate

normal_integer

	ldd	0,u
	bne	error
	jsr	drop
	jsr	drop
	jsr	drop			;Drop high word of double.
	lda	sign_flag
	bne	is_negative
	rts

is_negative

	jmp	negate

error	jsr	dotstr
	fcb	7,"Error",cr,lf
	jmp	abort

get_digit

;* Convert an ASCII digit into a nummerical value

	pshs	b			;Save ACCB
	lda	0,y			;Get the character
	cmpa	#$30			;Is it less then 0x30?
	bcs	no_digit1		;Then it's an invalid digit
	cmpa	#$3a			;Is it more then 0x30 and less then 0x3a?
	bcc	no_digit		;No then check for A-Z
	anda	#$f			;Yes then mask lowest nibble
	tfr	a,b			;Move ACCB to ACCA (Extend to 16 bits)
	clra
	cmpd	var_base		;Is result within given BASE?
	bcc	no_digit1		;No then it is not valid
	tfr	b,a			;Get ACCB back into ACCA
	puls	b			;Get saved ACCB
	leay	1,y			;Point to next character
	andcc	#$fe			;No error so clear carry
	rts

no_digit

	cmpa	#$41			;Now check if digit is in A-Z
	bcs	no_digit1		;Less then 0x41 then invalid digit
	cmpa	#'Z'+1			;Within A-Z then valid digit
	bcc	no_digit1		;Not valid so ERROR
	suba	#$37			;Make a nice number from it
	tfr	a,b			;Extend to 16 bits
	clra
	cmpd	var_base		;Is result within given BASE?
	bcc	no_digit1		;No then invalid digit so ERROR
	tfr	b,a			;Get ACCB back
	puls	b			;Get saved ACCB
	leay	1,y			;Point to next character
	andcc	#$fe			;No error so clear carry
	rts

no_digit1

	puls	b			;Restore saved ACCB
	orcc	#1			;Error so set Carry flag
	rts

	fdb	tonumber
	fdb	link34
link34a	fcb	7,">NUMBER"

tonumber

	ldd	0,u			;Length of the string
	std	numcnt			;Save it
	ldy	2,u			;Begin of the string
	leau	4,u			;length & stringaddress pulled from DSP

tonumber_loop

	jsr	get_digit
	bcs	tonumber_end
	tfr	a,b
	clra
	pshs	y
	pshu	d
	jsr	stod
	jsr	dswap
	jsr	_base
	jsr	at
	jsr	udstar
	jsr	dplus
	puls	y
	ldd	numcnt
	subd	#1
	std	numcnt
	bhi	tonumber_loop

tonumber_end

	leau	-2,u
	sty	0,u
	ldd	numcnt
	pshu	d
	rts

	fdb	blank
	fdb	link34a
link35	fcb	2,"BL"
blank	ldd	#$20
	pshu	d
	rts

	fdb	qstack
	fdb	link35
link35a	fcb	6,"?STACK"
qstack	tfr	u,x
	cmpx	#dsp+1
	bcc	stack_err
	rts

stack_err

	jsr	dotstr
	fcb	12," underflow",cr,lf
	jmp	abort

hidden	puls	a
	bra	hidden1

	fdb	_words
	fdb	link35a
link36	fcb	5,"WORDS"
_words	ldy	var_link_end

mn_loop1

	ldb	#10			;Ten words on a line...
mn_loop	pshs	b
	lda	0,y
	pshs	a
	anda	#hide			;Don't show the word if smudge set
	cmpa	#hide
	beq	hidden
	puls	a
	anda	#%00011111		;Mask for real length
	pshs	y
	leay	1,y

dsp_word_loop

	ldb	0,y
	pshs	a
	jsr	ot
	leay	1,y
	puls	a
	deca
	bne	dsp_word_loop
	jsr	space
	puls	y
hidden1	leay	-2,y
	ldy	0,y
	puls	b
	cmpy	#0
	beq	mn_ex
	decb
	bne	mn_loop
	jsr	do_cr
	bra	mn_loop1	
mn_ex	jmp	do_cr

	fdb	_true
	fdb	link36
link36a	fcb	4,"TRUE"
_true	ldd	#-1
	pshu	d
	rts

	fdb	_false
	fdb	link36a
link36b	fcb	5,"FALSE"
_false	ldd	#0
	pshu	d
	rts

	fdb	tib
	fdb	link36b
link37	fcb	3,"TIB"
tib	ldd	var_tib
	pshu	d
	rts

	fdb	c_line
	fdb	link37
link37a	fcb	6,"C/LINE"
c_line	ldd	var_cline
	pshu	d
	rts

	fdb	semi
	fdb	link37a
link37b	fcb	immed+1,';'
semi	jsr	semis
	jsr	usmudge
	jmp	lbrack

	fdb	cmpi
	fdb	link37b
link37c	fcb	1,':'
cmpi	jsr	create
	jsr	smudge
	jsr	dolit
	fdb	-3
	jsr	allot
	jmp	rbrack
	
	fdb	quit
	fdb	link37c
link38	fcb	4,"QUIT"
quit	clra
	clrb
	std	var_position
	jsr	prompt
	jsr	tib
	jsr	c_line
	jsr	accept
	jsr	shptib
	jsr	store
quitl1	jsr	blank	
	jsr	_word
	ldy	0,u
	lda	0,y
	tsta
	bne	quitl2
	jsr	drop
	bra	quit
quitl2	jsr	find
	ldd	0,u++
	cmpd	#0
	beq	quitl3
	cmpd	#1
	beq	quitl5
	ldd	var_state
	cmpd	#0
	bne	quitcom
quitl5	jsr	execute
	jsr	qstack
	bra	quitl1
quitl3	jsr	get_number
quitl4	jsr	qstack
	ldd	var_state
	cmpd	#0
	beq	quitl1
	jsr	literal
	bra	quitl1
quitcom	jsr	comcom
	jsr	qstack
	bra	quitl1

	
	fdb	warm			;1=immediate -1=not immediate	
	fdb	link38
link39	fcb	4,"WARM"
warm	lds	#stack_ptr		;Init Return Pointer
	ldu	#dsp			;Init Data-stack Pointer
	clra
	clrb
	std	var_state		;Interprete-mode is default
	jsr	_hi
	jmp	quit

	fdb	_hi
	fdb	link39
link40	fcb	2,"HI"
_hi	jsr	dotstr
;		   1  2   34567890123456789012345678901234567890123456789012  3  4
	fcb	84,cr,lf,"Nforth version 1.0 for Scrumpel 8 using MC6809 CPU",cr,lf
	fcb	"(C) 2021 by N. Brok (PE1GOO)",cr,lf
;		 5678901234567890123456789012  3  4
	rts

	fdb	tick
	fdb	link40
link41	fcb	1,"'"
tick	jsr	blank
	jsr	_word
	jsr	find
	ldd	0,u++
	cmpd	#0
	beq	error_tick
	rts

error_tick

	jsr	dotstr
	fcb	11,"Not found",cr,lf
	jmp	abort

	fdb	quest
	fdb	link41
link42	fcb	1,'?'
quest	jsr	at
	jmp	dot

	fdb	state
	fdb	link42
link43	fcb	5,"STATE"
state	ldd	#var_state
	pshu	d
	rts

	fdb	lbrack
	fdb	link43
link44	fcb	immed+1,'['
lbrack	jsr	dolit
	fdb	0
	jsr	state
	jmp	store

	fdb	rbrack
	fdb	link44
link45	fcb	1,']'
rbrack	jsr	dolit
	fdb	-1
	jsr	state
	jmp	store

	fdb	_immed
	fdb	link45
link46	fcb	9,"IMMEDIATE"
_immed	ldy	var_link_end
	lda	0,y
	ora	#immed
	sta	0,y
;	bset	0,y,immed
	rts

	fdb	cmove
	fdb	link46
link47	fcb	5,"CMOVE"
cmove	ldd	0,u++
	std	var_counter
	ldd	0,u++
	ldy	0,u++
	exg	d,x

cmove_loop

	lda	0,y+
	sta	0,x+
	ldd	var_counter
	subd	#1
	std	var_counter
	bne	cmove_loop
	rts

	fdb	postpone
	fdb	link47
link48	fcb	immed+8,"POSTPONE"

postpone

	jsr	blank
	jsr	_word
	jsr	find
	ldd	0,u++
	cmpd	#0			;Not Found?
	bne	pstcnt
	jmp	error_tick		;Error!

pstcnt	cmpd	#1			;Is it immediate?
	beq	yeimm			;Yes? Then compile as immediate

noimm	jsr	literal
	jsr	dolit
	fdb	comcom
	jmp	comcom

yeimm	jsr	literal
	jsr	dolit
	fdb	execute
	jmp	comcom

	fdb	cold
	fdb	link48
link49	fcb	4,"COLD"		
cold	lds	#stack_ptr		;Init Return Pointer
	ldu	#dsp			;Init Data-stack Pointer
	ldd	#10
	std	var_base		;Init base on decimal
	ldd	#80
	std	var_cline		;Init CHAR/LINE constant
	ldd	#line_buffer		;Init TIB constant
	std	var_tib
	ldd	#link999
	std	var_link_end		;Init last link
	ldd	#einde
	std	var_here		;Init first free space
	jmp	warm			;Execute warm

	fdb	S0
	fdb	link49
link50	fcb	2,"S0"
S0	ldd	#dsp
	pshu	d
	rts

	fdb	qdup
	fdb	link50
link51	fcb	4,"?DUP"
qdup	jsr	dup
	pulu	d
	cmpd	#0
	bne	qdup1
	bra	qdup2
qdup1	jsr	dup
qdup2	rts

	fdb	depth
	fdb	link51
link52	fcb	5,"DEPTH"
depth	jsr	S0
	jsr	spat
	jsr	sub
	jsr	twodiv
	jsr	dolit
	fdb	1
	jmp	sub

	fdb	dots
	fdb	link52
link53	fcb	2,".S"
dots	jsr	depth
	jsr	qdup
	jsr	zequal
	pulu	d
	cmpd	#0
	beq	dots1
	jsr	dotstr
	fcb	15,"Stack is empty!"
	jmp	do_cr
dots1	jsr	dotstr
	fcb	14,"Data on stack:"
	jsr	dolit
	fdb	0
	jsr	xdo
dotslp	jsr	s0
	jsr	dolit
	fdb	2
	jsr	sub
	jsr	i
	jsr	dolit
	fdb	2
	jsr	star
	jsr	sub
	jsr	at
	jsr	do_cr
	jsr	dotstr
	fcb	11,"---------> "
	jsr	dot
	jsr	xloop
	jmp	dotslp
	jmp	do_cr

;	fdb	dots
;	fdb	link52
;link53	fcb	2,'.S'
;dots	jsr	depth
;	jsr	dup
;	jsr	dolit
;	fdb	$3c
;	jsr	emit
;	jsr	space
;	jsr	dot
;	jsr	dolit
;	fdb	$3e
;	jsr	emit
;	jsr	qdup
;	jsr	zequal
;	pulu	d
;	cmpd	#0
;	beq	dots1
;	jmp	do_cr
;dots1	jsr	dolit
;	fdb	0
;	jsr	xdo
;dotslp	jsr	S0
;	jsr	dolit
;	fdb	2
;	jsr	sub
;	jsr	i
;	jsr	dolit
;	fdb	2
;	jsr	star
;	jsr	sub
;	jsr	at
;	jsr	space
;	jsr	dot
;	jsr	xloop
;	jmp	dotslp
;	rts

	fdb	qbase
	fdb	link53
link54	fcb	5,"?BASE"
qbase	jsr	_base
	jsr	at
	jsr	dup
	jsr	dolit
	fdb	10
	jsr	_base
	jsr	store
	jsr	dot
	jsr	_base
	jmp	store

	fdb	doth
	fdb	link54
link55	fcb	4,".HEX"
doth	jsr	_base
	jsr	at
	jsr	_hex
	jsr	swap
	jsr	dolit
	fdb	0
	jsr	lessnum
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	grtnum
	jsr	_type
	jsr	_base
	jmp	store

	fdb	dotb16
	fdb	link55
link55a	fcb	6,".BIN16"
dotb16	jsr	_base
	jsr	at
	jsr	dolit
	fdb	2
	jsr	_base
	jsr	store
	jsr	swap
	jsr	dolit
	fdb	0
	jsr	lessnum
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	grtnum
	jsr	_type
	jsr	_base
	jmp	store

	fdb	dotb8
	fdb	link55a
link55b	fcb	5,".BIN8"
dotb8	jsr	_base
	jsr	at
	jsr	dolit
	fdb	2
	jsr	_base
	jsr	store
	jsr	swap
	jsr	dolit
	fdb	0
	jsr	lessnum
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	number
	jsr	grtnum
	jsr	_type
	jsr	_base
	jmp	store

	fdb	_adres
	fdb	link55b
link56	fcb	5,"adres"
_adres	jsr	dovar
	fdb	0

	fdb	_aschr
	fdb	link56
link56a	fcb	4,"char"
_aschr	jsr	dovar
	fdb	0

	fdb	dotb
	fdb	link56a
link57	fcb	5,".byte"
dotb	jsr	_base
	jsr	at
	jsr	_hex
	jsr	swap
	jsr	dolit
	fdb	0
	jsr	lessnum
	jsr	number
	jsr	number
	jsr	grtnum
	jsr	_type
	jsr	_base
	jmp	store

	fdb	getch
	fdb	link57
link58	fcb	7,"getchar"
getch	jsr	_adres
	jsr	at
	jsr	cat
	jsr	dolit
	fdb	$7f
	jsr	_and
	jsr	dup
	jsr	_aschr
	jmp	store

	fdb	_ascii
	fdb	link58
link59	fcb	5,"ascii"
_ascii	jsr	_adres
	jsr	store
	jsr	space
	jsr	dolit
	fdb	$10
	jsr	dolit
	fdb	0
	jsr	xdo
asclp	jsr	getch
	jsr	dolit
	fdb	$20
	jsr	less
	jsr	_aschr
	jsr	at
	jsr	dolit
	fdb	$7e
	jsr	great
	jsr	_or
	pulu	d
	cmpd	#0
	bne	asc1
	bra	asc2
asc1	jsr	dolit
	fdb	$2e
	jsr	emit
	bra	asc3
asc2	jsr	_aschr
	jsr	at
	jsr	emit
asc3	jsr	dolit
	fdb	1
	jsr	_adres
	jsr	pstore
	jsr	xloop
	jmp	asclp
	rts

	fdb	hdump
	fdb	link59
link60	fcb	4,"DUMP"
hdump	jsr	_adres
	jsr	store
hdumplp	jsr	do_cr
	jsr	_adres
	jsr	at
	jsr	dup
	jsr	doth
	jsr	dotstr
	fcb	2,": "
	jsr	dolit
	fdb	8
	jsr	dolit
	fdb	0
	jsr	xdo
dmplp1	jsr	_adres
	jsr	at
	jsr	cat
	jsr	dotb
	jsr	_adres
	jsr	at
	jsr	oneplus
	jsr	cat
	jsr	dotb
	jsr	space
	jsr	dolit
	fdb	2
	jsr	_adres
	jsr	pstore
	jsr	xloop
	jmp	dmplp1
	jsr	_ascii
	jsr	keyq
	pulu	d
	cmpd	#0
	bne	hdump1
	jmp	hdumplp
hdump1	rts

	fdb	_wait
	fdb	link60
link999	fcb	4,"WAIT"
_wait	ldd	#0			; 3 cycles
wtlp1	subd	#1			; 4 cycles
	bne	wtlp1			; 6 cycles 65536*10 cycles
	rts				; 5 cycles + 5 cycles
;					Total of 655373 cycles
einde	equ	*

	end
