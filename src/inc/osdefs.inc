; Contains OS call/environment and hardware-specific defines.
;

;---------------------------------------------------------
; CLEAR EXISTING OSDEFSET
#ifdef OSDEFSET
#undef OSDEFSET
#endif

;---------------------------------------------------------
; CONFIGURATION FOR BARE METAL INSTALLS
#ifdef BAREMETAL
    ;not quite ready for this.
#define OSDEFSET "BAREMETAL"
#endif

;---------------------------------------------------------
; CONFIGURATION FOR TI-83 PLUS CSE
#ifdef 83PCSE
    ;also not ready for this.
#define OSDEFSET "83PCSE"
#endif

;---------------------------------------------------------
; If no special configuration, then default to TI-83 Plus.
#ifndef OSDEFSET
#define equ .equ
#define EQU .equ

#define bcall(lbl) rst 28h \.dw lbl
#define bjump(lbl) call 50h \.dw lbl
#define errhandon(lbl) ld hl,lbl \ call 59h
#define errhandoff() call 5Ch

_DelRes		        EQU 4A20h
_ChkFindSym         EQU 42F1h
_Arc_Unarc          EQU 4FD8h
_CreateAppVar       EQU 4E6Ah
_DelVarArc          EQU 4FC6h
_InsertMem          EQU 42F7h
_DelMem             EQU 4357h
_PutC               EQU 4504h
_NewLine            EQU 452Eh
_JForceCmdNoChar    EQU 4027h
_GetCSC             EQU 4018h
_GetKey             EQU 4972h
_EnoughMem	        EQU 42FDh
_ReloadAppEntryVecs EQU 4C36h
_ClrScrnFull        EQU 4546h

userMem              EQU  9D95h  ;83/84+SE (non-CSE) start of usermem

ramStart             EQU  8000h  ;256 bytes  ;used by boot code for scratch
ramCode              EQU  8100h  ;304 bytes  ;used as temp exec for flashwrite
saveSScreen          EQU  86ECh  ;786 bytes
plotSScreen          EQU  9340h  ;768 bytes
statVars             EQU  8A3Ah  ;531 bytes  ;use _DelRes to clear/invalidate
textShadow           EQU  8508h  ;128 bytes
cmdShadow            EQU  966Eh  ;128 bytes
tempSwapArea         EQU  82A5h  ;323 bytes  ;avoid archiving, etc

flags                EQU  89F0h

OP1                  EQU  8478h  ;11 bytes
OP2                  EQU  8483h
OP3                  EQU  848Eh
OP4                  EQU  8499h
OP5                  EQU  84A4h
OP6                  EQU  84AFh

curRow               EQU  844Bh  ;1 byte; 0-7
curCol               EQU  844Ch  ;1 byte; 0-15
penCol               EQU  86D7h
penRow               EQU  86D8h


textFlags      	EQU    5	;Text output flags
textEraseBelow 	EQU    1	; 1=erase line below small char
textScrolled	   EQU    2	; 1=screen scrolled
textInverse       EQU    3	; 1=display inverse bit-map
textInsMode       EQU    4	; 0=overstrike, 1=insert mode

asm_Flag1       EQU  33          ;// ASM CODING
asm_Flag2       EQU  34          ;// ASM CODING
asm_Flag3       EQU  35          ;// ASM CODING

;//direct keyboard scancodes
;//Note: AND these together to do multiple masks
dsk_up    .equ $F7  ;// 0
dsk_right .equ $FB  ;// 1
dsk_left  .equ $FD  ;// 2
dsk_down  .equ $FE  ;// 3
dsk_del   .equ $7F  ;// 4
dsk_mode  .equ $BF  ;// 5
dsk_2nd   .equ $DF  ;// 6
dsk_yequ  .equ $EF  ;// 7

#define OSDEFSET "83P"
#endif
