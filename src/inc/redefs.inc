#ifndef REDEF_GUARD
#define REDEF_GUARD

;Redefine material that are in "ti83plus.inc" so that the actual file is not
;used. Makes compiling the applicatoin MUCH faster. Also helps me keep tabs on
;exactly what references are being used.
#define equ .equ
#define EQU .equ

ramStart             EQU  8000h     ;Up to 1086 bytes, reset with bcall $5011
contrast             EQU  8447h     ;Why do I have this defined here?
curRow               EQU  844Bh     ;Homescreen text, columns 0-16
curCol               EQU  844Ch     ;Homescreen text, rows 0-8
OP1                  EQU  8478h     ;Used for, in this case, name for file I/O.
penCol               EQU  86D7h     ;X. Plot screen text, columns 0 to ~89.
penRow               EQU  86D8h     ;Y. Plot screen text, rows 0- ~57.
saveSScreen          EQU  86ECh     ;graph buffer-sized RAM, used on APD.
flags                EQU  89F0h     ;IY must be set to this for bcalls or exit
statVars             EQU  8A3Ah     ;531 bytes. Used _DelRes to clear b4 exit
plotSScreen          EQU  9340h     ;graph buffer.
appBackUpScreen      EQU  9872h     ;graph buffer-sized spare memory
userMem              EQU  9D95h     ;user memory. _InsertMem and _DelMem to mod.

asm_Flag1       EQU  33          ; ASM CODING
asm_Flag2       EQU  34          ; ASM CODING
asm_Flag3       EQU  35          ; ASM CODING

sizeof_ramStart         EQU     1086
sizeof_appbackupscreen  EQU     768
sizeof_plotSScreen      EQU     768
sizeof_saveSScreen      EQU     768
sizeof_statVars         EQU     531

;Romcall definition only on LLII.asm (page 1)
#define bcall(xxxx) rst 28h  \ .dw xxxx
#define bjump(xxxx) call 50h \ .dw xxxx

;Romcalls referenced only on LLII.asm (page 1)
_JForceCmdNoChar EQU 4027h
_ChkFindSym      EQU 42F1h
_InsertMem       EQU 42F7h
_EnoughMem       EQU 42FDh
_DelMem          EQU 4357h
_PutC            EQU 4504h
_ClrLCDFull      EQU 4540h
_GetKey          EQU 4972h
_DelRes          EQU 4A20h
_CreateAppVar    EQU 4E6Ah
_Arc_Unarc       EQU 4FD8h
_DelVarArc       EQU 4FC6h
_GrBufCpy        EQU 486Ah
_VPutS           EQU 4561h



;Other system dependencies that are directly defined in LLII.asm
; bcall($5011)                ;restores first 1087 bytes in RAM
; bcall($4C36)    ;reloadappentryvecs. Name is too long for TASM to swallow.

#endif