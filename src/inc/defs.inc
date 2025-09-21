BCALL_TABLE_NUM_ENTRIES .equ    16
DEFAULT_LINE_HEIGHT     .equ    7

;==============================================================================
;==============================================================================
;==============================================================================
; Keyboard and other bit labeling

kbd_down    .equ    (1<<0)
kbd_left    .equ    (1<<1)
kbd_right   .equ    (1<<2)
kbd_up      .equ    (1<<3)
kbd_yequ    .equ    (1<<4)
kbd_2nd     .equ    (1<<5)
kbd_mode    .equ    (1<<6)
kbd_del     .equ    (1<<7)

;bcall_entry_table       .equ    appBackUpScreen
;avarstart               .equ    bcall_entry_table+(3*16)
;basepage                .equ    

;==============================================================================
;==============================================================================
;==============================================================================
; Variables

.setvarbase(appBackUpScreen)
;Initializations and bookkeeping
.var(bcall_entry_table, (3*16))
.var(basepage,1)
.var(exitaddr,2)
.var(exitstack,2)
.var(flagbackroom,128)  ;Accessed via negative offsets to gameflags
.var(gameflags,128)



.setvarbase(statVars)
.assert(statVars = 8A3Ah, "statVars not in expected location. Fix for interrupts.")
.var(statVars_head, (8A8Ah-8A3Ah))
.var(statVars_int,  (8B00h-8A8Ah))
.var(statVars_vector,(8C01h-8B00h))
.var(statVars_foot, ((8A3Ah+531)-8C01h))
.assert(statVars_int = 8A8Ah, "Interrupt location incorrect")
.assert(statVars_vector = 8B00h,"Interrupt vector location incorrect")
.assert(statVars_foot = 8C01h, "statVars footer location incorrect")

.setvarbase(statVars_head)
.var(tempbuf,12)
.var(line_start,1)
.var(window_top,1)
.var(line_height,1)
.var(tab_entries,8)
.var(menu_flags,1)
.var(mRegA,2)
.var(mRegB,2)
.var(mRegSP,2)
.var(mstack,16)


.var(statVars_head_end,0)
.assert((statVars_head_end < statVars_int), "Variables allocated at statVars_head exceeds max length.")

;==============================================================================
;==============================================================================
;==============================================================================
; Flags.

dispFlags       EQU 0
;bits 0-2 used for numlength
leading_zeroes  .equ 3
right_align     .equ 4
show_neg        .equ 5
show_pos        .equ 6
show_long       .equ 7  ;unused


;==============================================================================
;==============================================================================
;==============================================================================
; Defines for scripting

#define mxy(x,y) .db 1,x,y

#macro mstabs(a,b,c,d,e,f,g)
    #define MSTABS_COUNT 0
    #ifdef a
        #define MSTABS_A eval(a)
        #define MSTABS_COUNT eval(MSTABS_COUNT + 1)
    #endif
    #ifdef b
        #define MSTABS_B eval(b)
        #define MSTABS_COUNT eval(MSTABS_COUNT + 1)
    #endif
    #ifdef c
        #define MSTABS_C eval(c)
        #define MSTABS_COUNT eval(MSTABS_COUNT + 1)
    #endif
    #ifdef d
        #define MSTABS_D eval(d)
        #define MSTABS_COUNT eval(MSTABS_COUNT + 1)
    #endif
    #ifdef e
        #define MSTABS_E eval(e)
        #define MSTABS_COUNT eval(MSTABS_COUNT + 1)
    #endif
    #ifdef f
        #define MSTABS_F eval(f)
        #define MSTABS_COUNT eval(MSTABS_COUNT + 1)
    #endif
    #ifdef g
        #define MSTABS_G eval(g)
        #define MSTABS_COUNT eval(MSTABS_COUNT + 1)
    #endif
    #if MSTABS_COUNT > 0
        .db 2,MSTABS_COUNT
        #if MSTABS < 7
            .fatal("You may not define more than seven (7) tab stops.")
        #endif
        #ifdef MSTABS_A
            .db MSTABS_A
        #endif
        #ifdef MSTABS_B
            .db MSTABS_B
        #endif
        #ifdef MSTABS_C
            .db MSTABS_C
        #endif
        #ifdef MSTABS_D
            .db MSTABS_D
        #endif
        #ifdef MSTABS_E
            .db MSTABS_E
        #endif
        #ifdef MSTABS_F
            .db MSTABS_F
        #endif
        #ifdef MSTABS_G
            .db MSTABS_G
        #endif
    #endif
#endmacro

;NOTE: DO NOT DEFINE AN 8-ARG OR HIGHER mstabs. HANDLER WILL CRASH IF SO.

#define mwin(x1,y1,x2,y2,f) .db 3,x1,y1,x2,y2,f
    #define mWINDOW 1<<0
    #define mBORDER 1<<1
    #define mMENU 1<<2
    #define mCHOICE 1<<3

#define mdispacc(f) .db 4,f
    #define mDIGIT(n) (n&7)
    #define mLZEROES (1<<leading_zeroes)
    #define mRALIGN (1<<right_align)
    #define mSHOWNEG (1<<show_neg)
    #define mSHOWPOS (1<<show_pos)
    #define mLONG

#define msetacc(n) .db 5,(n&$00FF),((n>>8)&$00FF)

#define maddacc(n) .db 6,(n&$00FF),((n>>8)&$00FF)

#define moper(n) .db 7,n
    #define mPUSH_A 0
    #define mPOP_A 1
    #define mEXCH_A_B 2
    #define mMOV_A_B 3
    #define mMOV_B_A 4
    #define mADD_A_B 5
    #define mMULT_A_B 6
    #define mDIV_A_B 7
    #define mMOD_A_B 8
    #define mMOV_BIND_A 9
    #define mMOV_A_BIND 10
    #define mJP_A 11
    #define mCLR_A_HIGH 12
    

#define m__ .db 8

#define mtab .db 9  ;inlineable via \t

#define mlf .db 10  ;inlineable via \n. Newline.

#define mlh(lineheight) .db 11,lineheight

#define m__ .db 12

#define m__ .db 13

#define mcr .db 14  ;inlineable via \r. Not sure what to use for yet.

#define m__ .db 15
;You'll probably want to include a way to build a menu from grid coords and
;destination addresses
