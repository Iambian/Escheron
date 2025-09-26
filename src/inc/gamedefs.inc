;
; Defines for memory locations to be used in-game
;

#ifndef MACROS_LOADED
    .error "You must include macros.inc before including this file."
#endif
#ifdef GAMEDEFS_LOADED
    .error "You cannot include this file more than once."
#endif



#define TEMPDATA_SIZE 16
#define NEWLINE_HEIGHT 7

;NOTE: Below is a bit of memory wrangling. If this location is altered, you
;   must account for the fact that an interrupt vector table and its routine
;   is located in this space. The interrupt itself is small, though.
.assert(saveSScreen == 86ECh, "saveSScreen must be 86ECh for interrupts location.")
saveSScreen_low     .equ saveSScreen
saveSScreen_vector  .equ 8700h
saveSScreen_mid     .equ 8702h
saveSScreen_int     .equ 8888h
saveSScreen_int_end .equ 88B8h
saveSScreen_end     .equ saveSScreen+768

.varloc("saveSScreen_low")
basepage        .var(1)
intcounter      .var(1)
waittimer       .var(1)
keytimer        .var(1)
prevkey         .var(1)
curkey          .var(1)
stackbase       .var(2)
seed            .var(8)
randData        .var(2)
filepointer     .var(2)
.varvalidate(saveSScreen_vector-saveSScreen_low)

.varloc("saveSScreen_mid")
tempdata        .var(TEMPDATA_SIZE)  ;Short-lived data goes here. Used in-place.
textx           .var(1)
texty           .var(1)
textxstart      .var(1)
;You'll want to define other flags prior to this one for fuller IY range
gameflags       .var(32)    ;IY should point to the start of this.
menuoptions     .var(65)    ;1 byte lead count, 32 16-bit entries.
menuoptionmask  .equ $3F
textacc         .var(2)     ;text system accumulator
textidx         .var(2)     ;text system index register
textaccshadow   .var(2)     ;text system shadow accumulator

; -- variables go here
.varvalidate(saveSScreen_int-saveSScreen_mid)
.assert(textx+1 == texty, "textx must always be immediately before texty.")

.varloc("saveSScreen_int_end")
; -- variables go here
.varvalidate(saveSScreen_end-saveSScreen_int_end)


;Static defines/equates. These are important constants that the
;game's systems use. Modify these with care.
;
KEY_LONG_INTERVAL   .equ 20     ;Delays key repeat after first keypress
KEY_SHORT_INTERVAL  .equ 5      ;Delays key repeat after the first repeat.
savefile_size       .equ 512


;===============================================================================
; Game engine flags
;

;Don't use this flag area for anything that needs to persist.
numflags            .equ 0
reloadflags         .equ 0
leading_nonzero_found  = 0
is_negative            = 1
force_map_reload       = 2  ;//When hotspot taken, force a system reload
;Two whole bytes for temporary storage.
tempfvar            .equ 1
tempfvar2           .equ 2



;Text engine macros
;\r is CR (13=$0D), \n is LF (10=$0A), \t is TAB (9 = $09)
;CR is used to set the left margin at current cursor location.
;LF is to return the cursor to the left margin and move it down by NEWLINE_HEIGHT
;TAB is to mark the current cursor position as a menu option location.

dhl_show_lead_zeroes  	.equ 3
dhl_right_align_num   	.equ 4
dhl_indic_neg_sign    	.equ 5  ;Must be set to indicate incoming number is signed.
dhl_show_pos_sign     	.equ 6  ;Must be signed, shows pos sign if number is positive
dhl_fetch_long_values 	.equ 7

#define mf_1DIGIT 1
#define mf_2DIGIT 2
#define mf_3DIGIT 3
#define mf_4DIGIT 4
#define mf_5DIGIT 5
#define mf_6DIGIT 6
#define mf_7DIGIT 7
#define mf_8DIGIT 0
#define mf_SHOW_LEAD_ZEROES (1<<dhl_show_lead_zeroes)
#define mf_RIGHT_ALIGN (1<<dhl_right_align_num)
#define mf_SIGNED_SHOW_NEG (1<<dhl_indic_neg_sign)
#define mf_SIGNED_SHOW_POS (1<<dhl_show_pos_sign)
#define mf_USE_LONG_VALUES (1<<dhl_fetch_long_values)

#define m_eof 0
#define m_setxy(x,y) .db 1,x,y
#define m_addxy(x,y) .db 2,x,y
#define m_call(adr) .db 3,lo(adr),hi(adr)
#define m_print(adr) .db 4,lo(adr),hi(adr)
#define m_printacc(flags) .db 5,flags
#define m_jrfnz(flagid,rel) .db 6,flagid,rel-$-2
#define m_jrfz(flagid,rel) .db 7,flagid,rel-$-2
#define m_drawbox(x1,y1,x2,y2) .db 8,x1,y1,x2,y2
#define m_menuopt() .db 9
#define m_newline() .db 10
#define m_setflag(flagid) .db 11,flagid
#define m_togflag(flagid) .db 12,flagid
#define m_setleftmargin() .db 13
#define m_clracc() .db 14
#define m_exacc() .db 15
#define m_setacc1(val1b) .db 16,val1b
#define m_setacc2(val2b) .db 17,lo(val2b),hi(val2b)
#define m_adrtoacc1(adr) .db 18,lo(adr),hi(adr)
#define m_adrtoacc2(adr) .db 19,lo(adr),hi(adr)
#define m_setidx(adr) .db 20,lo(adr),hi(adr)
#define m_addindexed(offset) .db 21, offset
#define m_mltacc(val) .db 22,val
#define m_mltacc2(val) .db 23,val








#define GAMEDEFS_LOADED