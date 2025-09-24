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

; -- variables go here
.varvalidate(saveSScreen_int-saveSScreen_mid)

.varloc("saveSScreen_int_end")
; -- variables go here
.varvalidate(saveSScreen_end-saveSScreen_int_end)


;Static defines/equates. These are important constants that the
;game's systems use. Modify these with care.
;
KEY_LONG_INTERVAL   .equ 20     ;Delays key repeat after first keypress
KEY_SHORT_INTERVAL  .equ 5      ;Delays key repeat after the first repeat.
savefile_size       .equ 512



#define GAMEDEFS_LOADED