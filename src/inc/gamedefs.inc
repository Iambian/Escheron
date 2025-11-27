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
#define NEWLINE_HEIGHT 8
#define BOX_MARGIN_TOP 4
#define BOX_MARGIN_LEFT 4

;NOTE: Below is a bit of memory wrangling. If this location is altered, you
;   must account for the fact that an interrupt vector table and its routine
;   is located in this space. The interrupt itself is small, though.
.assert(saveSScreen == 86ECh, "saveSScreen must be 86ECh for interrupts location.")
saveSScreen_low     .equ saveSScreen
saveSScreen_vector  .equ 8700h
saveSScreen_mid     .equ 8802h
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
textidxshadow   .var(2)     ;text system shadow index
textaccshadow2  .var(2)

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
; Game data structures.
; Labels defined by .var() are offsets (abs adr at 0 to simulate this)
; Label defined in .struct() are set to the size of the struct block.
; Since our assembler doesn't properly support member operators, this can
; easily enough be simulated by adding a bunch of offsets together.
;
;

.struct("playerobj")
p_id        .var(1)
p_status    .var(1)
p_lvl       .var(1)
p_exp       .var(1)
p_hp        .var(2)
p_maxhp     .var(2)
p_mp        .var(1)
p_maxmp     .var(1)
p_atk       .var(1)
p_def       .var(1)
p_mgc       .var(1)
p_agi       .var(1)
p_wpn       .var(1)
p_shl       .var(1)
p_arm       .var(1)
p_acc       .var(1)
p_spells    .var(12)
p_eof       .var(0)
.endstruct()


;NOTE: charslotid is the ID of the player in that character slot.
;(e.g. charslotid1 being set to 5 means that the first character slot is
;   now referencing player5).

.struct("gamedata")
charslotid1     .var(1) ;{ID of the player in that charslot. e.g. if 5 is here,
charslotid2     .var(1) ; then charslotid1 references player5. The value -1
charslotid3     .var(1) ; means a character slot is empty}
gold_stash      .var(2)
playerpos       .var(2)
playermapid     .var(1)
ship1pos        .var(3)
ship2pos        .var(3)
airshippos      .var(3)
teleportreturn  .var(4) ;.dw xy \.db mapid, state_info
stepsuntilnextencounter .var(1)
encounterformation  .var(1)
prngseed        .var(8)
underdeepid     .var(1) ;current level in underdeep. -1 if not in it.
underdeepseed   .var(8)
player0         .var(playerobj)
player1         .var(playerobj)
player2         .var(playerobj)
player3         .var(playerobj)
player4         .var(playerobj)
player5         .var(playerobj)
nplayer         .var(playerobj)
gameflagbank    .var(8)
inventory       .var(32)
flagbank0       .var(32)
flagbank1       .var(32)
flagbank2       .var(32)
.endstruct()

.struct("savefile")
save_header     .var(4)
saveslot1       .var(gamedata)
saveslot2       .var(gamedata)
.endstruct()

.struct("battleobj")
b_objid         .var(1) ;//enemy/charID. Set bit 7 to reference enemies
b_status_1      .var(1) ;//Poisoned/KO/sleep/anything else
b_status_2      .var(1) ;//probably not used
b_properties    .var(1) ;//infinite HP/MP, double attack, etc
b_eres          .var(1) ;//elemental resistance flags
b_ewkn          .var(1) ;//elemental weakness flags
b_eatk          .var(1) ;//elemental attack flags
b_curhp         .var(2) ;//Current Health Points
b_maxhp         .var(2) ;//Maximum Health Points
b_curmp         .var(1) ;//Current Mana Points
b_maxmp         .var(1) ;//Maximum Mana Points
b_atktot        .var(2) ;//Precomputed ATTACK total
b_deftot        .var(2) ;//Precomputed DEFENSE total
b_magtot        .var(2) ;//Precomputed MAGIC total
b_agitot        .var(2) ;//Precomputed AGILITY total
b_hitrate       .var(1) ;//Precomputed HIT RATE percentage total
b_crit          .var(1) ;//Precomputed CRITICAL RATE percentage total
b_evasion       .var(1) ;//Precomputed EVADE RATE percentage total
.union()
b_wpn           .var(1)
b_shl           .var(1)
b_arm           .var(1)
b_acc           .var(1)
b_spellbook     .var(p_eof-p_spells)
.unionsplit()
b_sprloc        .var(2)
b_sprdat        .var(2)
b_rank          .var(1)
b_gold          .var(2)
b_droprate      .var(1)
b_dropitem      .var(1)
b_curskill      .var(1)
b_skilltable    .var(8)
.endunion()
.endstruct()




;//data structure offset by common data
;structstart(battcom)
;    defsvar(1,b_wpn)        ;//Player current weapon
;    defsvar(1,b_shl)        ;//Player current shield
;    defsvar(1,b_arm)        ;//Player current armor
;    defsvar(1,b_acc)        ;//Player current accessory
;    defsvar(p_spells_size,b_spellbook) ;//Player spellbook
;structend(battplayer)

;structstart(battcom)
;    defsvar(2,b_sprloc)      ;//Enemy sprite offset to decompression buffer
;    defsvar(2,b_sprdat)      ;//Enemy sprite data address copy in bat buf
;    defsvar(1,b_rank)        ;//Enemy rank
;    defsvar(2,b_gold)        ;//Amount of gold the enemy is carrying
;    defsvar(1,b_droprate)    ;//Drop rate of item
;    defsvar(1,b_dropitem)    ;//Item that drops from this enemy
;    defsvar(1,b_curskill)    ;//Counter 0-7 of current skill enemy is using
;    defsvar(8,b_skilltable)  ;//SkillID's of all 8 skills this enemy can use
;structend(battenemy)






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
; Following are removed since we are not going to be using that capability.
; instead, the script system is overloading these values.
;#define mf_6DIGIT 6
;#define mf_7DIGIT 7
;#define mf_8DIGIT 0
#define mf_DIGITOVERLOAD_ADDFLGTOA  6
#define mf_DIGITOVERLOAD_ADDFLGTOI  7
#define mf_DIGITOVERLOAD_ADDFLGTOAP 0
;
#define mf_SHOW_LEAD_ZEROES (1<<dhl_show_lead_zeroes)
#define mf_RIGHT_ALIGN (1<<dhl_right_align_num)
#define mf_SIGNED_SHOW_NEG (1<<dhl_indic_neg_sign)
#define mf_SIGNED_SHOW_POS (1<<dhl_show_pos_sign)
#define mf_USE_LONG_VALUES (1<<dhl_fetch_long_values)

mmb_list_short      .equ 0
mmb_list_long       .equ 1
mmb_list_hpboost    .equ 2
mmb_list_mpboost    .equ 3
mmb_list_atkboost   .equ 4
mmb_list_defboost   .equ 5
mmb_list_magboost   .equ 6
mmb_list_agiboost   .equ 7
mmb_list_onlystatus .equ 8
mmb_single          .equ 9

#define mm_LIST_SHORT       (1 << mmb_list_short     )
#define mm_LIST_LONG        (1 << mmb_list_long      )
#define mm_LIST_HPBOOST     (1 << mmb_list_hpboost   )
#define mm_LIST_MPBOOST     (1 << mmb_list_mpboost   )
#define mm_LIST_ATKBOOST    (1 << mmb_list_atkboost  )
#define mm_LIST_DEFBOOST    (1 << mmb_list_defboost  )
#define mm_LIST_MAGBOOST    (1 << mmb_list_magboost  )
#define mm_LIST_AGIBOOST    (1 << mmb_list_agiboost  )
#define mm_LIST_ONLYSTATUS  (1 << mmb_list_onlystatus)
#define mm_LIST_SINGLE      (1 << mmb_single         )

#define m_XF_LD 0
#define m_XF_OR 1
#define m_XF_AND 2
#define m_XF_XOR 3
#define m_XF_ADD 4
#define m_XF_SUB 5
#define m_XF_NOT 6
#define m_XF_NEG 7

#define m_FX_EX 0
#define m_FX_LD 1

#define m_JR_OPERS (0 << 6)
#define m_JR_UNCOND (1 << 6)
#define m_JSR_UNCOND (2 << 6)
#define m_JSR_REG (3 << 6)
#define m_JR_BZ   (m_JR_OPERS | (0<<4))
#define m_JR_BNZ  (m_JR_OPERS | (1<<4))
#define m_JR_BPZ  (m_JR_OPERS | (2<<4))
#define m_JR_BPNZ (m_JR_OPERS | (3<<4))
#define m_JSR_A   (m_JSR_REG | (0<<4))
#define m_JSR_AP  (m_JSR_REG | (1<<4))
#define m_JSR_I   (m_JSR_REG | (2<<4))
#define m_JSR_IP  (m_JSR_REG | (3<<4))

;NOTE: Unconditional jump consumes 8-15
;NOTE: [td0001] Make those consumed bits extend the jump range.
;       (i.e. .db 23 \ .dw FSSS_SSSS_SSSS_SSSS, where F=1, S = int15_t)

; BEGIN TRANSITIONING TO NEW BYTECODES
;TODO:  SCRAP THE OLD ASSEMBLY AND REPURPOSE ITS REMAINS. CMDS 0-10 MOSTLY SAME
; Register file. All registers are 2 bytes wide.
; [A]   - Accumulator, splits into low byte [a0] and high byte [a1]
;           Most instructions will involve this register.
; [AP]  - Accumulator Prime. Temporary storage for argument passing and math ops
; [I]   - Index. May be used in math ops. Primary use is indexed operations.
; [IP]  - Index Prime. For storing and swapping for indexing an additional adr.
;

#macro RELCHK(rel_adr_val, bitrange)
    #define R_MINEVAL ((rel_adr_val) >= (-1<<(bitrange-1)))
    #define R_MAXEVAL ((rel_adr_val) <= ((1 <<(bitrange-1))-1))
    .echo "PRIMARY VALUE ", (-1<<(bitrange-1)), ", SECONDARY VALUE ", ((1 <<(bitrange-1))-1), ", RELATIVE ADDR: ",rel_adr_val
    .echo "EVAL MIN: ", R_MINEVAL, ", EVAL MAX: ",R_MAXEVAL
    .assert(R_MINEVAL && R_MAXEVAL, "Relative address outside allowed range")
#endmacro


#define m_eof           .db 0
#define m_setxy(x,y)    .db 1,x,y
#define m_addxy(x,y)    .db 2,x,y
;   slot 3 empty
#define m_dispacc(flg)  .db 4,flg   ;numeric display A given flags.
;overloaded unused digit display to perform int5_t imm add to register.
#define m_adda_sm(v)    .db 4,((v&31)<<3)+mf_DIGITOVERLOAD_ADDFLGTOA
#define m_addi_sm(v)    .db 4,((v&31)<<3)+mf_DIGITOVERLOAD_ADDFLGTOI
#define m_addap_sm(v)   .db 4,((v&31)<<3)+mf_DIGITOVERLOAD_ADDFLGTOAP
;   slot 5 empty
#define m_run(adr)      .db 6, lo(adr), hi(adr)     ;Run z80 ASM code.
#define m_clra0()       .db 7       ; Clears a0
#define m_seta0(v)      .db 8,v     ; v -> a0
#define m_menuopt()     .db 9       ;CTRL CODE: TAB
#define m_newline()     .db 10      ;CTRL CODE: LF
#define m_swap()        .db 11      ; a0 <-> a1
#define m_exaap()       .db 12      ; A <-> AP
#define m_setleft()     .db 13      ;CTRL CODE: CR. Resets to this X-pos on LF.
#define m_exiip()       .db 14      ; I <-> IP
#define m_exai()        .db 15      ; A <-> I
#define m_addap()       .db 16      ; A + AP -> A
#define m_addi()        .db 17      ; A + I -> A
#define m_sext()        .db 18      ; a0 sign-extends into a1
#define m_addind(v)     .db 19,v    ; uint8_t([ I + v ]) + A -> A
#define m_mltacc()      .db 20      ; a0 * a1 -> A
#define m_djnz(rel)     .db 21, rel-$-1 ;A--, `JR rel` if A != 0
#define m_write()       .db 22      ; a0 -> [I]
;%FFAAIIII Operation based on FF. F may merge AAIIII to rel. Else, defined below
#define m_jrbz(b,rel)   .db 23, m_JR_BZ|(b&15), rel-$-1   ;if A[`bt`] == 0
#define m_jrbnz(b,rel)  .db 23, m_JR_BNZ|(b&15), rel-$-1  ;if A[`bt`] != 0
#define m_jrbpz(b,rel)  .db 23, m_JR_BPZ|(b&15), rel-$-1  ;if AP[`bt`] == 0
#define m_jrbpnz(b,rel) .db 23, m_JR_BPNZ|(b&15), rel-$-1 ;if AP[`bt`] != 0
#define m_jr(rel)       .db 23, m_JR_UNCOND|(hi(rel-$-2)&$3F), lo(rel-$-1,14)
#define m_jsr(rel)      .db 23, m_JSR_UNCOND|(hi(rel-$-2)&$3F), lo(rel-$-1) \ RELCHK(rel-$, 14)
#define m_jsra()        .db 23, m_JSR_A
#define m_jsrap()       .db 23, m_JSR_AP
#define m_jsri()        .db 23, m_JSR_I
#define m_jsrip()       .db 23, m_JSR_IP
;slot 24 empty
;%SSCAAAAA C=oper(0:ex,1:ldtomem), does oper (1<SS) bytes starting at short adr A.
;input size is in actual bytes and is converted via macro
#define m_exfa(fb,size) .db 25, fb&31+(twobitlogsub2(size)<<6)+(m_FX_EX<<5)
#define m_ldfa(fb,size) .db 25, fb&31+(twobitlogsub2(size)<<6)+(m_FX_LD<<5)
;%OOOSAAAA [fbank adr] opers with A, store to A, 1 or 2 bytes.
#define m_ldaf(fb,size)  .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_LD  &7)<<5)
#define m_oraf(fb,size)  .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_OR  &7)<<5)
#define m_andaf(fb,size) .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_AND &7)<<5)
#define m_xoraf(fb,size) .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_XOR &7)<<5)
#define m_addaf(fb,size) .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_ADD &7)<<5)
#define m_subaf(fb,size) .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_SUB &7)<<5)
#define m_notaf(fb,size) .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_NOT &7)<<5)
#define m_negaf(fb,size) .db 26, fb&15+(((size-1)&1)<<4)+((m_XF_NEG &7)<<5)
;
#define m_ldind(v)      .db 27,v    ; uint8_t([ I + v ]) -> a0. Clears a1
#define m_cpjnz(v, rel) .db 28,v, rel-$-1   ;`JR rel` if a0 != v

#define m_debug         .db 31      ;TEMP CMD: TESTER DUMPS REGFILE AND FLAGBANK

;Recipes - Uses script system as microcode to make some operations easier to read
#define m_seta(v) m_seta0((v>>8)&$FF) \ m_swap() \ m_seta0(v&$FF)
#define m_seti(v) m_exai() \ m_seta(v) \ m_exai()
#define m_print(adr) m_seta(adr) \ m_printacc()
#define m_printsafe(adr) m_exaap() \ m_print(adr) \ m_exaap()
#define m_printidx() m_exai() \ m_printacc() \ m_exai()
#define m_mltacc1(v) m_swap() \ m_seta0(v) \ m_mltacc() \ m_swap() \ m_clra0() \ m_swap()
#define m_mltacc2(v) m_swap() \ m_seta0(v) \ m_mltacc() \ m_clra0() \ m_swap()
#define m_clra() m_clra0() \ m_swap() \ m_clra0()
#define m_seta_m1() m_clra0() \ m_djnz($+1) \ m_sext()
#define m_clri() m_exai() \ m_clra() \ m_exai()
#define m_add(v) m_exaap() \ m_seta(v) \ m_addap()
#define m_setap0(v) m_exaap() \ m_seta0(v) \ m_exaap()
#define m_ldind2(v) m_ldind(v+1) \ m_swap() \ m_addind(v)
;
#define m_drawbox(x1,y1,x2,y2) m_run(menu_drawBox) \ .db x1,y1,x2,y2
#define m_printacc() m_jsra()
#define m_inci() m_addi_sm(1)
;  #define something()

;==============================================================================
; Higher level game equates. These pertain to constants and flags as used
; by character state management, the menu system, and the battle system.

;Some equipment is gender-locked. We'll have to define them here, but the
;character data that uses them is defined elsewhere (likely in an X-DEF)
#define GENDER_NONE 0
#define GENDER_MALE 1
#define GENDER_FEMALE 2
#define GENDER_BOTH 3
#define GENDER_UNKNOWN 4
#define GENDER_ALL 255

; I'm not sure where this is supposed to be defined
; so here's as good a place as any.
MAX_ALLOWED_HP .EQU 600
MAX_ALLOWED_MP .EQU 255



;Symbol extensions used by the text rendering system to draw icons
;inline with relevant text data (e.g. item type symbols for equipment in menus)
#define SYM_START 240
SYM_FIRE      .equ SYM_START + 0
SYM_LIT       .equ SYM_START + 1
SYM_ICE       .equ SYM_START + 2
SYM_POISON    .equ SYM_START + 3
SYM_SLEEP     .equ SYM_START + 4
SYM_SWORD     .equ SYM_START + 5
SYM_STAFF     .equ SYM_START + 6
SYM_SHIELD    .equ SYM_START + 7
SYM_CLOTHING  .equ SYM_START + 8
SYM_ARMOR     .equ SYM_START + 9
SYM_ACCESSORY .equ SYM_START + 10
SYM_POTION    .equ SYM_START + 11
SYM_KEYITEM   .equ SYM_START + 12
SYM_HP        .equ SYM_START + 13
SYM_MP        .equ SYM_START + 14
SYM_CLAW      .equ SYM_START + 15

;TODO: Figure out what these equates are for. I suspect they're used
;      by the battle system for stat offsets.
EOFST_LENGTH  .EQU 9
EOFST_ATK     .equ 0
EOFST_DEF     .equ 1
EOFST_WIS     .equ 2
EOFST_AGI     .equ 3
EOFST_HIT     .equ 4
EOFST_CRIT    .equ 5
EOFST_F1      .equ 6
EOFST_F2      .equ 7
EOFST_F3      .equ 8

;Elemental identifiers for bitshift/masking purposes. These will not be
;directly used anywhere else but here and equipment.
ELM_FIRE     .equ 0
ELM_LIT      .equ 1
ELM_ICE      .equ 2
ELM_POISON   .equ 3
ELM_SLEEP    .equ 4 ;//First five elements correspond to symbol order
ELM_HOLY     .equ 5
ELM_KO       .equ 6
ELM_GRAVITY  .equ 7

;Property masks for use in the battle system.
;May have overlap with other systems.
ENF_FIRE     .equ 1<<ELM_FIRE   
ENF_LIT      .equ 1<<ELM_LIT    
ENF_ICE      .equ 1<<ELM_ICE    
ENF_POISON   .equ 1<<ELM_POISON 
ENF_SLEEP    .equ 1<<ELM_SLEEP  
ENF_HOLY     .equ 1<<ELM_HOLY   
ENF_KO       .equ 1<<ELM_KO     
ENF_GRAVITY  .equ 1<<ELM_GRAVITY
ENF_RESALL   .equ $FF

ENF_UNDEAD   .equ 1<<0
ENF_DRAIN    .equ 1<<1
ENF_HP_REGEN .equ 1<<2
ENF_DBL_ATK  .equ 1<<3
ENF_INF_HP   .equ 1<<4
ENF_INF_MP   .equ 1<<5
ENF_NO_RAND  .equ 1<<6

;//Status effects equates. Should match applicable elements.
;STATUS_        .equ 0   ;//ELM_FIRE     .equ 0
;STATUS_        .equ 1   ;//ELM_LIT      .equ 1
;STATUS_        .equ 2   ;//ELM_ICE      .equ 2
STATUS_POISON  .equ 3   ;//ELM_POISON   .equ 3
STATUS_SLEEP   .equ 4   ;//ELM_SLEEP    .equ 4
;STATUS_        .equ 5   ;//ELM_HOLY     .equ 5
STATUS_KO      .equ 6   ;//ELM_KO       .equ 6
;STATUS_        .equ 7   ;//ELM_GRAVITY  .equ 7

;Equipment property bitmasks and identifiers. Follows the specific format:
; %TTssDDDD, where TT:
;   00 = Null object.
;       ssDDDD = 0
;   01 = Item-specific special effect
;       ssDDDD = 6 bit item effect ID
;   10 = If used as item in battle, unleashes a spell effect.
;       ssDDDD = 6 bit spell ID
;   11 = Equipment properties
;       ss:00 = Equipment resistance identifier
;       ss:01 = Equipment attack identifier
;       ss:10 = Equipment weakness identifier
;       ss:11 = Equipment resists all elements (dddd not used in this case)
;       0ddd  = 3 bit element ID code.
EQPR_H_MASK .equ 3<<6
EQPR_H_NULL .equ 0<<6
EQPR_H_CODE .equ 1<<6
EQPR_H_ITEM .equ 2<<6
EQPR_H_PROP .equ 3<<6

EQPR_M_MASK .equ 3<<4
EQPR_M_RESI .equ 0<<4
EQPR_M_WEAK .equ 1<<4
EQPR_M_ATTK .equ 2<<4
EQPR_M_RALL .equ 3<<4

EQPR_L_MASK .equ 7<<0  ;//could be 15<<0, but we use only 3 bits

;
;NOTE:  Spells unleashed when equipment is used as an "item" in battle are
;       defined where spells are defined, instantiated upon first invocation
;       of `src/xdefs/spells.inc`. This is include-guarded.
;

EQ_RES_FIRE     .equ EQPR_H_PROP | EQPR_M_RESI | ELM_FIRE    
EQ_RES_LIT      .equ EQPR_H_PROP | EQPR_M_RESI | ELM_LIT     
EQ_RES_ICE      .equ EQPR_H_PROP | EQPR_M_RESI | ELM_ICE     
EQ_RES_POISON   .equ EQPR_H_PROP | EQPR_M_RESI | ELM_POISON  
EQ_RES_SLEEP    .equ EQPR_H_PROP | EQPR_M_RESI | ELM_SLEEP   
EQ_RES_HOLY     .equ EQPR_H_PROP | EQPR_M_RESI | ELM_HOLY    
EQ_RES_KO       .equ EQPR_H_PROP | EQPR_M_RESI | ELM_KO      
EQ_RES_GRAVITY  .equ EQPR_H_PROP | EQPR_M_RESI | ELM_GRAVITY
EQ_RES_ALL      .equ EQPR_H_PROP | EQPR_M_RALL

EQ_WKN_FIRE     .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_FIRE    
EQ_WKN_LIT      .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_LIT     
EQ_WKN_ICE      .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_ICE     
EQ_WKN_POISON   .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_POISON  
EQ_WKN_SLEEP    .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_SLEEP   
EQ_WKN_HOLY     .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_HOLY    
EQ_WKN_KO       .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_KO      
EQ_WKN_GRAVITY  .equ EQPR_H_PROP | EQPR_M_WEAK | ELM_GRAVITY 

EQ_ATK_FIRE     .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_FIRE    
EQ_ATK_LIT      .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_LIT     
EQ_ATK_ICE      .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_ICE     
EQ_ATK_POISON   .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_POISON  
EQ_ATK_SLEEP    .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_SLEEP   
EQ_ATK_HOLY     .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_HOLY    
EQ_ATK_KO       .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_KO      
EQ_ATK_GRAVITY  .equ EQPR_H_PROP | EQPR_M_ATTK | ELM_GRAVITY 

EQ_EFF_DEFENSE     .equ EQPR_H_CODE | 1
EQ_EFF_RUNIC       .equ EQPR_H_CODE | 2
EQ_EFF_REVENGE     .equ EQPR_H_CODE | 3
EQ_EFF_DRAIN       .equ EQPR_H_CODE | 4
EQ_EFF_CASTBLAZE   .equ EQPR_H_CODE | 5
EQ_EFF_CASTSHOCK   .equ EQPR_H_CODE | 6
EQ_EFF_HPREGEN     .equ EQPR_H_CODE | 7
EQ_EFF_F_WEARABLE  .equ EQPR_H_CODE | 8
EQ_EFF_M_WEARABLE  .equ EQPR_H_CODE | 9















#define GAMEDEFS_LOADED