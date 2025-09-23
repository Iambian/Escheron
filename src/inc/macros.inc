;Tools used to consolidate functionality and make source easier to read
;TODO: figure out a way to do use count for optimiztion purposes later
;NOTE: Things I learned:
;1. If you do something like mymacro("label"), you have freedom.
;   - #define STRINGDEF eval(name)
;   - #define LABELDEF concat(name)
;   - STRINGDEF shows as a string in cases of .echo
;   - LABELDEF converts that to a label. It still has to evaluate to something,
;       which you may do inside the macro.
;   - Using concat() in this way lets you construct your own labels. 
;   - concat() can also be used in other ways. Need to figure more on this.
;   - So far, I've got .varloc and .branch, each of which do something different
;

#define getpage(adr) ((adr>>16)&$FF)+0
#define lo(adr) (adr&$FF)+0
#define hi(adr) ((adr>>8)&$FF)+0

;Use this right before the first executable line in your app.
;This macro initis pagination for multipage apps, sets origin to $4000, and
;pastes a minimal application header. Code begins execution immediately
;following this macro's invocation.
;NOTE: The app name is hard-coded into this header. Be aware of that if you
;       need to use these macros for your own use!
#macro initapp()
    #ifdef APP_PAGE
        .error "You may not init an app more than once."
    #endif
    #ifdef APP_ENDED
        .error "You may not init an app that has already ended."
    #endif
    #define APP_PAGE 0
    #define APP_TOTAL_FREE_SPACE 0
    #if $ != 0
        .error "Only nonexecutable code (defines, equates, etc) may precede this."
    #endif
    .org $4000
    ;Application header
    .db $80, $0f, 0, 0, 0, 0    ;Program length (modified by signer)
    .db $80, $48, "Escheron"    ;App name
    .db $80, $90                ;Disable TI splash screen
    .db $80, $81, 1             ;Number of app pages. Signer fills this in.
    .db $80, $12, $01, $04      ;Signing key: 0104
    .db $03, $22, $09, $00      ;Date stamp. Unused.
    .db $02, $00                ;Date stamp signature. Unused.
    .db $80, $70                ;End of Header
    ;NOTE: As long as the header's less than 128 bytes, no need to pad to 256.
    .echo "Page 0 has been initialized. "
#endmacro

;When you're ready to go to the next page, use this.
;There is no way to flip back and forth between pages, so be very careful.
#macro nextpage()
    #ifndef APP_PAGE
        .error "You must first initialize the app via initapp()"
    #endif
    #define APP_TOTAL_FREE_SPACE eval(APP_PAGE_FREE_SPACE+APP_TOTAL_FREE_SPACE)
    #define APP_PAGE_FREE_SPACE eval($8000-($ & $0000FFFF))
    #if APP_PAGE_FREE_SPACE < 0
        .error "Page ",APP_PAGE," ran out of space, overran by ",$-$8000," bytes."
    #endif
    #define APP_PAGE eval(APP_PAGE+1)
    .echo "Page ",APP_PAGE," started. Previous page size ",($&$0000FFFF)-$4000 ," with ",APP_PAGE_FREE_SPACE," bytes leftover."
    .block APP_PAGE_FREE_SPACE
    .org $4000 + ($10000 * APP_PAGE)
#endmacro

;It's not necessary to end the app with this, but it does provide final page
;validation and warns you in case you run into that edge case where you
;can't sign the app due to insufficient space on the final page.
#macro endapp()
    #ifndef APP_PAGE
        .error "You must first initialize the app via initapp()"
    #endif
    #define APP_ENDED
    #define APP_TOTAL_FREE_SPACE eval(APP_PAGE_FREE_SPACE+APP_TOTAL_FREE_SPACE)
    #define APP_PAGE_FREE_SPACE eval($8000-($ & $0000FFFF))
    #if APP_PAGE_FREE_SPACE < 0
        .error "Page ",APP_PAGE," ran out of space, overran by ",$-$8000," bytes."
    #endif
    .echo "Page ",APP_PAGE," finalized. This page size ",($&$0000FFFF)-$4000, " with ",APP_PAGE_FREE_SPACE," bytes leftover."
    .echo "App has ",APP_PAGE+1," page(s). Total leftover space: ",APP_TOTAL_FREE_SPACE," bytes."
    #if APP_PAGE_FREE_SPACE < 70
        .warn "Possibly insufficient space remaining for app signature."
        .warn "Your app might not successfully sign. You should have at"
        .warn "least 70 bytes of last page free space to fit the signature."
    #endif
#endmacro

#macro assert(condition, message_if_failed)
    #if eval((condition) < 1)
        #define errmsg eval(message_if_failed)
        #define err "Assert failed: ", errmsg
        .error err
    #endif
#endmacro

#macro memclear(adr, size)
    #ifndef MEMCLEAR_USES
        #define MEMCLEAR_USES 1
    #else
        #define MEMCLEAR_USES eval(MEMCLEAR_USES + 1)
    #endif
    ld hl,adr
    ld de,adr+1
    ld bc,size-1
    ld (hl),0
    ldir
#endmacro

#macro varloc(name)
    #define VAR_STARTLABEL eval(name)
    ;.echo "Name: ",VAR_STARTLABEL
    #define VAR_STARTNAME concat(name)
    #define VAR_STARTBASE eval(VAR_STARTNAME + 0)
    #define VAR_CURBASE eval(VAR_STARTNAME + 0)
#endmacro

#macro var(size)
    #ifndef VAR_CURBASE
        .error "Variable declared with no base location. Use varloc()"
    #endif
    .equ    VAR_CURBASE
    #define VAR_CURBASE eval(VAR_CURBASE + size)
#endmacro

#macro varvalidate(size_limit)
    #ifndef VAR_CURBASE
        .error "Attempted validation with no base location. Use varloc()"
    #endif
    #define VAR_ENDSIZE eval(size_limit-(VAR_CURBASE-VAR_STARTBASE))
    #if VAR_ENDSIZE < 0
        .error "Overran ", VAR_STARTLABEL ," by ", -VAR_ENDSIZE ," bytes."
    #endif
    .echo "Validated ", VAR_STARTLABEL ,", leftover bytes: ", VAR_ENDSIZE
    #undef VAR_STARTLABEL
    #undef VAR_STARTNAME
    #undef VAR_STARTBASE
    #undef VAR_CURBASE
    #undef VAR_ENDSIZE
#endmacro



#macro show_stats()
    .echo "--- Displaying macro use statistics block ---"
    #ifdef MEMCLEAR_USES
        .echo "Memory clear macro use count: ", MEMCLEAR_USES
    #else
        .echo "Memory clear macro not used."
    #endif
#endmacro

;Relocate macros transcribed from relocate.inc.
;These form relocatable blocks; their purpose being that any code or macro
;within that relies on $ for evaluation will have a new origin. The actual
;location is still within the app, so you'll need to copy the relocatable
;code at runtime. Make sure you have the appropriate labeling. Example:
;
;myRoutine_blockstart:
;relocate(appbackupscreen)
;myRoutine:
;   relocatable code goes here
;endrelocate()
;myRoutine_blockend:
;...
;moveMyCodeRoutine:
;   ld  hl,myRoutine_blockstart
;   ld  de,appbackupscreen      ;(myRoutine is fine too since they match in this example)
;   ld  bc,myRoutine_blockend-myRoutine_blockstart
;   ldir
;
#macro relocate(newloc)
    #ifdef APP_RELOCATE_OLD
        .error "You may not nest relocate() blocks."
    #else
        #define APP_RELOCATE_OLD eval($)
        .org newloc
        #define APP_RELOCATE_NEW eval(newloc)
    #endif
#endmacro
#macro endrelocate()
    #ifndef APP_RELOCATE_NEW
        .error "You need to start a relocate() block before ending it."
    #else
        .org $ - APP_RELOCATE_NEW + APP_RELOCATE_OLD
        #undef APP_RELOCATE_NEW
        #undef APP_RELOCATE_OLD
    #endif
#endmacro


;Use this to create an offpage branch by specifying the destination.
;When you use bcall(), prepend the label with an underscore to use the branch.
;For example:
;
; branch(myRoutine) ;Put this someplace. Most people put it near the header.
;...
; bcall(_myRoutine)
;...
;myRoutine:
; your offpage code here
;
#macro branch(lbl)
    #define underscorelabel concat("_",lbl)
    #if $ < $8000
        underscorelabel .equ ($ & $0FFF)
    #elif $ > $7FFF
        underscorelabel .equ ($ | $C000)
    #else
        underscorelabel .equ $
    #endif
    .dw lbl
    .db getpage(lbl)
#endmacro

#define .branch(lbl) branch(lbl)
#define .initapp initapp
#define .nextpage nextpage
#define .newpage .nextpage
#define .endapp endapp
#define .show_stats show_stats
#define .varloc(label) varloc(label)
#define .var(size) var(size)
#define .varvalidate(size_limit) varvalidate(size_limit)
#define .setvarbase(label) setvarbase(label)
#define .assert(cond, errmsg) assert(cond, errmsg)
#define .relocate(newloc) relocate(newloc)
#define .endrelocate() endrelocate()


#define MACROS_LOADED