; Removed app header from this include file.
; A modified header will instead be constructed
; via build script.

; Fatal error macro. Displays message prefixed with "error" 
; and returns with error code
#define fatal(str,return) .error str
#macro defpage(page, appname)
#if page = 0
  #ifndef gpage
    #define free_space 0
    #define gpage 0
    #ifndef appname
      .echo "Warning: No name given, using \"Default\""
      #define appname "Default"
    #endif
    .org $4000
	#define gappname eval(appname)
  #else
    fatal("There was already a page 0.",1)
  #endif
#else
  #if ($ & $0000FFFF) > $8000
   #define err "Page ",gpage," went over bounds by ",$-$8000," bytes."
   fatal(err, 1)
  #endif
  #if page <= gpage
    fatal("Your page numbers must increase.")
  #endif
  #if page > gpage+1
    .echo "Warning: Skipping page ",gpage+1," at user request."
  #endif
  #define free_space eval(free_space + $8000-($ & $0000FFFF))
  .echo "Page ",gpage," free space: ",$8000-($ & $0000FFFF)
  .block ($8000-($ & $0000FFFF) + ((page-gpage-1)*$4000))
  #define gpage eval(page)
  .org $4000+(gpage*$10000)
#endif
#endmacro
	
#macro validate
#if ($ & $0000FFFF) > $8000
  #define err "Page ",gpage," went over bounds by ",$-$8000," bytes."
  fatal(err, 1)
#else
  .echo "Page ",gpage," free space: ",$8000-($ & $0000FFFF)
  #define free_space eval(free_space + $8000 - ($ & $0000FFFF))
  .echo "Success: Page sizes validated and \"",gappname,"\" is ready for signing."
  .echo "           In ",gpage+1," page(s), ",free_space," bytes are available."
#endif
#endmacro

#macro echo_fill(times, char, base)
#if times > 0
 #define base base,char
 echo_fill(eval(times-1), char, base)
#else
 .echo base
#endif
#endmacro

#define .defpage defpage
#define .validate validate