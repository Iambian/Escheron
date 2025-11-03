# Second revision of the parser/assembler simulating SPASM-ng. Mostly.
#
#

import os, sys, random, copy, re, colorama
import traceback
from typing import NamedTuple, Iterator, Optional, Tuple
from collections import deque
from colorama import Fore, Style, Back

colorama.just_fix_windows_console()

ERRMSG_RIGHT_ALIGN = True
ERRMSG_RIGHT_ALIGN_COL = 40

class SubToken(NamedTuple):
    type: str
    value: str
    position: int

class Token(NamedTuple):
    type: str
    v: str
    col: int
    row: int
    file: str


class IfStackEntry(NamedTuple):
    ''' `token` for traceback. `cond` means the following:
    
    * `True` = The current branch is being taken.
    * `False` = The current is not being taken but is still available to take.
    * `None` = A branch has already been taken and no more at this level will take.
    '''
    token: Token            #Added to allow error tracing.
    cond: Optional[bool]

def from_token(token:Token, toktype:str, value:int|str):
    return Token(toktype, value, token.col, token.row, token.file)
def tokline2str(tokens:"list[Token]|TokenStream"):
    if isinstance(tokens,TokenStream):
        tokens = tokens.tokens
    return ' '.join([t.v if isinstance(t.v,str) else str(t.v) for t in tokens])
def tokline2minitok(tokens:"list[Token]|TokenStream"):
    if isinstance(tokens,TokenStream):
        tokens = tokens.tokens
    a = [f"[TOK:{t.type}, V:{repr(t.v)}]" for t in tokens]
    return ' '.join(a)
NEWLINE_TOKEN = Token("NEWLINE", "\n", 0, 0, "")



class MacroDef(NamedTuple):
    name: Token             # The token used to identify this macro
    params: tuple[Token]    # Only one token per param, each an IDENT type.
    body: "TokenStream"     # Yeah. This tracks.

def redmsg(msg):
    return f"{Fore.RED}{msg}{Style.RESET_ALL}"
def yellowmsg(msg):
    return f"{Fore.YELLOW}{msg}{Style.RESET_ALL}"
def warnmsg(token:Token, msg:str):
    prerun = f"[{token.file}: LN {token.row}, COL {token.col}]"
    if ERRMSG_RIGHT_ALIGN and len(prerun) < ERRMSG_RIGHT_ALIGN_COL:
        prerun = ' '*(ERRMSG_RIGHT_ALIGN_COL-len(prerun)) + prerun
    return yellowmsg(f"{prerun} {msg}")
def errmsg(token:Token, msg:str):
    prerun = f"[{token.file}: LN {token.row}, COL {token.col}]"
    if ERRMSG_RIGHT_ALIGN and len(prerun) < ERRMSG_RIGHT_ALIGN_COL:
        prerun = ' '*(ERRMSG_RIGHT_ALIGN_COL-len(prerun)) + prerun
    return redmsg(f"{prerun} {msg}")
def err(token:Token, msg:str):
    raise ValueError(errmsg(token, msg))
def printerr(token:Token, msg:str):
    print(errmsg(token, msg))


class Parser(object):
    DEBUGMODE = True
    SHOW_SYMTABLE = True
    SHOW_SYMTABLE_MODE = None    #None|"SYM"|"MAC"
    SHOW_PARSE_LINESTART = True
    SHOW_PARSE_AFTER_PREOP = False
    SHOW_RESUBMISSIONS = True
    SHOW_RESULTS_RETURN = True
    MAX_RECURSION_DEPTH = 8
    MATH_OPS = ('+','-','*','/','&','|','^','<<','>>','==','!=','<','>','<=','>=','&&','||')
    def __init__(self, tokendata:"TokenStream"):
        ''' NOTE: self.parse() should, when complete, outputs binary data from
            data directives and instructions to self.binres during pass 2
            and depth 1.
        '''
        print("Starting...")
        self.tokens = tokendata
        self.symtable:dict[str, int|str|MacroDef] = dict()
        self.origin = 0
        self.binres:bytearray = bytearray()
        self.if_stack:list[IfStackEntry] = []
        try:
            self.parse(self.tokens, 1)

            if len(self.if_stack):
                baseentry = self.if_stack[0]
                err(baseentry.token, "Unbalanced #IF/#ENDIF statements starting here")
        except Exception as e:
            print(Fore.RED)
            traceback.print_exc()
            print(Style.RESET_ALL)

        #self.parse(self.tokens, 2)
        if self.__class__.DEBUGMODE and self.__class__.SHOW_SYMTABLE:
            mode = self.__class__.SHOW_SYMTABLE_MODE
            print("--Symbol Table--")
            for k in self.symtable:
                v = self.symtable[k]
                if not isinstance(v, MacroDef) and not mode=="MAC":
                    print(f"Symdef [{k}]: {repr(v)}")
                elif isinstance(v, MacroDef) and not mode=="SYM":
                    strparams = f"({', '.join([str(t.v) for t in v.params])})"
                    strbody = ' '.join([str(t.v) for t in v.body.tokens])
                    print(f"Macrodef [{k}] params: {strparams}")
                    print(f"{strbody}")
        pass

    @classmethod
    def from_filename(cls, filename):
        return cls(TokenStream.from_filename(filename))

    def parse(self, tokens:"TokenStream", passid=1, depth=1, trace=False, nontrival_expand=True) -> list[Token]:
        #All parsed lines must be executed immediately
        cls = self.__class__
        if depth > cls.MAX_RECURSION_DEPTH:
            raise ValueError(redmsg("Maximum recursion depth reached."))
        iterator = tokens.getline()
        results:list[Token] = []
        for lidx,line in enumerate(iterator):
            if len(line) < 1:
                continue
            if cls.DEBUGMODE and cls.SHOW_PARSE_LINESTART:
                print(errmsg(line[0], f"LSTRT: {'·'*(depth-1)} {tokline2str(line)}"))
            if len(tokline2str(line)) > 128:
                err(line[0],"Line buffer exceeded reasonable length.")
            lineiter = iter(line)
            token0 = next(lineiter)
            token0v, token0t = (token0.v.upper(), token0.type)
            #
            # Begin handling flow control preops
            #
            if token0t == "PREOP":
                # First handle conditionals.
                if token0v in ("#ENDIF", "#ELSE", "#ELIF") and len(self.if_stack) < 1:
                    err(token0, f"{token0v} used without corresponding #IF/#IFDEF/#IFNDEF")
                if token0v == "#ENDIF":
                    self.if_stack.pop()
                    continue
                if token0v == "#ELSE":
                    current_entry = self.if_stack[-1]
                    if current_entry.cond is False:
                        # If false (skipped IF/ELIF with no change), unconditionally toggle to true
                        self.if_stack[-1] = IfStackEntry(token0, True)
                    else:
                        # Otherwise (True/None) becomes stuck to None.
                        self.if_stack[-1] = IfStackEntry(token0, None)
                    continue
                if token0v == "#ELIF":
                    if len(line) < 2:
                        err(token0, f"Missing parameters for preop {token0.v}")
                    ifexpr = self.parse(TokenStream(line[1:]), 2, depth+1, trace)
                    ifresult = True if self.eval_expr(ifexpr, 2) != 0 else False
                    current_entry = self.if_stack[-1]
                    if current_entry.cond is False:
                        # If false, consider if we need to toggle to true.
                        if ifresult is True:
                            self.if_stack[-1] = IfStackEntry(token0, True)
                    else:
                        # Otherwise, (True/None) now becomes stuck to None
                        self.if_stack[-1] = IfStackEntry(token0, None)
                    continue
            #
            # Perform flow control, then handle other preops
            #
            if len(self.if_stack) and not self.if_stack[-1].cond:
                continue
            mtokpreop = ("#IF", "#IFDEF", "#IFNDEF", "#DEFINE", "#MACRO", "#UNDEF")
            if token0t == "PREOP" and token0v in mtokpreop:
                if len(line) < 2:
                    err(token0, f"Missing parameters for preop {token0.v}")
                token1 = line[1]
                token1v, token1t = (token1.v, token1.type)
                if token0v in ("#IFDEF", "#IFNDEF"):
                    r = self.is_macro_defined(token1v)
                    if token0v == "#IFNDEF":
                        r = not r
                    self.if_stack.append(IfStackEntry(token0, r))
                    continue
                if token0v == "#IF":
                    ifexpr = self.parse(TokenStream(line[1:]), 2, depth+1, trace)
                    ifresult = True if self.eval_expr(ifexpr, 2) != 0 else False
                    self.if_stack.append(IfStackEntry(token0, ifresult))
                    continue
                if token0v in ("#UNDEF", "#UNDEFINE"):
                    if token1v in self.symtable:
                        del self.symtable[token1v]
                    if token1v + '(' in self.symtable:
                        del self.symtable[token1v + '(']
                    continue
                if token0v == "#ENDMACRO":
                    err(token0, "#ENDMACRO used without corresponding #MACRO.")
                #
                # The macro definitions start here.
                #
                if token0v in ("#DEFINE", "#MACRO"):
                    if token1t not in ("IDENT", "MACRO", "DIRECTIVE", "DIR_CALL"):
                        err(token1, f"Invalid identifier {token1v} used as argument to {token0v}.")
                    if token1v in ("eval(","concat(","eval","concat"):
                        err(token1, f"Illegal redefinition of reserved macro name {token1v}")
                    next(lineiter)  #Advance iter past token1
                    if token1v.endswith('('):
                        # If it has parameters, obtain them.
                        paramlist = self.get_params_from_iter(lineiter)
                        if paramlist != [[]]:
                            if any([len(param) < 1 for param in paramlist]) or any([param[0].type != "IDENT" for param in paramlist]):
                                err(token0, "Illegal parameter formation on this line. Check commas.")
                            paramlist = [param[0] for param in paramlist]
                        else:
                            paramlist = []
                    else:
                        # Or if there are no parameters, don't.
                        paramlist = []
                    #Either way, the macrobody is afterward. If it's a #DEFINE.
                    macrobody = [t for t in lineiter if t.type != "NEWLINE"]
                    if token0v == "#DEFINE":
                        # Perform macro expansion and then evaluate it.
                        # The expansion here should NOT expand eval( and concat(
                        # Otherwise, proceed with evaluation as before
                        try:
                            print(f"#DEFINE DEBUG STEP0: {token1v}: {tokline2minitok(macrobody)}")
                            macrobody = self.parse(TokenStream(macrobody), passid, depth+1, trace, False)
                            print(f"#DEFINE DEBUG STEP1: {token1v}: {tokline2minitok(macrobody)}")
                            if len(paramlist):
                                raise Exception("This exception is just to prevent eval if macro has parameters.")
                            macrobody = self.eval_expr(macrobody, 2)
                            print(f"#DEFINE DEBUG STEP2: {token1v}: {macrobody}")

                        except Exception as e:
                            print(f"#DEFINE DEBUG EXCEPTION")
                            s = traceback.format_exc()
                            print(s)
                            #raise e
                            # Passthrough. macrobody will always have a valid result.
                            pass
                        if isinstance(macrobody, int):
                            # Simple define
                            macrodef = macrobody
                        elif isinstance(macrobody, str):
                            # Also simple define, but resolves to string.
                            macrodef = macrobody
                        elif isinstance(macrobody, Token):
                            err(token0, "This error should not be reachable. Output macrobody in #DEFINE is not supposed to be a Token type.")
                        else:
                            macrodef = MacroDef(token1, paramlist, TokenStream(macrobody))
                        if not isinstance(macrodef, MacroDef):
                            print(f"#DEFINE DEBUG STEP3: {token1v} assigned as {macrodef}")
                        else:
                            print(f"#DEFINE DEBUG STEP3: {token1v} assigned as {macrodef.params}: {tokline2minitok(macrodef.body.tokens)}")
                        self.symtable[token1v] = macrodef
                    if token0v == "#MACRO":
                        # Ignore contents of macrobody here. Anything following
                        # the signature on same line is ignored.
                        macrobody:list[Token] = []
                        while True:
                            try:
                                macroline = next(iterator)
                            except:
                                err(token0, "EOF in #MACRO block encountered without #ENDMACRO")
                            if len(macroline) < 1:
                                continue
                            m_token0 = macroline[0]
                            m_token0v = m_token0.v.upper()
                            m_token0t = m_token0.type
                            if m_token0t == "PREOP":
                                if m_token0v == "#MACRO":
                                    err(m_token0, "Illegal nesting of #MACRO statement.")
                                if m_token0v == "#ENDMACRO":
                                    if macrobody and macrobody[-1].type == "NEWLINE":
                                        macrobody = macrobody[:-1]  #strip final newline
                                    break
                            macrobody.extend(macroline+[NEWLINE_TOKEN])
                        macrodef = MacroDef(token1, paramlist, TokenStream(macrobody))
                        self.symtable[token1.v] = macrodef
                    continue
                pass    #End of preops
            #
            # Macro expansion code.
            # Perform as few evalulations here as possible.
            # Eval is supposed to be done in the next section.
            #
            if cls.DEBUGMODE and cls.SHOW_PARSE_AFTER_PREOP:
                print(errmsg(line[0], f"POSTP: {'·'*(depth-1)} {tokline2str(line)}"))
            #'''

            resubmit:list[Token] = []
            lineiter = iter(line)
            for token in lineiter:
                tokenv, tokent = (token.v, token.type)
                if tokenv == "VAR_STARTNAME":
                    if "VAR_STARTNAME" in self.symtable:
                        en = self.symtable["VAR_STARTNAME"]
                        if not isinstance(en, MacroDef):
                            print(f"MACROEXPANSION DEBUG STEP: {tokenv} assigned as {en}")
                        else:
                            print(f"MACROEXPANSION DEBUG STEP: {tokenv} assigned as {en.params}: {tokline2minitok(en.body.tokens)}")


                if tokent in ("MACRO","IDENT","DIR_CALL","DIRECTIVE") and tokenv in self.symtable:
                    symentry = self.symtable[tokenv]
                    if "VAR_STARTNAME" in self.symtable:
                        en = self.symtable["VAR_STARTNAME"]
                        if not isinstance(en, MacroDef):
                            print(f"MACROEXPANSION DEBUG STEP1: exparam {nontrival_expand} {tokenv} assigned as {en}")
                        else:
                            print(f"MACROEXPANSION DEBUG STEP1: exparam {nontrival_expand} {tokenv} assigned as {en.params}: {tokline2minitok(en.body.tokens)}")

                    if isinstance(symentry, MacroDef) and nontrival_expand:
                        print(f"Attempting to expand {tokenv}")
                        if tokenv.endswith('('):
                            invokelist = self.get_params_from_iter(lineiter)
                            if invokelist == [[]]:
                                invokelist = []
                        else:
                            invokelist = []
                        paramlist = [i.v for i in symentry.params]
                        if len(paramlist) > len(invokelist):
                            err(token, "Insufficient parameters passed to macro invocation.")
                        macrobody = []
                        for mtoken in symentry.body.tokens:
                            try:
                                if mtoken.v in paramlist:
                                    macrobody.extend(invokelist[paramlist.index(mtoken.v)])
                                else:
                                    macrobody.append(mtoken)
                            except Exception as e:
                                print("Exception triggered inside macrobody expansion")
                                # Might want to put something more here. There
                                # probably was since there used to be more above
                                raise e
                        # Attempt to further expand the macros inside macrobody
                        # Don't expand upon any eval() or concat() yet, tho.
                        try:
                            macrobody = self.parse(TokenStream(macrobody), passid, depth+1, trace, True)
                        except:
                            pass
                        resubmit.extend(macrobody)
                    else:
                        # Any other token must be passed through as-is.
                        resubmit.append(token)
                elif tokent == "MACRO" and tokenv.lower() in ("eval(", "concat("):
                    '''
                    if not nontrival_expand:
                        # Consume, reconstruct, then emit tokens to prevent
                        # any further expansions within.
                        t = self.joinparams(self.get_params_from_iter(lineiter))
                        resubmit.extend([token]+t)
                        continue
                    '''
                    # Execute built-in macros.
                    invokelist = self.get_params_from_iter(lineiter)
                    if any([param == [] for param in invokelist]):
                        err(token, f"Built-in macro [{tokenv}] may not contain empty parameters.")
                    if all([len(param) == 1 for param in invokelist]) and all([self.eval_to_string(param[0]) for param in invokelist]):
                        # Tested for string params.
                        s = ''.join([self.eval_to_string(param[0]) for param in invokelist])
                        if tokenv.lower() == "eval(":
                            newtoken = from_token(token, "STRING", s)
                        else:
                            print(f"PREEVALUATED CONCAT: {[self.eval_to_string(param[0]) for param in invokelist]}")
                            print(f"EVALUATED CONCAT: {s}")
                            newtoken = from_token(token, "IDENT", s)
                    elif len(invokelist) == 1:
                        print(tokline2minitok(invokelist[0]))
                        reparsed = self.parse(TokenStream(invokelist[0]), passid, depth+1, trace, True)
                        if len(reparsed):
                            testtok = reparsed[0]
                            if testtok.v == "VAR_STARTNAME":
                                if testtok.v in self.symtable:
                                    print(f"SYMVAL {testtok.v} = {self.symtable[testtok.v]}")
                                else:
                                    print(f"SYMVAL {testtok.v} referenced but not found.")
                        print(tokline2minitok(reparsed))
                        newtoken = from_token(token, "NUM", str(self.eval_expr(reparsed, 2)))
                    else:
                        if cls.DEBUGMODE:
                            print(yellowmsg(tokline2minitok(line)))
                        err(token, f"Illegal use of [{token.v}].")
                    resubmit.append(newtoken)
                else:
                    # TODO: Write comment to describe this situation. Had to change
                    # things so that nontrivial expansions are also excludable,
                    # not just built-ins.
                    resubmit.append(token)
                pass
            # If nothing expanded, line should be the same as resubmit.
            # If it did expand, append a newline as that would have been
            # stripped by the TokenStream() iter function.

            if self.strip_token_meta(line) != self.strip_token_meta(resubmit):
                if cls.DEBUGMODE and cls.SHOW_RESUBMISSIONS:
                    try:
                        print(errmsg(line[0],f"RESUB: [{tokline2str(line)}] to [{tokline2str(resubmit)}]"))
                        print(f"RESUBIN : {tokline2minitok(line)}")
                        print(f"RESUBOUT: {tokline2minitok(resubmit)}")
                    except:
                        print("The debug line at resubmit encountered an error.")
                        print(line)
                        print(yellowmsg(resubmit))
                        pass
                if resubmit and resubmit[-1].type != "NEWLINE":
                    resubmit.append(NEWLINE_TOKEN)
                tokens.resubmit_tokens(resubmit)
                continue
            #
            # Instruction and directive parsing goes here.
            # Do not worry about processing in depth > 2, not even .equ.
            # Since redefinition is disallowed with = or .equ, any macros that
            # would forward-reference such a thing would fail/pass through due
            # to missing symtable entries, or error out later due to redef.
            #
            # More information to follow, copied from old source.
            #
            # Instruction and directive parsing goes here.
            # NOTE: Standalone labels, with or with trailing colon, or the same
            # except with instruction/directive afterward should alias to
            # IDENTIFIER .equ $ \ [leftover line contents]
            # and resubmit edited line to tokens.
            # NOTE: If depth > 1, do not do any actual processing. Just let it
            # pass through to results.
            # NOTE: If depth == 1, advance .org as normal. On pass 2, output
            # byte data to self.results (distinct to local results) and let
            # local results be an empty list.
            types = [i.type for i in line]
            values = [i.v for i in line]
            if "DIRECTIVE" in types and depth < 2:
                #print(yellowmsg("HANDLE DIR ↑"))
                if sum([1 if i=="DIRECTIVE" else 0 for i in types]) > 1:
                    err(line[0],"There may not be more than one directive on a single logical line.")
                diridx = types.index("DIRECTIVE")
                dirtok = line[diridx]
                dirid = values[diridx].upper()
                if dirid == ".EQU":
                    print(yellowmsg(f"Processing directive line: [{tokline2minitok(line)}]"))
                    if diridx != 1:
                        err(line[0], "Directive .EQU must have only one token prior.")
                    if line[0].type != "IDENT":
                        err(line[0], "Token prior to .EQU must be an identifier.")
                    exprval = self.eval_expr(line[2:], passid)
                    # Add in check to prevent redefinition but only on same-pass
                    self.symtable[line[0].v] = exprval
                elif dirid == ".ERROR":
                    # Red text echo, plus program halt.
                    print(yellowmsg("Warn: .error directive implementation incomplete."))
                    print(redmsg(".error directive encountered. Raising error using text available."))
                    err(token, f"{tokline2str(line)}")
                elif dirid == ".ECHO":
                    # You'll want to figure out how this is going to work.
                    pass
                elif dirid in (".DB", ".BYTE"):
                    # Data byte (8 bit) support
                    # Limits: Warn if byte < -128 or > 255
                    print(yellowmsg(f"DB DEBUG1: DB input {line[diridx+1]}"))
                    data = self.parse_bytestream(line[diridx+1:], 1)
                    print(yellowmsg(f"DB DEBUG2: DB output {data.hex()}"))
                    pass
                elif dirid in (".DW", ".WORD"):
                    # Data word (16 bit) support
                    # Strings pad each char to 16 bit.
                    # Limits apply. Warn if < INT16_MIN or > UINT16_MAX
                    pass
                elif dirid in (".DL", ".LONG"):
                    # Data long (24 bit) support
                    # Strings pad each char to 24 bit.
                    # Limits apply. Warn if < INT24_MIN or > UINT24_MAX
                    pass
                elif dirid in (".BLOCK", ".FILL"):
                    # They do the same thing in SPASM-ng.
                    # First param is length. It must be positive.
                    # Optional second param is byte to use. Defaults to 0.
                    pass
                elif dirid in (".LIST", ".NOLIST"):
                    # Toggle list modes including or excluding this current
                    # statement invocation.
                    pass
                elif dirid == ".ASSUME":
                    # might have more than one use. reviewing source.
                    pass
                elif dirid == ".SHOW":
                    # This presumably allows you to echo the contents of a
                    # DEFINE'd object in some manner that an echo can't do.
                    # You'll want to explore this one some more.
                    pass
                elif dirid == ".OPTION":
                    # idk what this does. At all. Reviewing source.
                    # After reviewing the source, I still don't know what
                    # it does. All I know is that it reads an expression with
                    # '=' as a delimiter, and that it creates a double-underscore
                    # define. In source we see the following:
                    # "set_define (define, expr, -1, false);
                    # That... appears "normal". Still idk why this exists.
                    pass
                elif dirid == ".SEEK":
                    # idk what this does. probably auxillary file I/O
                    # It does unsafe things. Maybe we should just error if
                    # we see it.
                    pass
                elif dirid == ".EXPORT":
                    # Label exporting
                    pass
                elif dirid == ".ADDINSTR":
                    #The syntax for this is very weird. See if it's resolvable.
                    pass
                elif dirid == ".WARN":
                    # Yellow text echo.
                    pass
                elif dirid == ".ORG":
                    # Sets self.origin
                    # Must be a positive number.
                    pass
                elif dirid == ".END":
                    # End source parsing. That is to say, simply stop.
                    pass
                else:
                    err(dirtok, "Unrecognized directive")


                


            # This final section is intended to collect emittable tokens during
            # a recursive call (macro expansion) and return them to complete
            # that expansion. All preops passed into this level should have been
            # processed, executed and filtered out by now. What we should have
            # is directives, instructions, and possibly standalone expressions.
            if depth > 1 and line:
                line.append(NEWLINE_TOKEN)
                results.extend(line)

        # Strip final newline from results for macro expansion insertion
        if results:
            t = results[-1]
            if isinstance(t, Token) and t.type == "NEWLINE":
                results.pop()

        if cls.DEBUGMODE and cls.SHOW_RESULTS_RETURN:
            print(errmsg(line[0], f"RERET: {'·'*(depth-1)} {tokline2str(line)}"))

        return results
    


    def parse_bytestream(self, tokenline:list[Token], bytewidth:Optional[int]) -> Optional[bytes]:
        #NOTE: if bytewidth is None, this outputs data to console.
        #NOTE: Extended echo syntax is not available.
        iterline = iter(tokenline)
        paramlist = self.get_params_from_iter(iterline, True)
        result = bytearray()
        if not paramlist or paramlist == [[]]:
            raise ValueError(redmsg("Empty expression in bytestream parsing is disallowed."))
        for param in paramlist:
            if not param:
                raise ValueError(redmsg("Empty parameter in bytestream parsing is disallowed."))
            if len(param) == 1:
                stringified = self.eval_to_string(param[0])
                if isinstance(stringified, str) and bytewidth is not None:
                    for c in stringified:
                        result.append(ord(c))
                        for _ in range(3-bytewidth,2):
                            result.append(0)
                    continue
                elif isinstance(stringified, str) and bytewidth is None:
                    result.extend(stringified.encode("ASCII"))

            exprval = self.eval_expr(param)
            if bytewidth is None:
                result.extend(str(exprval).encode("ASCII"))
            else:
                int_min = -(1 << ((8*bytewidth)-1))
                int_max = (1 << (8*bytewidth)-1)-1
                if exprval < int_min or exprval > int_max:
                    print(warnmsg(param[0],f"Expression evalulates outside bounds. Value {exprval} is being truncated."))
                #exprval = max(exprval, int_min)
                #exprval = min(exprval, int_max)
                result.extend(exprval.to_bytes(16,"little", True if exprval<0 else False)[:bytewidth])
        return result

    def strip_token_meta(self, tokenin:Token|list[Token]):
        if isinstance(tokenin, Token):
            return [(tokenin.type, tokenin.v)]
        return list(zip([t.type for t in tokenin], [t.v for t in tokenin]))

    def joinparams(self, paramlist:list[list[Token]]):
        tokenlist = []
        for param in paramlist:
            tokenlist.extend(param)
            tokenlist.append(Token("COMMA",",",0,0,''))
        if not tokenlist:
            tokenlist = [None]  #Placeholder value for overwriting
        tokenlist[-1] = Token("OPER",")",0,0,'')
        return tokenlist
    
    def eval_to_string(self, token:Token) -> Optional[str]:
        if token.type == "STRING":
            return token.v
        if token.type == "IDENT":
            if token.v in self.symtable:
                symentry = self.symtable[token.v]
                if isinstance(symentry, str):
                    return symentry
        return None

    def get_params_from_iter(self, iterable:Iterator[Token], omit_close_paren:bool = False) -> list[list[Token]]:
        parenlevel = 0
        paramlist = []
        tokens = []
        for item in iterable:
            if item.type == "OPER":
                if item.v == "(":
                    parenlevel += 1
                elif item.v == ")":
                    parenlevel -= 1
                    if parenlevel < 0:
                        if omit_close_paren:
                            err(item, "Too many close parentheses found.")
                        if not tokens and paramlist:
                            err(item, "[TRAILING] Empty parameters are disallowed.")
                        paramlist.append(tokens)
                        break
            if item.type == "COMMA" and parenlevel == 0:
                if not tokens:
                    err(item, "[INNER] Empty parameters are disallowed.")
                paramlist.append(tokens)
                tokens = []
                continue
            tokens.append(item)
        else:
            if not item:
                raise ValueError(redmsg("No tokens after expansion invocation."))
            if not tokens:
                err(item, "[TRAILING] Empty parameters are disallowed.")
            if not omit_close_paren:
                err(item, "Macro invocation has no matching closing parenthesis.")
        return paramlist

    def is_macro_defined(self, name_without_open_paren) -> bool:
        if name_without_open_paren in self.symtable:
            return True
        if name_without_open_paren + '(' in self.symtable:
            return True
        return False
    
    def eval_expr(self, tokenline:list[Token], passnum:int=1) -> int:
        ''' Parses an expression in tokenline from left-to-right. Operator
            precedence is NOT respected other than parentheses. 
            Supported operations:

            * Unary `+`, `-`, `~`
            * Binary `+`, `-`, `*`, `/`, `&`, `|`, `^`, `&&`, `||`, `==`, `!=`, `>=`, `<=`
            * Parentheses
            * `NUM` type tokens and `IDENT` type tokens that resolve to an `int`.
            * `$` operator, which resolves to `self.origin`
        '''
        tokens = tokenline
        if len(tokens) == 0:
            raise ValueError(redmsg("eval_expr() called without any input tokens."))
        
        baseval:int = None
        operator:Token = None
        unary:Token = None

        tokseg_idx = 0
        while tokseg_idx < len(tokens):
            token = tokens[tokseg_idx]

            # Handle unary operators
            if token.type == "OPER" and token.v in ('+', '-', '~') and (baseval is None or operator is not None):
                if unary is not None: # Cannot have two unary operators in a row
                    err(token, "Invalid unary operator sequence")
                unary = token
                tokseg_idx += 1
                continue

            current_value:int = None
            if "NUM" in token.type or token.type == "IDENT" or (token.type == "OPER" and token.v == '$') or token.type == "IDENT":
                current_value = self.eval_val(token, passnum)
                tokseg_idx += 1
            elif token.type == "OPER" and token.v == '(':
                # Handle parenthesized expression
                paren_level = 0
                start_paren_pos = tokseg_idx
                sub_expr_start_idx = tokseg_idx + 1
                
                # Find matching parenthesis
                temp_idx = tokseg_idx
                while temp_idx < len(tokens):
                    temp_token = tokens[temp_idx]
                    if temp_token.v == '(':
                        paren_level += 1
                    elif temp_token.v == ')':
                        paren_level -= 1
                    
                    if paren_level == 0: # Found matching closing parenthesis
                        sub_expr_end_idx = temp_idx - 1
                        tokseg_idx = temp_idx + 1 # Advance main index past ')'
                        break
                    temp_idx += 1
                else: # Loop finished without finding matching parenthesis
                    err(temp_token, "Closing parenthesis not found.")

                if sub_expr_start_idx > sub_expr_end_idx:
                    current_value = 0 # Empty parenthesis sequence
                else:
                    sub_tokline = tokens[sub_expr_start_idx:sub_expr_end_idx + 1] # +1 for exclusive end
                    current_value = self.eval_expr(sub_tokline, passnum)  # Recursive call
            else:
                err(token, f"Unexpected token {token.v} of type {token.type}")

            # Apply unary operator to current_value if present
            if unary is not None:
                if unary.v == "-":
                    current_value = -current_value
                elif unary.v == '~':
                    current_value = ~current_value
                unary = None

            # Combine with baseval
            if baseval is None:
                baseval = current_value
            elif operator is not None:
                baseval = self.eval_pair(baseval, operator, current_value)
                operator = None
            else:
                err(token, "Missing operator before value.")

            # If there are more tokens, check for the next operator
            if tokseg_idx < len(tokens):
                next_token = tokens[tokseg_idx]
                if next_token.type == "OPER" and next_token.v in self.MATH_OPS:
                    if operator is not None: # Cannot have two binary operators in a row
                        err(next_token, "Invalid operator sequence encountered.")
                    operator = next_token
                    tokseg_idx += 1
                elif next_token.type == "OPER" and next_token.v in ('(', ')'):
                    # Parentheses are handled by the main loop, not as binary operators here
                    # This case should ideally not be reached if parsing is correct,
                    # as '(' would start a new value and ')' would end a sub-expression.
                    # If it's reached, it's implies a structural issue or an unexpected token.
                    err(next_token, "Unexpected parenthesis encountered.")
                elif next_token.type == "IDENT" or "NUM" in next_token.type:
                    err(next_token, "Missing operator between values.")
                else:
                    # Non-expression token, end of expression
                    break
        if operator is not None:
            err(operator, "Incomplete expression detected at around this position.")
        if baseval is None:
            err(token, "Malformed expression or expression does not yield a value at around this position.")
        return baseval
    
    def eval_pair(self, base:int, oper:Token, nval:int) -> int:
        if oper.v == "+":
            base += nval
        elif oper.v == "-":
            base -= nval
        elif oper.v == "*":
            base *= nval
        elif oper.v == "/":
            if nval == 0:
                err(oper, "Division by zero")
            base //= nval # Integer division
        elif oper.v == "&":
            base &= nval
        elif oper.v == "|":
            base |= nval
        elif oper.v == "^":
            base ^= nval
        elif oper.v == "<<":
            base <<= nval
        elif oper.v == ">>":
            base >>= nval
        elif oper.v == "==":
            base = 1 if base == nval else 0
        elif oper.v == "!=":
            base = 1 if base != nval else 0
        elif oper.v == "<":
            base = 1 if base < nval else 0
        elif oper.v == ">":
            base = 1 if base > nval else 0
        elif oper.v == "<=":
            base = 1 if base <= nval else 0
        elif oper.v == ">=":
            base = 1 if base >= nval else 0
        elif oper.v == "&&":
            base = 1 if (base != 0 and nval != 0) else 0
        elif oper.v == "||":
            base = 1 if (base != 0 or nval != 0) else 0
        else:
            err(oper, f"Unknown or unsupported operator '{oper.v}'")
        return base
    
    def eval_val(self, token:Token, passnum=1):
        toktype = token.type
        tokval = token.v
        if "NUM" in toktype:
            if isinstance(tokval, int):
                return tokval   #Already processed
            tokval = token.v.upper()
            if tokval.startswith("$"):
                return int(tokval[1:], 16)
            elif tokval.startswith("%"):
                return int(tokval[1:], 2)
            else:
                v = tokval
                if tokval.endswith(('H','h','D','d','B','b')):
                    v = v[:-1]
                # Determine base based on token type suffix or default to 10
                if "HEX" in toktype:
                    return int(v, 16)
                elif "BIN" in toktype:
                    return int(v, 2)
                else:
                    return int(v, 10)
        elif toktype == "IDENT":
            if tokval not in self.symtable:
                if passnum == 1:
                    return 0
                else:
                    err(token, f"Symbol [{tokval}] not found.")
            else:
                symval = self.symtable[tokval]
                if not isinstance(symval, int):
                    err(token, f"Symbol [{tokval}] type expected int, got {type(symval)}")
                return self.symtable[token.v]
        elif token.type == "OPER" and token.v == "$":
            return self.origin
        else:
            err(token, f"Illegal token type {toktype} of value {tokval}")


class TokenStream(object):
    def __init__(self, tokenlist:list[Token]):
        self.tokens = tokenlist
        self.reset()
        #for tokenline in self.getline():
        #    print(" ".join([t.v for t in tokenline]))
        pass

    def reset(self):
        self.resub_tokens = deque()
        self.tokens_index = 0


    @classmethod
    def from_filename(cls, filename):
        return cls(Tokenizer.from_filename(filename).tokens)

    def resubmit_tokens(self, tokenlist:list[Token]):
        ''' Inserts tokens at the start of the stream so getline() next()
            can acquire them. Primary uses is to replay a recent line but
            with changes, which may include the results of a multiline macro.
        '''
        self.resub_tokens.extend(tokenlist)
        #

    def getline(self) -> Iterator[list[Token]]:
        def isnewline(token:Token, defining:bool=False) -> bool:
            if not defining and token.type == "OPER" and token.v == '\\':
                return True
            if token.type == "NEWLINE":
                return True
            return False
        def getline_core():
            tokens = []
            inside_define = False
            while self.tokens_index < len(self.tokens) or self.resub_tokens:
                #If there are any resubmitted tokens, consume them first.
                if self.resub_tokens:
                    token = self.resub_tokens.popleft()
                else:
                    token = self.tokens[self.tokens_index]
                    self.tokens_index += 1
                if tokens:
                    if isnewline(token, inside_define):
                        yield tokens
                        inside_define = False
                        tokens = []
                    else:
                        tokens.append(token)
                else:
                    if not isnewline(token):
                        if token.type == "PREOP" and token.v.upper() == "#DEFINE":
                            inside_define = True
                        tokens.append(token)
            if tokens:
                yield tokens
                tokens = []
        # Ensure that ALL tokens are consumed, even those resubmitted after the
        # token yielding loop has ended.
        while True:
            yielded_any = False
            for line in getline_core():
                yielded_any = True
                yield line
            if self.resub_tokens:
                continue
            if not yielded_any:
                break


class Tokenizer(object):
    MAX_INCLUDE_DEPTH = 8
    TOKEN_SPEC = [  
        # NOTE: Additional types added post-tokenization
        #   NUM: Processed number
        #   NEWLINE: Actual newline converted from list to stream
        # Strings (single or double quoted, with escapes)
        ("STRING",   r'"(?:\\.|[^"\\])*"' + r"|" + r"'(?:\\.|[^'\\])*'"),
        # Hex and binary numbers
        ("HEX_NUM",  r'(?:\$[0-9A-Fa-f]+|[0-9A-Fa-f]+[hH](?![A-Za-z0-9_]))'),
        ("BIN_NUM",  r'(?:%[01]+|[01]+[bB](?![A-Za-z0-9_]))'),
        ("DEC_NUM",  r'\d+(?:[dD](?![A-Za-z0-9_]))?'),
        # Function-like macro: identifier immediately followed by (
        ("MACRO",    r'[A-Za-z_][A-Za-z0-9_]*\('),
        # Directives (e.g., .define)
        ("DIR_CALL", r'\.[A-Za-z_][A-Za-z0-9_]*\('),
        ("DIRECTIVE",r'\.[A-Za-z_][A-Za-z0-9_]*'),
        # Preprocessor (e.g., #define)
        ("PREOP",    r'\#[A-Za-z_][A-Za-z0-9_]*'),
        # Identifiers
        ("IDENT",    r'[A-Za-z_][A-Za-z0-9_]*'),
        # Operators and special characters
        ("OPER",     r'==|<=|>=|&&|!=|\|\||\$|>>|<<|[+\-*/&|^~<>()=\\:]'),
        ("COMMA",    r','),
        # Whitespace (to be skipped)
        ("SKIP",     r'[ \t\r\n]+'),
        # Any unknown character
        ("UNKNOWN",  r'.'),
    ]
    TOK_REGEX = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in TOKEN_SPEC)
    get_token = re.compile(TOK_REGEX).match

    def __init__(self, tokendata=list[Token]):
        self.tokens:list[Token] = tokendata

    @classmethod
    def from_filename(cls, filename):
        return cls(cls.makestream(filename))

    @classmethod
    def makestream(cls, filename, depth=1):
        if depth > cls.MAX_INCLUDE_DEPTH:
            raise ValueError(redmsg("Include depth exceeded."))
        tokenstream = []
        filedata = cls.readfile(filename)
        if filedata is None:
            raise IOError(redmsg(f"File {filename} not found, depth {depth}"))
        for lineno, line in enumerate(filedata,1):
            try:
                tokenline = list(cls.tokenize(line))
            except Exception as e:
                print(redmsg(f"[{filename}:{lineno}] - Tokenization error. Re-raising original error."))
                raise e
            tokenline = [Token(i.type,i.value,i.position,lineno,filename) for i in tokenline]
            if len(tokenline) < 1:
                continue
            token0 = tokenline[0]
            if token0.type == "PREOP" and token0.v.upper() == "#INCLUDE":
                if len(tokenline) < 2:
                    err(token0,"No parameter found with #INCLUDE")
                token1 = tokenline[1]
                if token1.type != "STRING":
                    err(token1, f"Invalid #INCLUDE parameter :{token1.v}")
                includefile = token1.v
                try:
                    includestream = cls.makestream(includefile, depth+1)
                except Exception as e:
                    printerr(token0, f"Depth {depth}: Exception stack unwinding.")
                    raise e
                tokenstream.extend(includestream)
                continue
            tokenline.append(NEWLINE_TOKEN)
            tokenstream.extend(tokenline)
        return tokenstream
    
    @classmethod
    def tokenize(cls, line:str) -> Iterator[SubToken]:
        code = cls.strip_comment(line)
        pos = 0
        mo = cls.get_token(code, pos)
        while mo:
            kind = mo.lastgroup
            value = mo.group()
            if kind == "SKIP":
                pass
            elif kind == "UNKNOWN":
                raise SyntaxError(redmsg(f"Unexpected character {value!r} at position {pos}"))
            else:
                if kind == "STRING":
                    value = unescape_string(value)
                yield SubToken(kind, value, pos)
            pos = mo.end()
            mo = cls.get_token(code, pos)
        if pos != len(code):
            raise SyntaxError(redmsg(f"Unexpected character {code[pos]!r} at position {pos}"))

    @staticmethod
    def strip_comment(line: str) -> str:
        line = line.strip()
        in_string = False
        escape = False
        quote_char = ''
        for i, c in enumerate(line):
            if escape:
                escape = False
                continue
            if c == '\\':
                escape = True
                continue
            if in_string:
                if c == quote_char:
                    in_string = False
                continue
            if c in ('"', "'"):
                in_string = True
                quote_char = c
                continue
            if c == ';':
                return line[:i]  # cut off comment
        return line  # no comment

    @staticmethod
    def readfile(filename) -> Optional[list[str]]:
        try:
            with open(filename, "r") as f:
                return list(f.readlines())
        except:
            print(f"Failed to open {filename}")
            return None

# Not sure who's supposed to own this yet, but I'm getting the feeling that
# the answer is "everyone"
def unescape_string(raw: str) -> str:
    if len(raw) < 2 or raw[0] != raw[-1] or raw[0] not in ('"', "'"):
        raise ValueError(f"Invalid string literal {raw!r}")
    content = raw[1:-1]  # strip quotes
    result = []
    p = ''
    for i,c in enumerate(content):
        if c == '\\':
            p = c
            continue
        if p == '\\':
            tc = c.upper()
            if tc == "N":
                result.append('\n')
            elif tc == "R":
                result.append('\r')
            elif tc == "T":
                result.append('\t')
            elif tc == "0":
                result.append('\0')
            elif tc == "\\":
                result.append('\\')
            elif tc == "\'":
                result.append('\'')
            elif tc == "\"":
                result.append('\"')
            elif tc == "#":
                result.append(chr(random.randint(0,255)))
            else:
                result.append(c)
            p = ''
        else:
            result.append(c)
    else:
        if p == '\\':
            result.append(p)
    return ''.join(c for c in result)


if __name__ == "__main__":
    ts = Parser.from_filename("tools/macrotest.z80")
    pass
    '''
    if MacroAssembler.DEBUG_MODE:
        filename = "tools/macrotest.z80"
    else:
        if len(sys.argv) != 2:
            print("Usage: python macroparse.py <filename>")
            sys.exit(1)
        filename = sys.argv[1]
    
    asm = MacroAssembler(filename)
    asm.process()


    '''
#NOTE: This is how I invoke on the command line to output all that debug text
#   to a separate output file. Keep because we may lose it.
#
# python tools/parser3.py tools/macrotest.z80 > parser_output.txt
#
