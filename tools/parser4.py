# This is the fourth revision of that SPASM-ng simulator thingie.
# This is getting tiresome. Right. Let's go for consistency today.
#
#

import os, sys, random, re, colorama
import traceback
from typing import NamedTuple, Iterator, Optional, Tuple
from collections import deque
from colorama import Fore, Style, Back

colorama.just_fix_windows_console()

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
    def __repr__(self):
        return f"Token[{self.type}/'{self.v}']"

class IfDef(NamedTuple):
    #`token` for traceback. `cond` is ternary None:False:True. 
    # Means a branch (has been taken):(not been taken):(is being taken)
    token: Token
    cond: Optional[bool]

class MacroDef(NamedTuple):
    name: Token             # The token used to identify this macro
    params: tuple[Token]    # Only one token per param, each an IDENT type.
    body: list[Token]

class LabelDef(NamedTuple):
    token:Token     #The token that stores this ident
    value:Token     #The token that this ident resolves to. Usually a NUM.

class AnonDef(NamedTuple):
    token: Token    # The local label token. Using this for linepos 
    addr: int       # The address value assigned to this anonymous label

class SegmentDef(NamedTuple):
    name:str            # References. Default "__DEFAULT"
    baseaddr: int       # Segment starting address
    data: bytearray # Must stay contiguous. Enforce it or make more segments


ERRMSG_RIGHT_ALIGN = True
ERRMSG_RIGHT_ALIGN_COL = 40
NEWLINE_TOKEN = Token("NEWLINE", "\n", 0, 0, "")

def from_token(token:Token, toktype:str, value:int|str):
    return Token(toktype, value, token.col, token.row, token.file)
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
def tokline2mini(tokenline:list[Token]):
    return ''.join([f"{i.v}" for i in tokenline])
    return ' '.join([f"[{i.type}/{i.v}]" for i in tokenline])



class Parser(object):
    DEBUGMODE = False
    SHOW_SYMTABLE = False
    SHOW_SYMTABLE_MODE = None       #None|"SYM"|"MAC"|"ANON"
    SHOW_PARSE_LINESTART = True
    MAX_RECURSION_DEPTH = 12
    MATH_OPS = {'+','-','*','/','&','|','^','<<','>>','==','!=','<','>','<=','>=','&&','||'}
    UNARY_OPS = {'+','-','~'}
    Z80INST = {'ADC', 'ADD', 'AND', 'BIT', 'CALL', 'CCF', 'CP', 'CPD', 'CPI', 'CPIR', 'CPL', 'DAA', 'DEC', 'DI', 'DJNZ', 'EI', 'EX', 'EXX', 'HALT', 'IM', 'IN', 'INC', 'IND', 'INDR', 'INI', 'INIR', 'JP', 'JR', 'LD', 'LDD', 'LDI', 'NEG', 'NOP', 'OR',  'OTDR', 'OTIR', 'OUT', 'OUTD', 'OUTI', 'POP', 'PUSH', 'RES', 'RET', 'RETI', 'RETN' 'RL', 'RLA', 'RLC', 'RLCA', 'RLD', 'RR', 'RRA', 'RRCA', 'RRD', 'RST', 'SBC', 'SCF', 'SET', 'SLA', 'SLL', 'SRA', 'SRL', 'SUB', 'XOR'}
    #
    #
    #
    def __init__(self, tokens: "Tokenizer", oldsymtable=None, silent=False):
        if silent:
            self.__class__.DEBUGMODE = False
        if oldsymtable is None:
            oldsymtable = dict()
        self.tokens:list[Token] = tokens.tokens
        self.inclist:dict[str,list[Token]] = tokens.tokenfiles
        self.symtable: dict[str,MacroDef|Token] = dict()
        self.labels: dict[str,LabelDef] = dict() # Persists
        self.anonlabels: list[AnonDef] = list()  # Persists
        self.anonset:set[int] = set()   #Not persist. Used for fast redef check
        self.anonindex: int = -1     # Index of currently-found anonymous label
        #self.binres:bytearray = bytearray()
        self.origin:int = 0
        self.ifstack:list[IfDef] = list()
        self.curseg:Optional[SegmentDef] = None
        self.segments:dict[str, list[SegmentDef]] = dict()
        for passid in range(1,3):
            #self.binres = bytearray()
            self.labelshift = None  # If it ever becomes true, code has not stablized.
            self.curseg = None
            self.anonchar = '_'
            self.segments = dict()
            self.symtable = dict()
            self.symtable.update(oldsymtable)
            self.anonset = set()
            self.origin = 0
            self.anonindex = -1
            try:
                self.parse(self.tokens, passid)
                if len(self.ifstack):
                    err(self.ifstack[0].token, "Unbalanced #IF/ENDIF from here.")
                #print(f"Pass {passid} completed successfully.")
                #print(f"Output binary: {self.read_data()}")
            except:
                traceback.print_exc()
                print(yellowmsg(f"Pass {passid} ended with error(s)."))
                break
        else:
            if self.curseg is None:
                #Because it is possible we're assembling just for the symtable.
                outlen = 0
            elif isinstance(self.curseg, SegmentDef):
                outlen = len(self.curseg.data)
            else:
                raise ValueError(redmsg("Current segment variable was damaged. This error should not be possible."))
            if not silent:
                print(f"Two-pass assembly completed. Output {outlen} bytes, {len(self.symtable)} symbols.")
                print(f"Output data: {self.read_data().hex()}")
        if self.__class__.DEBUGMODE and self.__class__.SHOW_SYMTABLE:
            mode = self.__class__.SHOW_SYMTABLE_MODE
            self.printsym(mode)

        return


    def printsym(self, mode=None):
        print("--Symbol Table--")
        for k in self.symtable:
            v = self.symtable[k]
            if not isinstance(v, MacroDef) and not mode in ("MAC", "ANON"):
                print(f"Symdef [{k}]: {repr(v)}")
            if isinstance(v, MacroDef) and not mode in ("SYM", "ANON"):
                strparams = f"({', '.join([str(t.v) for t in v.params])})"
                strbody = ' '.join([str(t.v) for t in v.body])
                print(f"Macrodef [{k}] params: {strparams}")
                print(f"{strbody}")        
        if not mode in ("SYM", "MAC"):
            print("--Anonymous Labels--")
            for v in self.anonlabels:
                print(f"ANON [{v.addr:08x}]")

    @classmethod
    def from_filename(cls, filename):
        return cls(Tokenizer.from_filename(filename))
    
    def read_data(self, segname="__DEFAULT"):
        ''' NOTE: To read data from currently-selected segment, you must
            explicitly pass `None` into `segmentname`, otherwise it'll attempt
            to read `__DEFAULT`. It is done this way to hide the concept of
            segments when (in the majority of cases) it is not needed or wanted.
        '''
        if not self.curseg or self.curseg.name != segname:
            if not segname in self.segments:
                return bytearray()
            self.curseg = self.segments[segname]
        return self.curseg.data

    
    def parse(self, tokens:list[Token], passid=1, depth=1, nontrivial_expansion=True, force_eval=False):
        cls = self.__class__
        noflow_preop_tokenvals = {"#INCLUDE", "#IF", "#IFDEF", "#IFNDEF", "#DEFINE", "#MACRO", "#UNDEF"}
        tokenstream = TokenStream(tokens)
        tokenstreamiter = tokenstream.getline()
        results = []
        if depth > cls.MAX_RECURSION_DEPTH:
            raise ValueError(redmsg("Maximum recursion depth reached."))
        for line in tokenstreamiter:
            if len(line) < 1:
                continue
            token0, token0v, token0t = (line[0], line[0].v.upper(), line[0].type)
            if cls.DEBUGMODE and cls.SHOW_PARSE_LINESTART:
                #print(errmsg(line[0], f"LNSTRT: {'·'*(depth-1)} {tokline2str(line)}"))
                print(errmsg(line[0], f"LNSTRT: {'·'*(depth-1)} {tokline2mini(line)}"))
            lineiter = iter(line)   #Doing this eases parameter extraction
            if len(line) > 128:
                err(token0, "Line exceeds reasonable length")
                pass
            #if "VAR_STARTNAME" in self.symtable:
            #    print(f"State of VAR_STARTNAME: {self.symtable['VAR_STARTNAME']}")

            #===================================================================
            # HANDLE FLOW CONTROL PREOPS AND FLOW CONTROL
            if token0t == "PREOP":
                if token0v in {"#ENDIF", "#ELSE", "#ELIF"} and len(self.ifstack) < 1:
                    err(token0, f"{token0v} used without corresponding #IF/#IFDEF/#IFNDEF")
                if token0v == "#ENDIF":
                    self.ifstack.pop()
                    continue
                if token0v == "#ELSE":
                    cur_entry = self.ifstack[-1]
                    newcond = True if cur_entry.cond is False else None
                    self.ifstack[-1] = IfDef(cur_entry.token, newcond)
                    continue
                if token0v == "#ELIF":
                    # The below conditional is necessary for fullskip blocks
                    # where there are nested if/elif/else blocks. The condition
                    # may throw an error that they'd never otherwise thrown if
                    # the containing block is being executed instead of skipped.
                    if self.ifstack[-1].cond is not None:
                        if len(line) < 2:
                            err(token0, f"Missing parameters for preop {token0.v}")
                        ifexpr = self.parse(line[1:], 2, depth+1, force_eval=True)
                        ifresult = True if self.eval_val(self.eval_expr(ifexpr, 2)) != 0 else False
                        newcond = self.ifstack[-1].cond
                        if newcond is False:
                            if ifresult is True:
                                newcond = True
                        else:
                            newcond = None
                        self.ifstack[-1] = IfDef(self.ifstack[-1].token, newcond)
                    continue
            if len(self.ifstack) and not self.ifstack[-1].cond and not force_eval:
                if token0v in {"#IF", "#IFDEF", "#IFNDEF"}:
                    self.ifstack.append(IfDef(token0, None))
                continue
            # HANDLE OTHER PREOPERATORS.
            if token0t == "PREOP" and token0v in noflow_preop_tokenvals:
                if len(line) < 2:
                    err(token0, f"Missing parameters for preop {token0.v}")
                token1, token1v, token1t = (line[1], line[1].v, line[1].type)
                if token0v == "#INCLUDE":
                    if token1v not in self.inclist:
                        err(token1, f"Include file \"{token1v}\" not found.")
                    #print(f"Include list: {self.inclist.keys()}")
                    tokenstream.resubmit_tokens(self.inclist[token1v])
                    #self.parse(self.inclist[token1v], passid, depth+1, nontrivial_expansion, force_eval)
                    continue
                if token0v in {"#IFDEF", "#IFNDEF"}:
                    b = True if (token1v in self.symtable) or (token1v+'(' in self.symtable) else False
                    #print(f"IF(N)DEF TOK: {token1v} location verify: {b}")
                    b = b if token0v != "#IFNDEF" else not b
                    #print(f"IF(N)DEF reverified {b}")
                    self.ifstack.append(IfDef(token0, b))
                    continue
                if token0v == "#IF":
                    ifexpr = self.parse(line[1:], 2, depth+1)
                    ifres = True if self.eval_val(self.eval_expr(ifexpr,2)) != 0 else False
                    self.ifstack.append(IfDef(token0, ifres))
                if token0v in {"#UNDEF", "#UNDEFINE"}:
                    if token1v in self.labels:
                        err(token1,"You may not undefine a label not defined through #DEFINE.")
                    if token1v in self.symtable:
                        del self.symtable[token1v]
                    if token1v+'(' in self.symtable:
                        del self.symtable[token1v+'(']
                if token0v == "#ENDMACRO":
                    err(token0, "#ENDMACRO used without corresponding #MACRO.")
                #---------------------------------------------------------------
                # MACRO DEFINITIONS BEGIN HERE
                if token0v in {"#DEFINE", "#DEF", "#MACRO"}:
                    if token1v == self.anonchar:
                        err(token1, "You may not define a macro anonymously.")
                    #Features common to both #define and #macro. Do first.
                    if token1t not in {"IDENT", "MACRO", "DIRECTIVE", "DIR_CALL"}:
                        err(token1, f"Invalid identifier {token1v} used as argument to {token0v}.")
                    if token1v in ("eval(","concat(","eval","concat"):
                        err(token1, f"Illegal redefinition of reserved macro name {token1v}")
                    next(lineiter)  #Advance iter past token0
                    next(lineiter)  #Advance iter past token1
                    if token1v.endswith('('):
                        # If it has parameters, obtain them.
                        paramlist = self.get_params_from_iter(lineiter)
                        if paramlist != [[]]:
                            if any([len(param) < 1 for param in paramlist]) or any([param[0].type != "IDENT" for param in paramlist]):
                                print(yellowmsg(f"ERR: PARAMS {paramlist}"))
                                err(token0, "Illegal parameter formation on this line. Check commas.")
                            paramlist = [param[0] for param in paramlist]
                        else:
                            paramlist = []
                    else:
                        # Or if there are no parameters, don't.
                        paramlist = []
                    #Flush the remaining tokens after the definitions into body.
                    macrobody = [t for t in lineiter if t.type != "NEWLINE"]
                if token0v in {"#DEFINE", "#DEF"}:
                    #Expansion control
                    try:
                        if token1v.endswith('(') or "$" in [t.v for t in line if t.type == "OPER"]:
                            raise Exception("Do not show this error. This exists solely to cancel parse()/eval().")
                        macrobody = self.parse(macrobody, passid, depth+1)
                        macrobody = self.eval_expr(macrobody, -1)
                    except Exception as e:
                        s  = traceback.format_exc()
                        #print(s)
                    # Deal with expansion results
                    if isinstance(macrobody, Token):
                        macrodef = MacroDef(token1, paramlist, [macrobody])
                    elif isinstance(macrobody, list):
                        macrodef = MacroDef(token1, paramlist, macrobody)
                    else:
                        err(token0, f"Macrobody expansion failed with unknown result: {macrobody}")
                    #print(macrodef)
                    self.sym_assign(token1v, macrodef)
                if token0v == "#MACRO":
                    macrobody:list[Token] = []
                    while True:
                        try:
                            macroline = next(tokenstreamiter)
                        except:
                            err(token0, "EOF in #MACRO block encountered without corresponding #ENDMACRO")
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
                    macrodef = MacroDef(token1, paramlist, macrobody)
                    self.sym_assign(token1.v, macrodef)
                    pass
                continue
            #
            # NOTE: You may need to move first token label initializations here
            #   in case macro expansions needs to use that value in the same line.
            #   Maybe this isn't a problem. Be on the lookout for this problem.
            #
            #===================================================================
            # PERFORM MACRO EXPANSION BELOW
            if cls.DEBUGMODE:
                pass
            resubmit:list[Token] = []

            #'''
            for tokenidx, token in enumerate(lineiter):
                tokenv, tokent = (token.v, token.type)
                if tokent in {"MACRO","IDENT","DIR_CALL","DIRECTIVE"}:
                    if tokenv.endswith('('):
                        try:
                            invokelist = self.get_params_from_iter(lineiter)
                        except Exception as e:
                            print(line)
                            raise e
                        invokelist = [] if invokelist == [[]] else invokelist
                    else:
                        invokelist = []
                    if tokenv in self.symtable:
                        # Could be tokens or macrodefs.
                        symentry = self.symtable[tokenv]
                        if isinstance(symentry, Token):
                            # THis is a hail-mary addition.
                            if "NUM" in symentry.type:
                                # Do not expand variables.
                                resubmit.append(token)
                                continue
                        if isinstance(symentry, MacroDef):
                            #print(f"SYMTABLE MACRODEF: {tokenv}, ENTRY: {symentry}")
                            paramlist = [i.v for i in symentry.params]
                            if len(paramlist) > len(invokelist):
                                err(token, "Insufficient parameters passed to macro invocation.")
                            macrobody = []
                            for mtoken in symentry.body:
                                if mtoken.v in paramlist:
                                    macrobody.extend(invokelist[paramlist.index(mtoken.v)])
                                else:
                                    macrobody.append(mtoken)
                        else:
                            #print(f"SYMTABLE OTHER: {tokent}/{tokenv} -> {symentry}")
                            if invokelist:
                                err(token, f"Illegal use of parameters used with a non-parameterized macro.")
                            #macrobody = [symentry]
                            resubmit.append(token)  #PASSTHROUGH
                            continue
                        try:
                            #print(f"PUSH PARSE: {macrobody}")
                            macrobody = self.parse(macrobody, 2, depth+1)
                        except Exception as e:
                            print(f"{tokenv} ; {symentry}")
                            print(errmsg(token, f"DEPTH [{depth}]: Error in recursive parse. Unwinding."))
                            raise e
                        if isinstance(macrobody, Token):
                            #print(f"append: {macrobody}")
                            resubmit.append(macrobody)
                        else:
                            #print(f"extend: {macrobody}")
                            resubmit.extend(macrobody)
                    elif tokenv in self.labels and tokenidx:
                        # A label that's been assigned a value directly by .EQU
                        # whether it's this pass or a prior one.
                        #print(f"SYMTABLE NOFIRST REEVAL [{tokenv}] to {self.labels[tokenv].value}")
                        if tokenv.endswith('(') or tokent in {"MACRO","DIR_CALL","DIRECTIVE"}:
                            err(token, f"Macro expansion invalid type. Expected IDENT, found {tokent}")
                        resubmit.append(self.labels[tokenv].value)
                    elif tokenv.lower() in {"eval(", "concat("}:
                        # Built-in macros.
                        if any([param == [] for param in invokelist]):
                            err(token, f"Built-in macro [{tokenv}] may not contain empty parameters.")
                        if all([len(param) == 1 for param in invokelist]) and all([self.eval_to_string(param[0]) for param in invokelist]):
                            # Tested for string params.
                            s = ''.join([self.eval_to_string(param[0]) for param in invokelist])
                            if tokenv.lower() == "eval(":
                                newtoken = from_token(token, "STRING", s)
                            else:
                                newtoken = from_token(token, "IDENT", s)
                            #if cls.DEBUGMODE and cls.SHOW_EXPANSION_ACTIONS:
                            #    print(yellowmsg(f"Token [T:{token.type}]-{token.v} found, action: STRING EVAL/CONCAT"))

                        elif len(invokelist) == 1:
                            reparsed = self.parse(invokelist[0], passid, depth+1)
                            newtoken = self.eval_expr(reparsed, 2)
                            #if cls.DEBUGMODE and cls.SHOW_EXPANSION_ACTIONS:
                            #    print(yellowmsg(f"Token [T:{token.type}]-{token.v} found, action: EXPRESSION EVAL"))
                        else:
                            #if cls.DEBUGMODE:
                            #    print(yellowmsg(tokline2minitok(line)))
                            err(token, f"Illegal use of [{token.v}]. Parameters seen: {invokelist}")
                        resubmit.append(newtoken)
                    elif token.type in {"MACRO", "DIR_CALL"}:
                        # Unidentified macro with possible parameters? That's
                        # an actual error to raise.
                        err(token, f"MACRO or DIR_CALL [{tokenv}] was not found.")
                    else:
                        #If it didn't resolve, make it someone else's problem.
                        resubmit.append(token)
                        #err(token, f"MACRO or IDENT [{tokenv}] doesn't resolve.")
                else:
                    #Not a token eligible for expansion
                    resubmit.append(token)
                pass
            #'''


            if [(i.v,i.type) for i in line] != [(i.v,i.type) for i in resubmit]:
                #print(redmsg(f"RESUBM: {resubmit}"))
                if resubmit and resubmit[-1].type != "NEWLINE":
                    resubmit.append(NEWLINE_TOKEN)
                if any([isinstance(i, list) for i in resubmit]):
                    err(f"Illegal value made its way into resubmit: {resubmit}")
                tokenstream.resubmit_tokens(resubmit)
                continue

            #===================================================================
            # DIRECTIVE AND INSTRUCTION PARSING

            #NOTE: Here, '=' aliases to .EQU for assignment.
            if len(line) > 1 and line[1].type == "OPER" and line[1].v == "=":
                line[1] = from_token(line[1], "DIRECTIVE", ".EQU")
            types = [i.type for i in line]
            values = [i.v for i in line]
            if "DIRECTIVE" in types and depth == 1:
                if sum([1 if i=="DIRECTIVE" else 0 for i in types]) > 1:
                    err(line[0],"There may not be more than one directive on a single logical line.")
                diridx = types.index("DIRECTIVE")
                dirtok = line[diridx]
                dirid = values[diridx].upper()
                # Separate .EQU to allow processing leading labels separately
                if dirid == ".EQU":
                    if diridx != 1:
                        err(line[0], "Directive .EQU must have only one token prior.")
                    if line[0].type != "IDENT":
                        err(line[0], "Token prior to .EQU must be an identifier.")
                    labeltok = line[0]
                    #if labeltok.v == "VAR_STARTNAME":
                    #    print(yellowmsg(f"{self.labeltable[labeltok.v]}"))
                    #    err(labeltok, "DEBUG .EQU HALT CODE")
                    try:
                        exprval = self.eval_expr(line[2:], passid)
                    except Exception as e:
                        print(yellowmsg(f"DEBUG EQUATE TRAIL: {line[2:]}"))
                        raise e
                    self.label_inc_once(labeltok, exprval, passid)
                elif line[0].type == "IDENT":
                    #If literally anything else, check the first token to see
                    # if it's an identifier. If so, set label to value of origin
                    labeltok = line[0]
                    exprval = from_token(line[0], "NUM", str(self.origin))
                    self.label_inc_once(labeltok, exprval, passid)


                # Any leading labels with a directive embedded should have been
                # taken care of now.
                if dirid == ".ERROR":
                    # Red text echo, plus program halt.
                    print(yellowmsg("Warn: .error directive implementation incomplete."))
                    print(redmsg(".error directive encountered. Raising error using text available."))
                    err(token, f"{line}")
                elif dirid == ".ECHO":
                    data = self.parse_bytestream(line[diridx+1:], None, passid)
                    print(data.decode(encoding="ASCII", errors="ignore"))
                    # You'll want to figure out how this is going to work.
                    pass
                elif dirid in (".DB", ".BYTE"):
                    # Data byte (8 bit) support
                    # Limits: Warn if byte < -128 or > 255
                    data = self.parse_bytestream(line[diridx+1:], 1, passid)
                    self.write_data(data)
                    #print(f"Processing DB directives. Retrieved [{data}]")
                    pass
                elif dirid in (".DW", ".WORD"):
                    # Data word (16 bit) support
                    # Strings pad each char to 16 bit.
                    # Limits apply. Warn if < INT16_MIN or > UINT16_MAX
                    data = self.parse_bytestream(line[diridx+1:], 2, passid)
                    self.write_data(data)
                    pass
                elif dirid in (".DL", ".LONG"):
                    # Data long (24 bit) support
                    # Strings pad each char to 24 bit.
                    # Limits apply. Warn if < INT24_MIN or > UINT24_MAX
                    data = self.parse_bytestream(line[diridx+1:], 3, passid)
                    self.write_data(data)
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
                    expres = int(self.eval_expr(line[diridx+1:]).v)
                    if expres < 0:
                        err(dirtok, ".ORG expression must result in a positive number")
                    if expres > (1<<24)-1:
                        err(dirtok, ".ORG expressoin must be less than 01000000h")
                    self.origin = expres
                elif dirid == ".END":
                    raise StopIteration
                    # End source parsing. That is to say, simply stop.
                    pass
                elif dirid == ".EQU":
                    # This has already been processed, but this needs to be
                    # duplicated in this IF-ELIF-ELSE block to prevent
                    # .EQU from hitting the unrecognized directive error.
                    pass
                else:
                    err(dirtok, "Unrecognized directive")
            elif depth == 1:
                # If it's not a directive, then the only remaining thing left
                # that it could be is an instruction. We'll detect a leading
                # label and assign them accordingly, then figure out how to
                # implement instruction detection logic later.
                if line[0].type == "IDENT" and line[0].v.upper() not in cls.Z80INST:
                    exprval = from_token(line[0], "NUM", str(self.origin))
                    labeltok = line[0]
                    self.label_inc_once(labeltok, exprval, passid)
            ''''''
            # This final section is intended to collect emittable tokens during
            # a recursive call (macro expansion) and return them to complete
            # that expansion. All preops passed into this level should have been
            # processed, executed and filtered out by now. What we should have
            # is directives, instructions, and possibly standalone expressions.
            if depth > 1 and line:
                results.extend(line+[NEWLINE_TOKEN])

        # Strip final newline from results for macro expansion insertion
        if results:
            t = results[-1]
            if isinstance(t, Token) and t.type == "NEWLINE":
                results.pop()


        return results

    def joinparams(self, paramlist:list[list[Token]]):
        tokenlist = []
        for param in paramlist:
            tokenlist.extend(param)
            tokenlist.append(Token("COMMA",",",0,0,''))
        if not tokenlist:
            tokenlist = [None]  #Placeholder value for overwriting
        tokenlist[-1] = Token("OPER",")",0,0,'')
        return tokenlist
    
    def label_inc_once(self, token:Token, newval:Token, passid:int):
        #token should be guaranteed to be an assignable label. If not, don't.
        #symtable is temp. labeltable is persist.
        #anonset is temp. anonlabels is persist.
        tokenv, tokent = (token.v, token.type)
        address = int(newval.v)
        if tokent != "IDENT":
            return
        if tokenv == self.anonchar:
            # Processing anonymous label
            if address in self.anonset:
                prevtok = [d.token for d in self.anonlabels if d.token == self.origin]
                if len(prevtok) != 1:
                    err(token, f"SERIOUS ERROR: Quick check anonymous label claims a duplicate label where there is either none or too many found. Instances found: {len(prevtok)}")
                err(prevtok, "<-- Previous definition found here.")
            else:
                self.anonset.add(address)
                self.anonindex += 1
                if passid == 1:
                    self.anonlabels.append(AnonDef(token, address))
                else:
                    if address != self.anonlabels[self.anonindex].addr:
                        err(token, "Label shifting has been detected. 3rd pass unavailable to correct this condition.")
                        self.labelshift = True
        else:
            # Processing standard label
            if tokenv in self.symtable:
                print(errmsg(token, f"[{token.v}] already defined. Initial definition follows."))
                try:
                    err(self.labels[token.v].token, f"<-- Initial definition for [{token.v}] here")
                except:
                    raise ValueError(redmsg(f"[{token.v}] already defined. Initial definition not locatable. Check macros."))
            else:
                self.sym_assign(tokenv, newval)
                if passid == 1:
                    self.labels[tokenv] = LabelDef(token, newval)
                else:
                    oldval = self.labels[tokenv].value
                    if int(oldval.v) != address:
                        err(token, "Label shifting has been detected. 3rd pass unavailable to correct this condition.")
                        self.labelshift = True

    def write_data(self, extendable:bytes|bytearray):
        #NOTE: self.origin will always point to the byte after the data would
        #       be written. Do not waste your time modifying any other source
        #       to put it before. We will simply calculate it here.
        #print("Writing data...")
        calcorigin = self.origin-len(extendable)
        if not self.curseg:
            if "__DEFAULT" in self.segments:
                self.curseg = self.segments["__DEFAULT"]
            else:
                self.curseg = SegmentDef("__DEFAULT", calcorigin, bytearray())
                self.segments["__DEFAULT"] = self.curseg
        ''' 
        # Don't bother with continuity checks for now. Origin's only role here
        # is to inform start of segment. Origin's primary role is for address
        # calculations, none of which is being done here.
        if self.origin-(len(self.curseg.data)+len(extendable)) != self.curseg.baseaddr:
            print(len(self.curseg.data))
            print(len(extendable))
            print(self.origin)
            print(self.curseg.baseaddr)            
            raise ValueError(redmsg(f"Noncontiguous segment \"{self.curseg.name}\" detected."))
        '''
        self.curseg.data.extend(extendable)


    def parse_bytestream(self, tokenline:list[Token], bytewidth:Optional[int], passid=int) -> Optional[bytes]:
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
                if isinstance(stringified, str):
                    if bytewidth:
                        # Is string and is outputting data
                        for c in stringified:
                            charval = ord(c).to_bytes(16,"little", signed=False)[:bytewidth]
                            self.origin += bytewidth
                            result.extend(charval)
                    else:
                        # Is string and is not outputtting data (.echo)
                        encoded = stringified.encode("ASCII")
                        result.extend(encoded)
                    continue
            # If no string condition caught, process a number instead
            exprval = int(self.eval_expr(param, passid).v)
            if bytewidth is None:
                # Is a number but is not outputting data (.echo)
                result.extend(str(exprval).encode("ASCII"))
            else:
                int_min = -(1 << (8*bytewidth-1))
                int_max = (1 << (8*bytewidth))-1
                #print(f"Boundaries: {int_min}, {int_max}")
                if ((exprval < int_min) or (exprval > int_max)) and passid > 1 and bytewidth:
                    print(warnmsg(param[0],f"Expression evalulates outside bounds. Value {exprval} is being truncated."))
                # Is a number and it is being output.
                # Default action is to chop bits that won't fit.
                # Value clamping logic is here in case of future feature.
                #exprval = max(exprval, int_min)
                #exprval = min(exprval, int_max)
                v = exprval.to_bytes(16,"little",signed= True if exprval<0 else False)[:bytewidth]
                result.extend(v)
                #print(v.hex())
                self.origin += bytewidth
        return result

    def sym_assign(self, key, value):
        # While this could've been a simple dict assignment, doing it this way
        # puts it all in a single spot so I can easily debug it. Because boy
        # howdy did I need to find out what got assigned where and when.
        #print(yellowmsg(f"Assigning [{key}] the value: {value}"))
        #if key == "VAR_UNIONSTART":
        #    print(yellowmsg(f"Assigning [{key}] the value: {value}"))
        self.symtable[key] = value

    def get_params_from_iter(self, iterable:Iterator[Token], omit_close_paren:bool = False) -> list[list[Token]]:
        parenlevel = 0
        paramlist = []
        tokens = []
        item = None
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
            paramlist.append(tokens)
            tokens = []
            if not omit_close_paren:
                err(item, "Macro invocation has no matching closing parenthesis.")
        return paramlist
    
    def eval_to_string(self, token:Token) -> Optional[str]:
        if token.type == "STRING":
            return token.v
        if token.type == "IDENT":
            if token.v in self.symtable:
                symentry = self.symtable[token.v]
                if isinstance(symentry, str):
                    return symentry
        return None
    
    def eval_expr(self, tokenline:list[Token], passnum:int=1, depth=0) -> Token:
        ''' Parses an expression in `tokenline` unconditionally from left-to-right,
            excepting parentheses. Supports all major operators. Label resolution
            based on `passnum`. `$` oper resolves to `self.origin`
        '''
        cls = self.__class__
        if depth > cls.MAX_RECURSION_DEPTH:
            raise ValueError(redmsg(f"Too many parentheses were used. Maximum depth is {cls.MAX_RECURSION_DEPTH}"))
        tokens = iter(tokenline)
        curval:int = None
        oper:Token = None
        unarystack:list[str] = list()
        #print(f"Evaluating: {tokline2mini(tokenline)}")
        for token in tokens:
            # Special case for expressions leading with unary operators.
            # Special case ends on a value or value-yielding paren group.
            #print(token)
            if token.v in cls.UNARY_OPS and curval is None:
                unarystack.append(token.v)
                continue
            elif token.v in cls.MATH_OPS and curval is None:
                err(token, f"Illegal operator {token.v} used in leading unary stack.")
            # Handle binary operators, and other unary operators after it.
            if token.v in cls.MATH_OPS and isinstance(curval, int):
                if not oper:
                    oper = token
                else:
                    if token.v in cls.UNARY_OPS:
                        unarystack.append(token.v)
                    else:
                        err(token, f"Illegal use of operator {token.v}")
                continue
            # Handle parethesis group, consuming all tokens inside the group.
            # Parenthesis group must evalulate to a number token so it can
            # be taken forward for further parsing. The overwrite is intended.
            # Empty parentheses evaluate to Token NUM 0.
            if token.v =='(':
                paramlist = self.get_params_from_iter(tokens)
                if paramlist == [] or paramlist == [[]]:
                    token = from_token(token, "NUM", "0")
                else:
                    if len(paramlist) != 1:
                        err(token, "Illegal comma use in expression.")
                    token = self.eval_expr(paramlist[0], passnum, depth+1)
                    '''
                    try:
                        print(f"Paren resolve: 0x{int(token.v).to_bytes(2, 'big').hex()}")
                    except:
                        print(f"Paren resolve print failure. Raw value: {token}")
                    '''
            # This one is also a passthrough to evalulate labels. Unary stack
            # will be consumed to produce a Token NUM.
            if token.type == "IDENT" or (token.type == "OPER" and token.v == "$"):
                val = self.eval_val(token, passnum, unarystack)
                unarystack = list()
                token = from_token(token, "NUM", str(val))
            # If we got to this point, we have a number to parse, either to set
            # curval with or for binary expression evaluation. All valid paths
            # will be met with a 'continue'. A passthrough here means we got
            # something unparseable.
            if "NUM" in token.type:
                if curval is None:
                    curval = self.eval_val(token, passnum, unarystack)
                    unarystack = list()
                    continue
                if not oper:
                    err(token, "Missing operator at around this location.")
                else:
                    newval = self.eval_val(token, passnum, unarystack)
                    unarystack = list()
                    curval = self.eval_pair(curval, oper, newval)
                    oper = None
                    continue
            err(token, f"Unexpected token {token.v} at this location.")
        # An empty expression yields 0. The legality of
        # that operation is the caller's responsibility.
        if curval is None:
            curval = 0
        if not len(tokenline):
            returnval = Token("NUM", str(curval), 0, 0, '')
        else:
            returnval = from_token(tokenline[0], "NUM", str(curval))
        return returnval
    
    
    def eval_pair(self, base:int, oper:Token, nval:int) -> int:
        #print(f"NUMERIC EVAL {base} {oper.v} {nval}")
        oldbase = base
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
        #print(f"Pair eval {oldbase}{oper.v}{nval} = {base}")
        return base
    

    def eval_val(self, token:Token, passnum=1, unarystack=None) -> int:
        if unarystack is None:
            unarystack = []
        tokenv = token.v
        tokent = token.type
        if "NUM" in tokent:
            # Number string conditioning
            if isinstance(tokenv, int):
                tokenv = str(tokenv)
            tokenv = str(tokenv).upper()
            if tokenv.startswith(('$','%')):
                tokenv = tokenv[1:]
            else:
                if tokenv.endswith(('H','D','B')):
                    tokenv = tokenv[:-1]
            # Number base conversion to type int
            if "HEX" in tokent:
                value = int(tokenv, 16)
            elif "BIN" in tokent:
                value = int(tokenv, 2)
            elif "OCT" in tokent:
                # Not supported, but it's here just in case we need it later.
                value = int(tokenv, 8)
            else:
                value = int(tokenv, 10)
            # Begin processing unary stack from end to start
            for unary in reversed(unarystack):
                if unary == '+':
                    pass
                if unary == '~':
                    value = ~value
                if unary == '-':
                    value = -value
        elif tokent == "OPER" and tokenv == "$":
            # Doing it this way lets us use numeric unary operator processing.
            origintoken = from_token(token, "NUM", str(self.origin))
            #print(f"ORIGIN TOKEN: {self.origin} {origintoken}")
            value = self.eval_val(origintoken, passnum, unarystack)
        elif tokent == "IDENT" and tokenv == self.anonchar:
            # Anonymous label processing. Unary operators here have a very
            # different meaning. Still, we'll want to filter out any '~' and
            # carry that over to recursive numeric processing if any are found.
            nextcount = unarystack.count("+")
            prevcount = unarystack.count("-")
            unarystack = "~" * unarystack.count("~")    #Consolidate remaining
            # A single + is the same as nothing. So...
            if not nextcount:
                nextcount = 1
            # Therefore a single - maths out to offset 0, which was the
            # most current already-defined label.
            offset = nextcount-prevcount
            try:
                anondef = self.anonlabels[self.anonindex+offset]
                anontoken = from_token(token, "NUM", str(anondef.addr))
            except:
                if passnum == 1:
                    anontoken = from_token(token, "NUM", "0")
                else:
                    err(token, f"Anonymous label not found. Index attempted: [{self.anonindex + offset}]")
            value = self.eval_val(anontoken, passnum, unarystack)
        elif tokent == "IDENT":
            # Normal label processing. Doing a recursive call on a token to
            # take advantage of numeric unary operator processing.
            symval:Token = None
            if tokenv not in self.symtable:
                if passnum == 1:
                    symval = from_token(token, "NUM", "0")
                elif passnum == -1:
                    raise Exception("This error exists only to simulate a bug that allowed macrodef to work")
                else:
                    if tokenv not in self.labels:
                        err(token, f"Symbol [{tokenv}] not found.")
                    else:
                        symval = self.labels[tokenv].value
            else:
                symval = self.symtable[tokenv]
            value = self.eval_val(symval, passnum, unarystack)
        else:
            err(token, f"Illegal token type {tokent} of value {tokenv}")
        return value


class TokenStream(object):
    def __init__(self, tokenlist: "list[Token]|Tokenizer"):
        if not isinstance(tokenlist,(list,Tokenizer)):
            raise ValueError(f"Incorrect input type. Expected list or Tokenizer, got {type(tokenlist)} from {tokenlist}")
        if isinstance(tokenlist,Tokenizer):
            self.tokens = tokenlist.tokens
            self.embeds = tokenlist.tokenfiles
        else:
            self.tokens = tokenlist
            self.embeds = dict()
        for i in self.tokens:
            if not isinstance(i, Token):
                raise ValueError(f"Incorrect type in tokenlist. Got {type(i)}")
        self.reset()

    def __iter__(self):
        return self.getline()

    @classmethod
    def from_filename(cls, filename):
        return cls(Tokenizer.from_filename(filename))
    
    @classmethod
    def from_str(cls, textdata):
        return cls(Tokenizer.from_str(textdata))

    def reset(self):
        self.resub_tokens = deque()
        self.tokidx = 0

    def resubmit_tokens(self, tokenlist:list[Token]):
        # Tokens maybe re-inserted at the start of the stream for reprocessing.
        if not isinstance(tokenlist, list):
            raise ValueError(f"Object being resubmitted is not a list. {type(tokenlist)} found.")
        for token in tokenlist:
            if not isinstance(token, Token):
                raise ValueError(f"Object in resubmit is not a token. {token}, type: {type(token)}")
        self.resub_tokens.extendleft(list(reversed(tokenlist)))

    def gettok(self) -> list[Token]:
        return self.tokens

    def getline(self) -> Iterator[list[Token]]:
        self.reset()
        def isnewline(token:Token, defining:bool=False) -> bool:
            if not isinstance(token, Token):
                err(NEWLINE_TOKEN, f"Illegal object in tokenstream: {token}")
            return True if token.type == "NEWLINE" or (not defining and token.type == "OPER" and token.v == '\\') else False
        def getline_core():
            tokens = []
            inside_define = False
            while self.tokidx < len(self.tokens) or self.resub_tokens:
                if self.resub_tokens:
                    token = self.resub_tokens.popleft()
                else:
                    token = self.tokens[self.tokidx]
                    self.tokidx += 1
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
        # Ensure all tokens are consumed regardless of submission time.
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
    UNESCAPE_LISTING = {"N":lambda:'\n',"R":lambda:'\r',"T":lambda:'\t',"0":lambda:'\0',"\\":lambda:'\\',"\'":lambda:'\'',"\"":lambda:'\"','#':lambda:chr(random.randint(0,255)) }

    def __init__(self, tokendata:list[Token] = None, tokenfiles:dict[str,list[Token]] = None):
        self.tokens:list[Token] = tokendata
        if tokenfiles is None:
            tokenfiles = dict()
        self.tokenfiles:dict[str,list[Token]] = tokenfiles

    @classmethod
    def from_filename(cls, filename):
        return cls(*cls.makestream(filename))
    
    @classmethod
    def from_str(cls, textdata:str):
        return cls(*cls.makestream("NUL", depth=1, textdata=textdata))

    @classmethod
    def makestream(cls, filename, depth=1, textdata:str=None, incsfound:dict[str,list[Token]]=None) -> Tuple[list[Token], dict[str,list[Token]]]:
        tokendict:dict[str,list[Token]] = dict()
        if incsfound is not None:
            tokendict = incsfound
        if depth > cls.MAX_INCLUDE_DEPTH:
            raise ValueError(redmsg("Include depth exceeded max depth."))
        tokenstream = []
        if depth==1 and textdata is not None and filename=="NUL":
            filedata = textdata.split('\n')
        else:
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
                if not includefile in tokendict:
                    tokendict[includefile] = None
                    try:
                        includestream , _ = cls.makestream(includefile, depth=depth+1, incsfound=tokendict)
                    except Exception as e:
                        printerr(token0, f"Depth {depth}: Exception stack unwinding.")
                        raise e
                    tokendict[includefile] = includestream
                
            tokenline.append(NEWLINE_TOKEN)
            tokenstream.extend(tokenline)
        return (tokenstream, tokendict)
    
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
                    value = cls.unescape_string(value)
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
        with open(filename, "r") as f:
            return list(f.readlines())
        
    @classmethod
    def unescape_string(cls, raw: str) -> str:
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
                result.append(cls.UNESCAPE_LISTING.get(c.upper(),c)())
                p = ''
            else:
                result.append(c)
        else:
            if p == '\\':
                result.append(p)
        return ''.join(c for c in result)



if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python macroparse.py <filename>")
        sys.exit(1)
    ts = Parser.from_filename(sys.argv[1])
    print(ts.read_data())
    pass
    filename = sys.argv[1]

#NOTE: This is how I invoke on the command line to output all that debug text
#   to a separate output file. Keep because we may lose it.
#
# python tools/parser4.py tools/macrotest.z80 > parser_output.txt
#
