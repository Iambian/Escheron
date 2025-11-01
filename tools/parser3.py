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
    SHOW_RESUBMISSIONS = False
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

    def parse(self, tokens:"TokenStream", passid=1, depth=1, trace=False, expandbuiltins=True) -> list[Token]:
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
            if cls.DEBUGMODE:
                print(yellowmsg(f"DEBUG: Line before macro expansion: {tokline2str(line)}"))
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
                        # First, expand any nested macros within the body
                        expanded_macrobody = self.parse(TokenStream(macrobody), passid, depth+1, trace, True)
                        
                        # If the expanded body is a single IDENT token (likely from concat), store its value as a string.
                        if len(expanded_macrobody) == 1 and expanded_macrobody[0].type == "IDENT":
                            macrodef = expanded_macrobody[0].v # Store the string value directly
                        elif self._is_evaluatable_expression(expanded_macrobody):
                            # If it's an evaluatable expression, evaluate it.
                            try:
                                evaluated_value = self.eval_expr(expanded_macrobody, passid)
                                macrodef = evaluated_value
                            except Exception as e:
                                # Should not happen if _is_evaluatable_expression is accurate,
                                # but as a safeguard, store as TokenStream if evaluation fails.
                                traceback.print_exc()
                                macrodef = MacroDef(token1, paramlist, TokenStream(macrobody))
                        else:
                            # If not evaluatable, store the original macrobody as a TokenStream
                            macrodef = MacroDef(token1, paramlist, TokenStream(macrobody))
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
                if cls.DEBUGMODE:
                    print(yellowmsg(f"DEBUG: Processing token: {token.v} ({token.type})"))
                if tokent in ("MACRO","IDENT","DIR_CALL","DIRECTIVE") and tokenv in self.symtable:
                    symentry = self.symtable[tokenv]
                    if isinstance(symentry, MacroDef):
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
                            macrobody = self.parse(TokenStream(macrobody), passid, depth+1, trace, False)
                        except:
                            pass
                        if cls.DEBUGMODE:
                            print(yellowmsg(f"DEBUG: Expanding macro/symbol '{tokenv}'. Body: {tokline2str(macrobody)}"))
                        resubmit.extend(macrobody)
                    else:
                        # Any other token must be passed through as-is.
                        resubmit.append(token)
                elif tokent == "MACRO" and tokenv.lower() in ("eval(", "concat("):
                    if not expandbuiltins:
                        # Consume, reconstruct, then emit tokens to prevent
                        # any further expansions within.
                        t = self.joinparams(self.get_params_from_iter(lineiter))
                        resubmit.extend([token]+t)
                        continue
                    # Execute built-in macros.
                    # Add debug print before calling get_params_from_iter
                    if cls.DEBUGMODE:
                        print(yellowmsg(f"DEBUG: Built-in macro '{tokenv}' encountered. Full line: {tokline2str(line)}. Current token: {token.v}"))
                    
                    invokelist = self.get_params_from_iter(lineiter)
                    
                    if any([param == [] for param in invokelist]):
                        err(token, f"Built-in macro [{tokenv}] may not contain empty parameters.")
                    
                    if tokenv.lower() == "concat(":
                        concatenated_string_parts = []
                        for param_tokens in invokelist:
                            if len(param_tokens) == 1:
                                p_token = param_tokens[0]
                                if p_token.type == "STRING":
                                    concatenated_string_parts.append(p_token.v)
                                elif p_token.type == "IDENT":
                                    # IDENT tokens are treated as their literal string value for concat
                                    concatenated_string_parts.append(p_token.v)
                                elif "NUM" in p_token.type:
                                    err(p_token, f"Built-in macro [{tokenv}] does not accept numeric parameters. Found: {p_token.v}")
                                else:
                                    err(p_token, f"Unsupported token type [{p_token.type}] for concat. Expected STRING or IDENT.")
                            else:
                                err(param_tokens[0], f"Built-in macro [{tokenv}] expects single-token string or identifier parameters. Found: {tokline2str(param_tokens)}")
                        s = ''.join(concatenated_string_parts)
                        newtoken = from_token(token, "IDENT", s) # concat always results in an IDENT
                    elif tokenv.lower() == "eval(":
                        if len(invokelist) == 1:
                            # eval expects a single expression, which must be evaluatable
                            evaluated_value = self.eval_expr(invokelist[0], passid)
                            newtoken = from_token(token, "NUM", str(evaluated_value)) # eval always results in a NUM
                        else:
                            err(token, f"Built-in macro [{tokenv}] expects exactly one parameter.")
                    else:
                        if cls.DEBUGMODE:
                            print(yellowmsg(tokline2minitok(line)))
                        err(token, f"Illegal use of [{token.v}].")
                    if cls.DEBUGMODE:
                        print(yellowmsg(f"DEBUG: Built-in macro '{tokenv}' invoked. Result: {newtoken.v} ({newtoken.type})"))
                    resubmit.append(newtoken)
                else:
                    # If it wasn't a macro, or if it was a built-in macro but expandbuiltins is False,
                    # pass the token through and let the evalulators deal with it.
                    resubmit.append(token)
                pass
            # If nothing expanded, line should be the same as resubmit.
            # If it did expand, append a newline as that would have been
            # stripped by the TokenStream() iter function.

            if self.strip_token_meta(line) != self.strip_token_meta(resubmit):
                if cls.DEBUGMODE and cls.SHOW_RESUBMISSIONS:
                    try:
                        print(errmsg(line[0],f"RESUB: [{tokline2str(line)}] to [{tokline2str(resubmit)}]"))
                    except:
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
                if values[diridx].upper() == ".EQU":
                    if diridx != 1:
                        err(line[0], "Directive .EQU must have only one token prior.")
                    if line[0].type != "IDENT":
                        err(line[0], "Token prior to .EQU must be an identifier.")
                    exprval = self.eval_expr(line[2:], passid)
                    # Add in check to prevent redefinition but only on same-pass
                    self.symtable[line[0].v] = exprval
                if values[diridx].upper() == ".ERROR":
                    print(yellowmsg("Warn: .error directive implementation incomplete."))
                    print(redmsg(".error directive encountered. Raising error using text available."))
                    err(token, f"{tokline2str(line)}")


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
            if results:
                print(errmsg(results[0], f"RERET: {'·'*(depth-1)} {tokline2str(results)}"))
            else:
                print(yellowmsg(f"DEBUG: RERET: {'·'*(depth-1)} (empty results)"))

        return results
    
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

    def get_params_from_iter(self, iterable:Iterator[Token]) -> list[list[Token]]:
        parenlevel = 1 # Start with 1 because the opening parenthesis is part of the MACRO token
        paramlist = []
        tokens = []
        item = None # Initialize item to None
        for item in iterable:
            if self.__class__.DEBUGMODE:
                print(yellowmsg(f"DEBUG: get_params_from_iter - Processing token: {item.v} ({item.type}), parenlevel: {parenlevel}"))
            if item.type == "OPER":
                if item.v == "(":
                    parenlevel += 1
                elif item.v == ")":
                    parenlevel -= 1
                    if parenlevel == 0: # Found the matching closing parenthesis for the initial MACRO token
                        paramlist.append(tokens)
                        break
            if item.type == "COMMA" and parenlevel == 1: # Only split parameters at the top level of the macro's arguments
                paramlist.append(tokens)
                tokens = []
                continue
            tokens.append(item)
        else:
            # The loop completed without a 'break'.
            # If 'item' is still None, it means the iterable was empty.
            if item is None:
                raise ValueError(redmsg("No tokens after expansion invocation."))
            # If 'item' is not None, it means the loop ran but didn't find a matching closing parenthesis.
            err(item, "Macro invocation has a no matching closing parenthesis.")
        if self.__class__.DEBUGMODE:
            print(yellowmsg(f"DEBUG: get_params_from_iter - Generated paramlist: {[[t.v for t in p] for p in paramlist]}"))
        return paramlist

    def is_macro_defined(self, name_without_open_paren) -> bool:
        if name_without_open_paren in self.symtable:
            return True
        if name_without_open_paren + '(' in self.symtable:
            return True
        return False

    def _is_evaluatable_expression(self, tokenlist:list[Token]) -> bool:
        ''' Checks if a list of tokens represents a simple mathematical expression. '''
        if not tokenlist:
            return False
        
        # A simple heuristic: if it contains any directives or non-math operators
        # that are not parentheses, it's likely not a simple evaluatable expression.
        # This is a simplification and might need refinement for more complex cases.
        if len(tokenlist) == 1 and tokenlist[0].type == "STRING":
            return False # A single string token is not evaluatable as an integer expression
            
        for token in tokenlist:
            if token.type == "NEWLINE": # Explicitly exclude newlines from evaluatable expressions
                return False
            if token.type == "DIRECTIVE" or token.type == "DIR_CALL":
                return False
            if token.type == "PREOP":
                return False
            # Exclude MACRO tokens from being considered evaluatable by eval_expr
            if token.type == "MACRO":
                return False
            if token.type == "IDENT":
                if token.v in self.symtable:
                    symval = self.symtable[token.v]
                    if isinstance(symval, MacroDef): # If it's a MacroDef, it's not a simple evaluatable expression
                        return False
                    if isinstance(symval, str): # If it's a string, it's not evaluatable as an integer expression
                        return False
                else:
                    # If an IDENT token is not in the symbol table, it's likely an instruction or label,
                    # and thus not part of a simple evaluatable expression.
                    return False
            if token.type == "OPER" and token.v not in self.MATH_OPS and token.v not in ('(', ')', '$'):
                return False
        return True
    
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
        
        if self.__class__.DEBUGMODE:
            print(yellowmsg(f"DEBUG: eval_expr called with: {tokline2str(tokenline)}"))

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
                    if self.__class__.DEBUGMODE:
                        print(yellowmsg(f"DEBUG: eval_expr - Sub-expression result: {current_value}"))
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
            
            if self.__class__.DEBUGMODE:
                print(yellowmsg(f"DEBUG: eval_expr - current_value: {current_value}, baseval: {baseval}, operator: {operator.v if operator else 'None'}"))

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
        cls = self.__class__
        if self.__class__.DEBUGMODE:
            print(yellowmsg(f"DEBUG: eval_val called with token: {token.v} ({token.type})"))
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
                if self.__class__.DEBUGMODE:
                    print(yellowmsg(f"DEBUG: eval_val - Resolving symbol '{tokval}': {symval}"))
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
        print(yellowmsg(f"DEBUG: Resubmitting tokens: {tokline2str(tokenlist)}"))
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
                print(yellowmsg(f"DEBUG: TokenStream yielding line: {tokline2str(line)}"))
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
                includefile = unescape_string(token1.v)
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
                yield SubToken(kind, value, pos)
            pos = mo.end()
            mo = cls.get_token(code, pos)
        if pos != len(code):
            raise SyntaxError(redmsg(f"Unexpected character {code[pos]!r} at position {pos}"))

    @staticmethod
    def strip_comment(line: str) -> str:
        line = line.strip()
        in_string = False
        escape_next = False # Flag to indicate if the *next* character is escaped
        quote_char = ''
        for i, c in enumerate(line):
            if escape_next:
                escape_next = False # Consume the escape
                continue # Treat it as part of the string/literal
            
            if c == '\\':
                escape_next = True
                continue
            
            if in_string:
                if c == quote_char:
                    in_string = False # End of string
                continue
            
            # If not in string
            if c in ('"', "'"):
                in_string = True
                quote_char = c
                continue
            
            if c == ';':
                return line[:i] # Comment found
        
        return line # No comment found

    @staticmethod
    def readfile(filename) -> Optional[list[str]]:
        try:
            with open(filename, "r") as f:
                return list(f.readlines())
        except FileNotFoundError:
            print(f"Failed to open {filename}")
            return None
        except Exception as e:
            print(f"An unexpected error occurred while reading {filename}: {e}")
            raise e

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
            elif tc == "#":     #random data byte, as per SPASM-ng spec.
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
