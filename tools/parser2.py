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
    value: str|int
    position: int

class Token(NamedTuple):
    type: str
    v: str|int
    col: int
    row: int
    file: str

class IfStackEntry(NamedTuple):
    ''' `cond` has the following states and meanings:

    `True` = The current branch is being taken.
    `False` = The current is not being taken but is still available to take.
    `None` = A branch has already been taken and no more at this level will take.

    `token` exists for traceback purposes.
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
    SHOW_SYMTABLE = False
    SHOW_SYMTABLE_MODE = None    #None|"SYM"|"MAC"
    SHOW_PARSE_LINESTART = False
    SHOW_PARSE_AFTER_PREOP = True
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
        except Exception as e:
            print(Fore.RED)
            traceback.print_exc()
            print(Style.RESET_ALL)

        #self.parse(self.tokens, 2)
        if len(self.if_stack):
            baseentry = self.if_stack[0]
            err(baseentry.token, "Unbalanced #IF/#ENDIF statements starting here")
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

    def parse(self, tokens:"TokenStream", passid=1, depth=1, trace=False) -> list[Token]:
        cls = self.__class__
        if depth > cls.MAX_RECURSION_DEPTH:
            raise ValueError(redmsg("Maximum recursion depth reached."))
        iterator = tokens.getline()
        results:list[Token] = []
        for lidx,line in enumerate(iterator):
            if len(line) < 1:
                continue
            if cls.DEBUGMODE and cls.SHOW_PARSE_LINESTART:
                print(errmsg(line[0], f"LS: {'·'*(depth-1)} {tokline2str(line)}"))
            if len(tokline2str(line)) > 256:
                err(line[0],"Line buffer exceeded reasonable length.")
            token0 = line[0]
            token0v = str(token0.v).upper()
            # Must handle condition-changing preops first.
            if token0.type == "PREOP" and token0v == "#ENDIF":
                if len(self.if_stack) < 1:
                    err(token0, "#ENDIF used without corresponding #IF/#IFDEF/#IFNDEF.")
                self.if_stack.pop()
                continue
            if token0.type == "PREOP" and token0v == "#ELSE":
                if len(self.if_stack) < 1:
                    err(token0, "#ELSE used without corresponding #IF/#IFDEF/#IFNDEF.")
                current_entry = self.if_stack[-1]
                if current_entry.cond is False:
                    # If false (skipped IF/ELIF with no change), unconditionally toggle to true
                    self.if_stack[-1] = IfStackEntry(token0, True)
                else:
                    # Otherwise (True/None) becomes stuck to None.
                    self.if_stack[-1] = IfStackEntry(token0, None)
                continue
            if token0.type == "PREOP" and token0v == "#ELIF":
                if len(self.if_stack) < 1:
                    err(token0, "#ELIF used without corresponding #IF/#IFDEF/#IFNDEF.")
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
            # Now that we did stuff that could change if_stack level or state...
            if len(self.if_stack) > 0 and (self.if_stack[-1].cond is False or self.if_stack[-1].cond is None):
                # Consume lines if top of stack is False or None
                continue
            # This token line is reachable. Continue processing.
            if token0.type == "PREOP":
                if token0v in ("#IF", "#IFDEF", "#IFNDEF", "#DEFINE", "#MACRO", "#UNDEF"):
                    if len(line) < 2:
                        err(token0, f"Missing parameters for preop {token0.v}")
                    token1 = line[1]
                    token1v = token1.v
                    if token0v in ("#IFDEF", "#IFNDEF"):
                        ifresult = False
                        if token1v in self.symtable:
                            ifresult = True
                        elif token1v+'(' in self.symtable:  # Function macrodef
                            ifresult = True
                        if token0v == "#IFNDEF":
                            ifresult = not ifresult
                        self.if_stack.append(IfStackEntry(token0, ifresult))
                        continue
                    if token0v == "#IF":
                        ifexpr = self.parse(TokenStream(line[1:]), 2, depth+1, trace)
                        ifresult = True if self.eval_expr(ifexpr, 2) != 0 else False
                        self.if_stack.append(IfStackEntry(token0, ifresult))
                        continue
                    if token0v in ("#MACRO", "#DEFINE"):
                        if line[1].type not in ("IDENT", "MACRO", "DIRECTIVE", "DIR_CALL"):
                            err(token1, f"Invalid identifier assigned to {token0v}")
                    if token0v in ("#UNDEF", "#UNDEFINE"):
                        if token1v in self.symtable:
                            del self.symtable[token1v]
                            continue
                        withparen = token1v + '('
                        if withparen in self.symtable:
                            del self.symtable[withparen]
                            continue
                    if token0v == "#DEFINE":
                        if token1.type in ("IDENT", "DIRECTIVE"):
                            # No params. Simple macro. Could be a symtable entry
                            macname = token1.v
                            macbody = line[2:]
                            try:
                                macbody = self.eval_expr(macbody, passid)
                            except Exception as e:
                                # If err, macbody still contains original
                                # contents. That's fine. It's just a text repl.
                                pass
                            if isinstance(macbody, int):
                                macdef = macbody
                            elif isinstance(macbody,Token):
                                err(token,f"macbody must not be a lone token. Found: {macbody} ")
                            else:
                                macdef = MacroDef(token1, list(), TokenStream(macbody))
                            self.symtable[macname] = macdef
                        elif token1.type in ("MACRO", "DIR_CALL"):
                            if token1.v in ("eval(","concat(","eval","concat"):
                                err(token1, f"Illegal redefinition of reserved macro name {token1.v}")
                            paramlist = self.get_paramlist(line, 2)
                            if paramlist:
                                if any([(len(param) > 1 or param[0].type != "IDENT") for param in paramlist]):
                                    err(token1, "Illegal identifier(s) found in macro signature.")
                                macrolen = sum([len(param) for param in paramlist])
                                macrolen += len(paramlist) #n-1 commas plus end paren = n
                                paramlist = [param[0] for param in paramlist] # Flatten list with 1st token ea.
                            else:
                                macrolen = 1    #empty params. No args.
                            macrobody = line[2+macrolen:]
                            macrodef = MacroDef(token1, paramlist, TokenStream(macrobody))
                            self.symtable[token1.v] = macrodef
                        else:
                            err(token1, "Illegal macro identifier")
                        continue # Added to skip macro expansion for #DEFINE line
                    if token0v == "#ENDMACRO":
                        err(token0, "#ENDMACRO used without corresponding #MACRO.")
                    if token0v == "#MACRO":
                        if depth > 1:
                            err(token0, "Illegal nesting of #MACRO during expansion.")
                        if token1.v in ("eval(","concat(","eval","concat"):
                            err(token1, f"Illegal redefinition of reserved macro name {token1.v}")
                        paramlist = self.get_paramlist(line, 2)
                        if paramlist:
                            if any([(len(param) > 1 or param[0].type != "IDENT") for param in paramlist]):
                                err(token1, "Illegal identifier(s) found in macro signature.")
                            paramlist = [param[0] for param in paramlist] # Flatten list with 1st token ea.
                            #Note: Length not considered because anything after
                            #the macrodef on the same line is discarded.
                        macrobody = []
                        while True:
                            try:
                                macroline = next(iterator)
                            except StopIteration:
                                err(token0, "EOF encountered without #ENDMACRO.")
                            if len(macroline) < 1:
                                continue
                            mltok0 = macroline[0]
                            mltok0v = mltok0.v.upper()
                            if mltok0.type == "PREOP" and mltok0v == "#MACRO":
                                err(mltok0, "Illegal nesting of #MACRO statement.")
                            if mltok0.type == "PREOP" and mltok0v == "#ENDMACRO":
                                break
                            macroline.append(from_token(mltok0, 'NEWLINE', '\n'))
                            macrobody.extend(macroline)
                        #NOTE: macrobody now set. prior block shou
                        macrodef = MacroDef(token1, paramlist, TokenStream(macrobody))
                        self.symtable[token1.v] = macrodef
                        continue # Added to skip macro expansion for #MACRO line
                pass

            #'''
            # Preops processed. Begin macro expansion section.
            if cls.DEBUGMODE and cls.SHOW_PARSE_AFTER_PREOP:
                print(errmsg(line[0], f"{'·'*(depth-1)} {tokline2str(line)}"))
            resubmit:list[Token] = []
            lineiter = iter(line)
            for token in lineiter:
                tokenv,tokent = (token.v,token.type)
                if tokent in ("MACRO","IDENT","DIR_CALL") and tokenv in self.symtable:
                    symentry = self.symtable[tokenv]
                    if isinstance(symentry, MacroDef):
                        if tokenv.endswith('('):
                            invokelist = self.get_params_from_iter(lineiter)
                        else:
                            invokelist = []
                        paramlist = [i.v for i in symentry.params]
                        if len(paramlist) > len(invokelist):
                            err(token,"Insufficient parameters passed to macro invocation.")
                        premacrobody = symentry.body.tokens #copies \ and NL
                        macrobody = []
                        for newtoken in premacrobody:
                            try:
                                if newtoken.v in paramlist:
                                    macrobody.extend(invokelist[paramlist.index(newtoken.v)])
                                else:
                                    macrobody.append(newtoken)
                            except Exception as e:
                                raise e
                        try:
                            # Call macro_expand with passid
                            expanded_macrobody = self.macro_expand(macrobody, passid, builtinmacros=True, depth=depth+1)
                            resubmitval = self.parse(TokenStream(expanded_macrobody), passid, depth+1, trace)
                        except Exception as e:
                            print(errmsg(token, f"{' '*depth} Error in parse recursion. Unwinding from [{token}]"))
                            raise e
                        resubmit.extend(resubmitval)
                        if not isinstance(resubmitval, list):
                            raise ValueError("Resubmission value returned from parse is not a list.")
                    elif isinstance(symentry, int):
                        resubmit.append(from_token(token, "NUM", symentry))
                    else:
                        err(token, f"Unknown symentry return type value [{token.v}], type [{type(symentry)}]")
                elif tokent == "MACRO" and tokenv.lower() in ("eval(", "concat(") and expanded_builtin_macro:
                    # Built-in macros
                    invokelist = self.get_params_from_iter(lineiter)
                    # The parse method already handles built-in macros, so we can just call it
                    # with the current token and its parameters.
                    # This effectively re-routes the built-in macro handling from parse() to macro_expand()
                    # by calling macro_expand() from parse() and then parse() again for the result.
                    # This is a bit convoluted, but it maintains the existing structure.
                    # A cleaner approach might be to have parse() call macro_expand() for ALL macros,
                    # and macro_expand() handles both user-defined and built-in macros.
                    # For now, I'll just pass the original token and its parameters to macro_expand.
                    
                    # Create a temporary token list for the built-in macro invocation
                    temp_macro_invocation = [token] + self.joinparams(invokelist)
                    
                    try:
                        # Call macro_expand to process the built-in macro
                        expanded_builtin_macro = self.macro_expand(temp_macro_invocation, passid, builtinmacros=True, depth=depth+1)
                        resubmit.extend(expanded_builtin_macro)
                    except Exception as e:
                        print(errmsg(token, f"{' '*depth} Error in built-in macro expansion. Unwinding from [{token}]"))
                        raise e
                else:
                    resubmit.append(token)
            if line != resubmit:
                if resubmit and resubmit[-1].type != "NEWLINE":
                    resubmit.append(NEWLINE_TOKEN)
                tokens.resubmit_tokens(resubmit)
                continue

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

        return results

    def get_paramlist(self,tokenline:list[Token], start:int) -> list[list[Token]]:
        if len(tokenline) < 1:
            raise ValueError(redmsg("You may not pass an empty line into get_paramlist()"))
        if start >= len(tokenline):
            err(tokenline[-1], f"Start value {start} is out of bounds.")
        paramlist:list[list[Token]] = []
        paramtokens:list[Token] = []
        depth = 0
        for idx, token in enumerate(tokenline[start:], start):
            if token.type == "OPER" and token.v == "(":
                depth += 1
            if token.type == "OPER" and token.v == ")":
                depth -= 1
                if depth < 0:
                    break
            if token.type == "COMMA":
                paramlist.append(paramtokens)
                paramtokens = []
                continue
            paramtokens.append(token)
        else:
            err(tokenline[start], "Line has no matching close parenthesis.")
        
        # After the loop, if there are any collected tokens for the last parameter, add them.
        if paramtokens: # This condition ensures we don't add an empty list if there were no parameters
            paramlist.append(paramtokens)

        return paramlist
    
    def macro_expand(self, toklist:list[Token], passid:int, builtinmacros=False, depth=1) -> list[Token]:
        if depth > self.__class__.MAX_RECURSION_DEPTH:
            raise ValueError(f"Macro expansion depth limit exceeded.")
        newtokens = []
        tokenstream = iter(toklist)
        for token in tokenstream:
            tokenval, tokentype = (token.v, token.type)
            if tokentype in ("MACRO","IDENT","DIR_CALL"):
                # Collect parameters if there are any, regardless of whether or
                # not we can actually do anything with them.
                if tokenval.endswith('('):
                    invokeparams = self.get_params_from_iter(tokenstream)
                else:
                    invokeparams = []
                symentry = self.symtable.get(tokenval)
                if isinstance(symentry, MacroDef):
                    # If it exists and is a MacroDef, start replacing
                    # paramdefs with invocation parameters.
                    oldmacrobody = symentry.body.tokens
                    macroparams_v = [p.v for p in symentry.params] # Extract string values of parameters
                    if len(macroparams_v) > len(invokeparams):
                        err(token, f"Insufficient parameters passed to macro [{token}] invocation.")
                    newmacrobody = []
                    for macrotoken in oldmacrobody:
                        macrotokenval = macrotoken.v
                        if macrotokenval in macroparams_v:
                            # Substitute parameter with its invoked value
                            newmacrobody.extend(invokeparams[macroparams_v.index(macrotokenval)])
                        else:
                            newmacrobody.append(macrotoken)
                    # Expand any expandables inside new macrobody.
                    try:
                        newmacrobody = self.macro_expand(newmacrobody, passid, builtinmacros, depth+1)
                    except Exception as e:
                        print(errmsg(token, f"{' '*depth} Error in macro recursion. Unwinding from [{token}]"))
                        raise e
                elif tokenval in ("eval(","concat(") and builtinmacros:
                    # Processing built-in.
                    # Pre-expand parameters first
                    expanded_invokeparams = []
                    for param_tokens in invokeparams:
                        try:
                            expanded_param = self.macro_expand(param_tokens, passid, builtinmacros, depth + 1)
                            expanded_invokeparams.append(expanded_param)
                        except Exception as e:
                            print(errmsg(token, f"{' '*(depth+1)} Error in parameter macro expansion. Unwinding from [{token}]"))
                            raise e

                    is_eval = (tokenval.lower() == "eval(")
                    is_concat = (tokenval.lower() == "concat(")

                    # Check if all parameters are single tokens and resolve to strings
                    all_single_string_or_ident = True
                    resolved_strings = []
                    for param in expanded_invokeparams:
                        if len(param) == 1:
                            resolved_str = self._resolve_to_string(param[0], passid)
                            if resolved_str is not None:
                                resolved_strings.append(resolved_str)
                            else:
                                all_single_string_or_ident = False
                                break
                        else:
                            all_single_string_or_ident = False
                            break

                    if is_eval:
                        if all_single_string_or_ident:
                            # Eval with multiple string/ident params -> concatenate to STRING
                            concatenated_s = ''.join(resolved_strings)
                            newmacrobody = [from_token(token, "STRING", concatenated_s)]
                        elif len(expanded_invokeparams) == 1:
                            # Eval with single param, not string/ident -> assume expression
                            expr_tokens = expanded_invokeparams[0]
                            try:
                                # Need to parse the expression tokens before evaluating
                                # The parse method expects a TokenStream, so wrap it
                                parsed_expr = self.parse(TokenStream(expr_tokens), passid, depth + 1)
                                eval_result = self.eval_expr(parsed_expr, passid)
                                newmacrobody = [from_token(token, "NUM", eval_result)]
                            except Exception as e:
                                err(token, f"Error evaluating expression in eval() macro: {e}")
                        else:
                            err(token, "Unknown or unsupported eval() macro usage: Multiple parameters not all resolving to single strings/idents, or single parameter not an expression.")
                    elif is_concat:
                        if all_single_string_or_ident:
                            # Concat with multiple string/ident params -> concatenate to IDENT
                            concatenated_s = ''.join(resolved_strings)
                            newmacrobody = [from_token(token, "IDENT", concatenated_s)]
                        else:
                            err(token, "Unknown or unsupported concat() macro usage: Parameters must all be single tokens resolving to strings or idents.")
                    else:
                        err(token, "Unrecognized built-in macro. This error should have been unreachable.")
                else:
                    # We found something that wasn't recognized. Pass through
                    # for now and let the caller raise any errors this may cause
                    newmacrobody = [token] + self.joinparams(invokeparams)
            else:
                newmacrobody = [token]
            newtokens.extend(newmacrobody)
        return newtokens

    def joinparams(self, paramlist:list[list[Token]]):
        tokenlist = []
        for param in paramlist:
            tokenlist.extend(param)
            tokenlist.append(Token("COMMA",",",0,0,''))
        if not tokenlist:
            tokenlist = [None]  #Placeholder value for overwriting
        tokenlist[-1] = Token("OPER",")",0,0,'')
        return tokenlist

    def get_params_from_iter(self, iterable:Iterator[Token]) -> list[list[Token]]:
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
                        paramlist.append(tokens)
                        break
            if item.type == "COMMA" and parenlevel == 0:
                paramlist.append(tokens)
                tokens = []
                continue
            tokens.append(item)
        else:
            if not item:
                raise ValueError(redmsg("No tokens after expansion invocation."))
            err(item, "Macro invocation has a no matching closing parenthesis.")
        return paramlist
        
    def _parse_macro_invocation_params(self, tokenline:list[Token], start_idx:int) -> Tuple[list[list[Token]], int]:
        """
        Parses parameters from a macro invocation within a line.
        Assumes tokenline[start_idx] is the token *after* the opening parenthesis.
        Returns a tuple: (list of parameter token lists, index of closing parenthesis)
        """
        if start_idx >= len(tokenline):
            err(tokenline[-1], "Attempted to parse macro parameters past end of line.")

        invoked_param_list:list[list[Token]] = []
        current_param:list[Token] = []
        paren_depth = 0 # This is for nested parentheses *within* parameters
        close_paren_idx = -1

        for p_idx in range(start_idx, len(tokenline)):
            p_token = tokenline[p_idx]
            if p_token.type == "OPER" and p_token.v == "(":
                paren_depth += 1
            elif p_token.type == "OPER" and p_token.v == ")":
                if paren_depth == 0: # Found the matching closing parenthesis for the macro invocation
                    close_paren_idx = p_idx
                    break
                paren_depth -= 1
            elif p_token.type == "COMMA" and paren_depth == 0:
                invoked_param_list.append(current_param)
                current_param = []
                continue
            current_param.append(p_token)
        
        if close_paren_idx == -1:
            err(tokenline[start_idx], "Macro invocation has no matching closing parenthesis.")

        if current_param: # Add the last collected parameter
            invoked_param_list.append(current_param)
        
        return invoked_param_list, close_paren_idx

    def find_closeparen(self,tokenline:list[Token], start:int) -> int:
        if len(tokenline) < 1:
            raise ValueError(redmsg("You may not pass an empty line into find_closeparen()"))
        if start >= len(tokenline):
            err(tokenline[-1], "Attempted to find close parenthesis past end of line.")
        depth = 0
        for idx, token in enumerate(tokenline[start:], start):
            if token.type == "OPER" and token.v == "(":
                depth += 1
            if token.type == "OPER" and token.v == ")":
                depth -= 1
                if depth < 0:
                    break
        else:
            err(tokenline[start], "Line has no matching close parenthesis.")
        return idx


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

    def _resolve_to_string(self, token:Token, passid:int) -> Optional[str]:
        """
        Resolves a token to a string if it's a STRING type or an IDENT
        that resolves to a string in the symtable.
        """
        if token.type == "STRING":
            return unescape_string(token.v)
        elif token.type == "IDENT":
            if token.v in self.symtable:
                symval = self.symtable[token.v]
                if isinstance(symval, str):
                    return symval
                elif isinstance(symval, MacroDef):
                    # If it's a MacroDef, it might expand to a string.
                    # This is a recursive call, so we need to be careful about depth.
                    # For now, let's assume direct string resolution for IDENT.
                    # If a macro expands to a string, it should be handled by the pre-expansion step.
                    pass # Fall through to return None
                elif isinstance(symval, int):
                    # An IDENT resolving to an int is not a string for eval/concat purposes
                    pass # Fall through to return None
                else:
                    err(token, f"Symbol [{token.v}] type expected string or int, got {type(symval)}")
            # If not in symtable or not a string, it doesn't resolve to a string
            return None
        return None

    def eval_expr(self, tokenline:list[Token], passnum:int=1) -> int:
        ''' Parses an expression in tokenline between and including the position
            values in segment (start,end). Expressions are processed from left
            to right regardless of operator precedence, except for parenthesis
            use and whatever is needed to make unary operators work correctly.
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
