# Macro assembler for partially reading .z80/.inc files
# that assemble with SPASM-ng
# Instruction parsing not supported.
# Will eventually be used to output data streams from all .db/.dw statements.
# 

import string, re, sys, random, copy
from typing import NamedTuple, Iterator, Optional, Tuple

class IncludeDepthExceeded(RecursionError):
    pass
class MacroExpansionDepthExceeded(RecursionError):
    pass

class SymbolNotFoundError(NameError):
    pass

class Token(NamedTuple):
    ''' Represents a token identified during lexical analysis. '''
    type: str
    value: str
    position: int

class TokenLine(NamedTuple):
    ''' A line of tokens and information about where it was tokenized from.
        Tokens are stored as a tuple to ensure immutability.
    '''
    tokens: tuple[Token, ...]
    filename: str
    lineno: int

class MacroParam(NamedTuple):
    ''' Represents a single parameter within a macro definition or invocation. '''
    paramid: int        # Starts at 0
    param: tuple[Token]

class MacroParamList(NamedTuple):
    '''
    Represents the name and parameters of a macro, used for both definition
    and invocation.

    Definition: Parameters may only contain a single IDENTIFIER token.
    Invocation: Parameters may contain any number of tokens, comma-separated,
                and may include nested macro invocations.
    Aspirational: Future implementation will handle built-in SPASM-ng macros
                  like "eval(" or "concat(" with special in-place expansion.
    '''
    name: Token
    params: tuple[MacroParam]
    startpos: int
    endpos: int

class MacroDictEntry(NamedTuple):
    '''
    Stores a macro's definition, including its name, parameter list, and
    the tokenized lines of its body.
    The `tokenlines` should exclude the #macro/#endmacro directives themselves.
    '''
    name: Token
    paramlist: MacroParamList
    tokenlines: list[TokenLine]


class MacroAssembler(object):
    MAX_RECURSION_DEPTH = 8
    DEBUG_MODE = True # Set to True to enable debug print statements and test code
    STOP_REPORTING_DEPTH_AT = 2
    #===========================================================================
    #===========================================================================
    # A border was added because I'm tired of trying to hunt for this variable
    # each time I want to change the test type.
    DEBUG_TEST_TYPE = 5
    #===========================================================================
    #===========================================================================
    LBLCHARSTART = string.ascii_letters + '_'
    LBLCHARS = LBLCHARSTART + string.digits
    MATH_OPS = ('+','-','*','/','&','|','^','<<','>>','==','!=','<','>','<=','>=','&&','||')
    TOKEN_SPEC = [
        # Strings (single or double quoted, with escapes)
        ("STRING",      r'"(?:\\.|[^"\\])*"' + r"|" + r"'(?:\\.|[^'\\])*'"),
        # Hex and binary numbers
        ("HEX_NUMBER",  r'(?:\$[0-9A-Fa-f]+|[0-9A-Fa-f]+[hH](?![A-Za-z0-9_]))'),
        ("BIN_NUMBER",  r'(?:%[01]+|[01]+[bB](?![A-Za-z0-9_]))'),
        ("DEC_NUMBER",  r'\d+(?:[dD](?![A-Za-z0-9_]))?'),
        # Function-like macro: identifier immediately followed by (
        ("MACRO_CALL",  r'[A-Za-z_][A-Za-z0-9_]*\('),
        # Directives (e.g., .define)
        ("DIRECTIVE_CALL",r'\.[A-Za-z_][A-Za-z0-9_]*\('),
        ("DIRECTIVE",   r'\.[A-Za-z_][A-Za-z0-9_]*'),
        # Preprocessor (e.g., #define)
        ("PREOP",       r'\#[A-Za-z_][A-Za-z0-9_]*'),
        # Identifiers
        ("IDENTIFIER",  r'[A-Za-z_][A-Za-z0-9_]*'),
        # Operators and special characters
        ("OPERATOR",    r'==|<=|>=|&&|!=|\|\||\$|>>|<<|[+\-*/&|^~<>()=\\]'),
        ("COMMA",       r','),
        # Whitespace (to be skipped)
        ("SKIP",        r'[ \t\r\n]+'),
        # Any unknown character
        ("MISMATCH",    r'.'),
    ]

    TOK_REGEX = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in TOKEN_SPEC)
    get_token = re.compile(TOK_REGEX).match

    def __init__(self, filepath):
        self.filepath = filepath
        self.defaultmacros = dict()
        self.reset()

    def reset(self):
        self.macros: dict[str, MacroDictEntry] = dict()
        self.symtable: dict[str, int|None] = dict()
        self.result = bytearray()
        self.origin = 0

    def process(self):
        tokenlines = self.preparse()
        tokenlinescopy = copy.deepcopy(tokenlines)
        self.parse(tokenlinescopy, 1)
        tokenlinescopy = copy.deepcopy(tokenlines)
        self.parse(tokenlinescopy, 2)
        return self.result

    def print_shorttok(self, tokenline:TokenLine|list[Token]|MacroDictEntry):
        def ptt(tl:list[Token]):
            print(' '.join(t.value for t in tl))
        if isinstance(tokenline, MacroDictEntry):
            tokenlines = tokenline.tokenlines
            for idx,tokenline in enumerate(tokenlines):
                print(f"Line {idx}: ",end='')
                ptt(tokenline.tokens)
            return
        if isinstance(tokenline, TokenLine):
            tokenline = tokenline.tokens
        ptt(tokenline)
        


    #===========================================================================
    # Marker to make the parse easier to find
    def parse(self, tokenlines:list[TokenLine], passid=1, depth=1, trace=None):
        ''' NOTE: tokenlines WILL MUTATE.
        THIS IS BY DESIGN TO SUPPORT IN-PLACE MACRO EXPANSION. TOP-LEVEL CALLERS
        MUST DEEPCOPY tokenlines PRIOR TO CALLING AND REGENERATE FROM THE COPY
        IN ORDER TO CORRECTLY PERFORM A SECOND PASS.
        '''
        if trace:
            print(f"Input tokenlines len: {len(tokenlines)}")
        if len(tokenlines) < 1:
            return
        if depth > self.MAX_RECURSION_DEPTH:
            leadingttokenline = tokenlines[0]
            raise MacroExpansionDepthExceeded(f"Expansion depth exceeded, starting at {leadingttokenline.filename}:{leadingttokenline.lineno}")
        if self.DEBUG_MODE:
            def printtok(tokens: tuple[Token, ...]):
                return ' '.join(t.value for t in tokens)
            #NOTE: 
            if depth < self.STOP_REPORTING_DEPTH_AT:
                print(f"Pass {passid} start at depth {depth}")

        inside_macrodef = False
        inside_macrodef_startline:TokenLine = None
        macrodef_expansion_buffer = list()
        macrodef_param_list:MacroParamList = None
        preop_if_level = 0

        # Macros must always be regenerated at the start of each pass since
        # they may expand differently the next time around
        if depth == 1:
            self.macros = copy.deepcopy(self.defaultmacros)

        # This is the parser section. This processes the tokenlines object
        # and potentially mutates it for macro expansion purposes. All
        # possible mutations are on or after the current tokenline.
        tokenline_index = 0
        while tokenline_index < len(tokenlines):
            try:
                tokenline = tokenlines[tokenline_index]
            except IndexError:
                print(f"Unhandled OOB assembly/buffer at index {tokenline_index} of {len(tokenlines)}")
                raise IndexError(f"Unhandled access at recursion level {depth}")
            tokens = tokenline.tokens
            if trace:
                print(f"Tokenline idx {tokenline_index}: ",end='')
                self.print_shorttok(tokens)

            if len(tokens) < 1:
                tokenline_index += 1
                continue
            lead_token = tokens[0]


            # Begin processing the lines
            if inside_macrodef:
                # If defining a macro, simply collect the lines.
                if lead_token.type == "PREOP" and lead_token.value.upper() == "#ENDMACRO":
                    # End of macro definition
                    mde = MacroDictEntry(macrodef_param_list.name, macrodef_param_list, macrodef_expansion_buffer)
                    self.macros[macrodef_param_list.name.value] = mde
                    inside_macrodef = False
                    inside_macrodef_startline = None
                    macrodef_param_list = None
                    macrodef_expansion_buffer = list()
                else:
                    # Collect lines for macro definition
                    macrodef_expansion_buffer.append(tokenline)
                tokenline_index += 1 # Consume the line
                continue # Skip further processing for this line

            #NOTE: There will be another PREOP block. After macro expansion.
            if lead_token.type == "PREOP":
                lead_token_val = lead_token.value.upper()
                if lead_token_val in ("#MACRO", "#DEFINE"):
                    if len(tokens) < 2:
                        raise ValueError(f"Incomplete {lead_token_val} operation at {tokenline.filename}:{tokenline.lineno}")
                    if tokens[1].value in ("eval(", "concat("):
                        raise ValueError(f"Illegal redefinition of reserved macro name {tokens[1].value} at {tokenline.filename}:{tokenline.lineno}")

                if lead_token_val == "#MACRO":
                    # Multi-line macro definition.
                    if inside_macrodef == True:
                        raise RecursionError(f"Illegal #MACRO nest at {tokenline.filename}:{tokenline.lineno}")
                    if tokens[1].type == "IDENTIFIER":
                        # Bare macro definition (no parameters).
                        mpl = MacroParamList(tokens[1], tuple(), 0, 0)
                        macrodef_param_list = mpl
                    elif tokens[1].type in ("MACRO_CALL", "DIRECTIVE_CALL"):
                        #Trailing parenthesis in args is not a problem here.
                        mpl = self.find_macro_and_param(tokenline)
                        macrodef_param_list = mpl
                    else:
                        raise ValueError(f"Unexpected token at {tokenline.filename}:{tokenline.lineno} pos {tokens[1].position}")
                    macrodef_expansion_buffer = list()
                    inside_macrodef_startline = tokenline
                    inside_macrodef = True
                    #Have to manually increment to avoid further line parsing.
                    tokenline_index += 1
                    continue
                # #DEFINE directives should be processed after other preprocessor directives.
                # NOTE: SPASM-ng appears to be parsing the macro body of a
                #   #DEFINE prior to assigning its contents. Run more assembler
                #   tests to verify exact behavior as it also varies between
                #   passes. Review SPASM-ng's source to discover more info.
                if lead_token_val == "#DEFINE":
                    if tokens[1].type in ("IDENTIFIER","DIRECTIVE"):
                        # Bare macro definition (no parameters).
                        mde_tokline = self.slice_tokline(tokenline, 2)
                        mde_expansionbuf = [mde_tokline,]
                        #DEBUGGING START
                        if tokens[1].value == "AA":
                            print("Trace off.")
                            inputtrace = False
                        else:
                            inputtrace = False
                        #DEBUGGING END
                        self.parse(mde_expansionbuf, passid, depth+1, inputtrace)
                        mpl = MacroParamList(tokens[1], tuple(), 0, 0)
                        mde = MacroDictEntry(mpl.name, mpl, mde_expansionbuf)
                        self.macros[tokens[1].value] = mde
                    elif tokens[1].type in ("MACRO_CALL", "DIRECTIVE_CALL"):
                        mpl = self.find_macro_and_param(tokenline)
                        if mpl.endpos+2 >= len(tokenline.tokens):
                            raise ValueError(f"Disallowed empty macro body at {tokenline.filename}:{tokenline.lineno}")
                        mde_tokline = self.slice_tokline(tokenline,mpl.endpos+1)
                        mde_expansionbuf = [mde_tokline,]
                        #Note: Parameterized #defines should not preemptively expand
                        #self.parse(mde_expansionbuf, passid, depth+1)
                        mde = MacroDictEntry(mpl.name, mpl, mde_expansionbuf )
                        self.macros[tokens[1].value] = mde
                        pass
                    else:
                        raise ValueError(f"Unexpected token in {tokenline.filename}:{tokenline.lineno} pos {tokens[1].position}")
                    tokenline_index += 1    #Moving past #define manually.
                    continue
            # Macro expansion
            macexp_idx = 0
            macexp_tokens = list(tokens)
            restart_parse = False
            if trace:
                print(f"Made it to macroexpansion section. tokens input: {len(macexp_tokens)} ")
            while macexp_idx < len(macexp_tokens):
                token = macexp_tokens[macexp_idx]
                if trace:
                    print(token)
                if token.type in ("DIRECTIVE", "IDENTIFIER", "DIRECTIVE_CALL", "MACRO_CALL"):
                    if token.value in self.macros:
                        mac_mde = self.macros[token.value]
                        if token.value.endswith("("):
                            # macexp_tokens is mutable so we can't trust that tokenline
                            # has the data we need in the place we need it in.
                            temp_tokline = self.splice_tokline(tokenline, None, None, macexp_tokens)
                            inv_mpl = self.find_macro_and_param(temp_tokline, macexp_idx)
                            assert inv_mpl, "Macro expansion error. Tripping this error shouldn't be possible."
                        else:
                            inv_mpl = MacroParamList(token, tuple(), macexp_idx, macexp_idx)
                        params_found = len(inv_mpl.params)
                        params_expected = len(mac_mde.paramlist.params)
                        if  params_found != params_expected:
                            raise ValueError(f"Macro param count mismatch. Found {params_found}, expected {params_expected} in {tokenline.filename}:{tokenline.lineno} ")
                        # Prepare expansion buffer
                        invocation_expansion_buffer:list[TokenLine] = list()
                        for mdef_tokline in mac_mde.tokenlines:
                            mdef_tokenbuf:list[Token] = list()
                            mdef_paramlist = [p.param[0] for p in mac_mde.paramlist.params]
                            for mdef_token in mdef_tokline.tokens:
                                #backfill parameters
                                if mdef_token.type == "IDENTIFIER":
                                    if mdef_token.value in mdef_paramlist:
                                        idx = mdef_paramlist.index(mdef_token.value)
                                        mdef_tokenbuf.extend(list(inv_mpl.params[idx].param))
                                else:
                                    mdef_tokenbuf.append(mdef_token)
                            invocation_expansion_buffer.append(self.splice_tokline(mdef_tokline,None, None, mdef_tokenbuf))
                        try:
                            self.parse(invocation_expansion_buffer, passid, depth+1)
                        except MacroExpansionDepthExceeded as e:
                            print(f"[{depth}] -- Macro expansion stack unwinding from {tokenline.filename}:{tokenline.lineno}")
                            raise e
                        except Exception as e:
                            print(f"[{depth}] -- Error encountered, unwinding from {tokenline.filename}:{tokenline.lineno}")
                            raise e
                        # Expansion buffer created and modified in-place.
                        # Now insert first line into macexp_tokens, replacing
                        # the tokens that formed that macro.
                        for line in invocation_expansion_buffer:
                            print(line.tokens)
                        start = inv_mpl.startpos
                        end = inv_mpl.endpos+1
                        macexp_tokens[start:end] = []   #Erase macro invocation
                        ieb = invocation_expansion_buffer
                        if len(ieb) == 0:
                            # Macro empty. Reconstruct line, then restart parse
                            newtokline = self.splice_tokline(tokenline, None, None, macexp_tokens)
                            tokenlines[tokenline_index] = newtokline
                            restart_parse = True
                        elif len(ieb) == 1:
                            # Single line macro. Replace current line, then restart.
                            macexp_tokens[start:start] = ieb[0].tokens
                            newtokline = self.splice_tokline(tokenline, None, None, macexp_tokens)
                            tokenlines[tokenline_index] = newtokline
                            restart_parse = True
                        else:
                            # Multi-line macro. Replace current line, extending
                            # with all new lines. Then restart to properly parse
                            # new and improved first line.
                            left = self.splice_tokline(tokenline, None, None, macexp_tokens[:start] + ieb[0].tokens)
                            ieb[0] = left
                            right = self.splice_tokline(tokenline, None, None, ieb[-1].tokens + macexp_tokens[:start])
                            ieb[-1] = right
                            tokenlines[tokenline_index:tokenline_index+1] = ieb
                            restart_parse = True
                        break
                    elif token.type == "MACRO_CALL" and token.value.lower() == "eval(":
                        pass
                    elif token.type == "MACRO_CALL" and token.value.lower() == "concat(":
                        pass
                    elif token.type in ("DIRECTIVE_CALL", "MACRO_CALL"):
                        print(self.macros.keys())
                        raise ValueError(f"Unknown or unidentified macro '{token.value}' or malformed call/expression at {tokenline.filename}:{tokenline.lineno}")
                # Ok. If we got here, we're still parsing the tokens on this line.
                macexp_idx += 1
                if trace:
                    print(f"Macroparse status idx: {macexp_idx} at {tokenline.filename}:{tokenline.lineno}")

            if restart_parse:
                continue
            tokenline_index += 1
            '''
            TODO: Implement the other PREOP tokens. So far, we're working
            with #if, #ifdef, #ifndef, #else, #elif, #endif for flow control.
            '''

            # NOTE: If depth > 1. bleh. fill this note in later.
            # the intent is to ensure that recursion doesn't result in data
            # output but instead collapses expressions into their resultant
            # values and overwrites them inline so that depth == 1 can process
            # them, as any depth > 1 is reserved for processing macro expansions
            # and for #define, which apparently tries to evalulate the line
            # prior to setting the contents of the define. And yet somehow
            # preserves definitions for strings and acts as a text replacement
            # if concat() is somehow encountered? concat()'s purpose is to
            # concatenate strings to form a label, which can then be used as
            # text replacement for right-hand values in .equ statements and such.

            #NOTE: Maybe that can be implemented by not actually running an
            #       expression parse on any of the contents if depth > 1 since
            #       we're expanding macros at that point?
            #       But that still doesn't solve the bare #DEFINE problem >.>

            #TODO: Directive parsing. Find all the [backslash] chars and split
            # them wrt newlines.


            pass
        if depth==1 and inside_macrodef:
            raise ValueError(f"Failed to close #MACRO definition starting at {inside_macrodef_startline.filename}:{inside_macrodef_startline.lineno}")

        if self.DEBUG_MODE and passid == 1 and depth == 1:
            if self.DEBUG_TEST_TYPE == 0:
                # Print what the macros filled out to.
                print("Printing macros...")
                for k in self.macros:
                    print(self.macros[k].name.value)

            if self.DEBUG_TEST_TYPE == 1:
                # Print the contents of each macro
                for k in self.macros:
                    v = self.macros[k]
                    print(f"Macro: {k}")
                    print(f"Macro params: {v.paramlist.params}")
                    for tokline in v.tokenlines:
                        print(tokline)

            if self.DEBUG_TEST_TYPE == 2:
                # Basic visual test for exposing underlying tokens
                for tokenline in tokenlines:
                    #if tokenline.filename.lower() == "src/inc/osdefs.inc":
                        print(tokenline)

            if self.DEBUG_TEST_TYPE == 3:
                # Visual test for summarizing TokenList contents, space-delimit
                for item in tokenlines:
                    tokval = printtok(item.tokens)
                    print(f"TokenLine: file:{item.filename}, linenum: {item.lineno}, line: [{tokval}]")

            if self.DEBUG_TEST_TYPE == 4:
                # Visual test for identifying all function-like macro on a line
                for tokenline in tokenlines:
                    startpos = 0
                    while True:
                        try:
                            v = self.find_macro_and_param(tokenline, startpos)
                        except Exception as e:
                            print(f"Error encountered: {e}")
                        if v is None:
                            break
                        startpos = v.endpos
                        numparams = len(v.params)
                        print(f"Macro sig found in {tokenline.filename}: {tokenline.lineno}, pos: {v.startpos}, params: {numparams}")
                        print(f"Macroname: {v.name.value}")
                        for i in range(numparams):
                            print(f"Param {i}: {printtok(v.params[i].param)}")
            if self.DEBUG_TEST_TYPE == 5:
                # Visual test for identifying what each macro expands to
                # Summary notation used
                print("Debug test type 5 start")
                print(self.macros.keys())
                for k in self.macros:
                    n, plist, toklines = self.macros[k]
                    if len(toklines) == 0:
                        print(f"Macro {n.value} is empty.")
                    if len(toklines) == 1:
                        print(f"Macro {n.value}: {' '.join(t.value for t in toklines[0].tokens)} ")
                    if len(toklines) > 1:
                        print(f"Multiline macro {n.value} expands to:")
                        for tokline in toklines:
                            print(f"\t{' '.join(t.value for t in tokline.tokens)}")
                pass
        return
        
    def expr_parse(self, tokenline:TokenLine, segment:Tuple[int,int], passnum:int=1) -> Token:
        tokseg = self.slice_tokline(tokenline, segment[0], segment[1])
        tokens = tokseg.tokens
        if len(tokens) == 0:
            raise ValueError(f"Expression segment returned zero at {tokenline.filename}:{tokenline.lineno}")
        tokseg_idx = 0
        baseval:int = None
        operator:Token = None
        unary:Token = None
        newval:int = None
        while tokseg_idx < len(tokens):
            token = tokens[tokseg_idx]
            if "NUM" in token.type:
                newval = self._expr_parse_getval(token, passnum)
                if unary is not None:
                    if unary.value == "-":
                        newval = -newval
                    unary = None
                if baseval is not None and operator is not None:
                    baseval = self._expr_combine(baseval, operator, newval)
                    operator = None
                    continue
                elif baseval is None:
                    if operator is None:
                        #Reachable here if this is the first number
                        baseval = newval
                    else:
                        raise ValueError(f"Illegal operator position at {tokenline.filename}:{tokenline.lineno} col {operator.position}")
            if token.type == "OPERATOR":
                if token.value in ('+','-') and unary is None:
                    #If trying to predict a unary, one must not be set.
                    if baseval is None or operator is not None:
                        # If not baseval, then what we have must be unary.
                        # If we have an operator, then this could be a unary.
                        unary = token
                elif (baseval is not None) and (operator is None) and (token.value in self.MATH_OPS):
                    # Set a valid operator if we haven't found one yet and is in
                    # a plausible position for it to count.
                    operator = token
                    continue
                elif token.value == '(':
                    paren_level = 0
                    startpos = tokseg_idx+1
                    if tokseg_idx >= len(tokens):
                        raise ValueError(f"Unexpected expression ending after open paren at {tokenline.filename}:{tokenline.lineno} col {token.position}")
                    while True:
                        tokseg_idx += 1
                        if tokseg_idx >= len(tokens):
                            raise ValueError(f"Closing parenthesis not found at around {tokenline.filename}:{tokenline.lineno} col {token.position}")
                        temptok = tokens[tokseg_idx]
                        if temptok.value == "(":
                            paren_level += 1
                        if temptok.value == ")":
                            paren_level -= 1
                            if paren_level < 0:
                                endpos = tokseg_idx-1
                                tokseg_idx += 1 #advance past close paren
                                break
                    if startpos > endpos:
                        # An empty paren sequence should result in zero.
                        newval = 0
                    else:
                        temptokline = TokenLine(tokens, tokenline.filename, tokenline.lineno)
                        temptokline = self.slice_tokline(temptokline, startpos, endpos)
                        try:
                            newval = self.expr_parse(temptokline, 0, len(temptokline.tokens), passnum)
                        except Exception as e:
                            raise e
                    if unary is not None:
                        if unary.value == '-':
                            newval = -newval
                        unary = None
                    if baseval is not None and operator is not None:
                        baseval = self._expr_combine(baseval, operator, newval)
                        operator = None
                    elif baseval is None and operator is None:
                        baseval = newval
                    else:
                        raise ValueError(f"Incomplete expression at {tokenline.filename}:{tokenline.lineno}")
                    continue
                else:
                    raise ValueError(f"Invalid operator or operator use at {tokenline.filename}:{tokenline.lineno} col {token.position}")
            tokseg_idx += 1
        if operator is not None:
            raise ValueError(f"Incomplete expression detected at {tokenline.filename}:{tokenline.lineno}")
        if baseval is None:
            raise ValueError(f"Malformed expression or expression does not yield a value at {tokenline.filename}:{tokenline.lineno}")
        return Token("DEC_NUMBER", str(baseval), tokenline.tokens[0].position)

    def _expr_parse_getval(self, token: Token, passnum:int) -> int:
        ''' Returns 0 on first-pass unidentified identifiers.
            Returns self.origin on operator "$"
            Raises an exception on non-expression operators or unrecognized tokens.
        ''' 
        if "NUMBER" in token.type:
            if token.value.startswith("$"):
                return int(token.value[1:], 16)
            else:
                v = token.value
                if token.value.endswith(('H','h','D','d','B','b')):
                    v = v[:-1]
                return int(v, {"H":16, "B":2, "D":10}[token.type[0]])
        elif token.type == "IDENTIFIER":
            if token.value not in self.symtable:
                if passnum == 1:
                    return 0
                else:
                    raise ValueError(f"Symbol {token.value} not found.")
            else:
                return self.symtable[token.value]
        
        elif token.type == "OPERATOR" and token.value == "$":
            return self.origin
        # Note: This condition is removed. It was intende for use in
        # expression locating, but that never really happened.
        #elif token.type == "OPERATOR" and token.value in self.MATH_OPS:
        #    return None
        else:
            raise ValueError(f"Illegal token type {token.type} of value {token.value} found in expression.")
        
    def _expr_combine(self, baseval:int, oper:Token, newval:int) -> int:
        if oper.value == "+":
            baseval += newval
        elif oper.value == "-":
            baseval -= newval
        elif oper.value == "*":
            baseval *= newval
        elif oper.value == "/":
            if newval == 0:
                raise ZeroDivisionError(f"Division by zero at {oper.position}")
            baseval //= newval # Integer division
        elif oper.value == "&":
            baseval &= newval
        elif oper.value == "|":
            baseval |= newval
        elif oper.value == "^":
            baseval ^= newval
        elif oper.value == "<<":
            baseval <<= newval
        elif oper.value == ">>":
            baseval >>= newval
        elif oper.value == "==":
            baseval = 1 if baseval == newval else 0
        elif oper.value == "!=":
            baseval = 1 if baseval != newval else 0
        elif oper.value == "<":
            baseval = 1 if baseval < newval else 0
        elif oper.value == ">":
            baseval = 1 if baseval > newval else 0
        elif oper.value == "<=":
            baseval = 1 if baseval <= newval else 0
        elif oper.value == ">=":
            baseval = 1 if baseval >= newval else 0
        elif oper.value == "&&":
            baseval = 1 if (baseval != 0 and newval != 0) else 0
        elif oper.value == "||":
            baseval = 1 if (baseval != 0 or newval != 0) else 0
        else:
            raise ValueError(f"Unknown or unsupported operator '{oper.value}' at {oper.position}")
        return baseval

    def slice_tokline(self, tokenline:TokenLine, start, end=None) -> TokenLine:
        return TokenLine(tokenline.tokens[start:end], tokenline.filename, tokenline.lineno)

    def splice_tokline(self, tokenline:TokenLine, start=None, end=None, newtokens:Optional[list[Token]]=None) -> TokenLine:
        ''' Reference notes:
        `newtokens` may be empty (for deletion).
        Insertion: `start` == `end`. 
        Replace:   `start` < `end`, where numtokens replaced at `start` is `end`-`start`. 
        Append:    `start` == `len(tokenline.tokens)`, `end` == `None`. 
        Prepend:   `start` == `None`, `end` == 0. 
        Replace All: `start` and `end` == `None`
        '''
        oldtokens = list(tokenline.tokens)
        oldtokens[start:end] = list(newtokens) if newtokens else []
        return TokenLine(tuple(oldtokens), tokenline.filename, tokenline.lineno)

    @staticmethod
    def locate_expressions(tokenline: TokenLine) -> Tuple[Tuple[int, int], ...]:
        '''
        Locates expressions in a TokenLine. An expression is a consecutive sequence
        of tokens of the type HEX_NUMBER, BIN_NUMBER, DEC_NUMBER, IDENTIFIER, or OPERATOR.
        All other token types are considered non-expression tokens.
        This function returns a tuple of 2-tuples, each 2-tuple represents the
        start and end position (inclusive) of each expression.
        If no expressions are found, the main tuple should be empty.
        NOTE: Addendum: operator [BACKSLASH] is considered a logical newline so
            this is to be both a separator and to be omitted from an expression.
        '''

        expression_token_types = {"HEX_NUMBER", "BIN_NUMBER", "DEC_NUMBER", "IDENTIFIER", "OPERATOR"}
        expressions = []
        current_expression_start = None

        for i, token in enumerate(tokenline.tokens):
            if token.type in expression_token_types:
                if not (token.type == "OPERATOR" and token.value == "\\"):
                    if current_expression_start is None:
                        current_expression_start = i
            else:
                if current_expression_start is not None:
                    expressions.append((current_expression_start, i - 1))
                    current_expression_start = None
        
        if current_expression_start is not None:
            expressions.append((current_expression_start, len(tokenline.tokens) - 1))
            
        return tuple(expressions)


    def preparse(self, filepath=None, inccount=1) -> list[TokenLine]:
        ''' Tokenizes the file at <filepath>, recursively resolving all #include
            directives, and returns a list of TokenLine objects.
        '''
        cls = self.__class__
        if filepath is None:
            filepath = self.filepath
        if inccount > cls.MAX_RECURSION_DEPTH:
            raise IncludeDepthExceeded(f"#INCLUDE depth exceeded on file: {filepath}")
        tokenlines = []
        filedata = self.openfile(filepath)
        if filedata is None:
            raise FileNotFoundError(f"File {filepath} not found.")
        lines = filedata.splitlines()
        for linenum, line in enumerate(lines, start=1):
            if not line.strip():
                continue
            line = self.strip_comments(line)
            if not line:
                continue
            try:
                tokens = list(self.tokenize(line))
            except Exception as e:
                print(f"Error occurred at line {linenum} in file {filepath}.")
                raise e
            if not tokens:
                continue
            # Insert tokenized include file, or insert tokenized.
            if tokens[0].value == "#include":
                if len(tokens) < 2 or tokens[1].type != "STRING":
                    raise ValueError(f"Malformed #INCLUDE at {filepath}:{linenum}.")
                incpath = self.unescape_string(tokens[1].value)
                try:
                    tokenlines.extend(self.preparse(incpath, inccount+1))
                except IncludeDepthExceeded as e:
                    print(f"[{inccount}] -- #INCLUDE stack unwinding from {filepath}:{linenum}")
                    raise e
                except FileNotFoundError as e:
                    print(f"#INCLUDE statement asked for a missing file at {filepath}:{linenum}, error to follow.")
                    raise e
            else:
                tokenline = TokenLine(tuple(tokens), filepath, linenum)
                tokenlines.append(tokenline)
        return tokenlines
    
    def find_macro_and_param(self, tokenline:TokenLine, start=0) -> Optional[MacroParamList]:
        paren_level = None
        macro_name:Token = None
        macro_params:list[MacroParam] = []
        param_buffer:list[Token] = []
        param_id = 0
        for pos in range(start,len(tokenline.tokens)):
            token = tokenline.tokens[pos]
            # Iterate until it catches on a macro call. Once it does, any
            # further macros will simply increase parenthesis level as if it
            # were a '(' operator, since they're basically named open parens.
            if token.type in ("DIRECTIVE_CALL", "MACRO_CALL") and token.value.endswith('('):
                if not macro_name:
                    start_pos = pos
                    paren_level = 0
                    macro_name = token
                    continue
                else:
                    paren_level += 1
            # If we haven't caught a macro, skip further processing.
            if not macro_name:
                continue
            # Otherwise, keep iterating with the available symbols.
            elif token.type == "OPERATOR":
                if token.value == "(":
                    paren_level += 1
                if token.value == ")":
                    paren_level -= 1
                    # 0 is the baseline. If falls below 0, we fell out of macro.
                    if paren_level < 0:
                        param = MacroParam(param_id, tuple(param_buffer))
                        macro_params.append(param)
                        end_pos = pos
                        break
            elif token.type == "COMMA" and paren_level == 0:
                param = MacroParam(param_id, tuple(param_buffer))
                macro_params.append(param)
                param_buffer = []
                param_id += 1
                continue
            # All other tokens are placed into the buffer with the above
            # ensuring that the commas and the final closing paren is not added.
            # Parens that are part of params, however, will be added.
            param_buffer.append(token)
        else:
            # Loop ended without breaking. Means that a function signature
            # wasn't found, or the final close paren wasn't found.
            if not macro_name:
                return None
            raise ValueError(f"Matching parenthesis not found in {tokenline.filename}: Ln {tokenline.lineno}, starting at {macro_name.position}")
        result = MacroParamList(macro_name, tuple(macro_params), start_pos, end_pos)
        return result
        
    def tokenize(self, code:str) -> Iterator[Token]:
        cls = self.__class__
        pos = 0
        mo = cls.get_token(code, pos)
        while mo:
            kind = mo.lastgroup
            value = mo.group()
            if kind == "SKIP":
                pass
            elif kind == "MISMATCH":
                raise SyntaxError(f"Unexpected character {value!r} at position {pos}")
            else:
                yield Token(kind, value, pos)
            pos = mo.end()
            mo = cls.get_token(code, pos)
        if pos != len(code):
            raise SyntaxError(f"Unexpected character {code[pos]!r} at position {pos}")

    @staticmethod
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

    @staticmethod
    def openfile(filepath) -> Optional[str]:
        try:
            with open(filepath, 'r') as f:
                filedata = f.read()
        except FileNotFoundError:
            print(f"Error: File '{filepath}' not found.")
            return None
        except IOError as e:
            print(f"Error reading file '{filepath}': {e}")
            return None
        return filedata

    @staticmethod
    def strip_comments(code: str) -> str:
        def strip_line(line: str) -> str:
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
        return '\n'.join(strip_line(ln) for ln in code.splitlines())




if __name__ == "__main__":
    if MacroAssembler.DEBUG_MODE:
        filename = "tools/macrotest.z80"
    else:
        if len(sys.argv) != 2:
            print("Usage: python macroparse.py <filename>")
            sys.exit(1)
        filename = sys.argv[1]
    
    asm = MacroAssembler(filename)
    asm.process()
