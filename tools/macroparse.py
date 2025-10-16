# Macro assembler
#
#
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
    DEBUG_TEST_TYPE = 0
    LBLCHARSTART = string.ascii_letters + '_'
    LBLCHARS = LBLCHARSTART + string.digits
    MATH_OPS = ('+','-','*','/','&','|','^','<<','>>','==','!=','<','>','<=','>=','&&','||')
    TOKEN_SPEC = [
        # Strings (single or double quoted, with escapes)
        ("STRING",      r'"(?:\\.|[^"\\])*"' + r"|" + r"'(?:\\.|[^'\\])*'"),
        # Hex and binary numbers
        ("HEX_NUMBER",  r'(?:\$[0-9A-Fa-f]+|[0-9A-Fa-f]+H(?![A-Za-z0-9_]))'),
        ("BIN_NUMBER",  r'(?:%[01]+|[01]+B(?![A-Za-z0-9_]))'),
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

    #===========================================================================
    # Marker to make the parse easier to find
    def parse(self, tokenlines:list[TokenLine], passid=1, depth=1):
        ''' NOTE: tokenlines WILL MUTATE.
        THIS IS BY DESIGN TO SUPPORT IN-PLACE MACRO EXPANSION. TOP-LEVEL CALLERS
        MUST DEEPCOPY tokenlines PRIOR TO CALLING AND REGENERATE FROM THE COPY
        IN ORDER TO CORRECTLY PERFORM A SECOND PASS.
        '''
        if len(list(TokenLine)) < 1:
            return
        if depth > self.MAX_RECURSION_DEPTH:
            leadingttokenline = tokenlines[0]
            raise MacroExpansionDepthExceeded(f"Expansion depth exceeded, starting at {leadingttokenline.filename}:{leadingttokenline.lineno}")
        if self.DEBUG_MODE:
            def printtok(tokens: tuple[Token, ...]):
                return ' '.join(t.value for t in tokens)
            print(f"Pass {passid} start.")

        inside_macrodef = False
        macrodef_expansion_buffer = list()
        macrodef_param_list:MacroParamList = None
        preop_if_level = 0

        # Macros must always be regenerated at the start of each pass since
        # they may expand differently the next time around
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
            lead_token = tokens[0]
            # Begin processing the lines
            if inside_macrodef:
                # If defining a macro, simply collect the lines.
                if lead_token.type == "PREOP" and lead_token.value.upper() == "#ENDMACRO":
                    # End of macro definition
                    mde = MacroDictEntry(macrodef_param_list.name, macrodef_param_list, macrodef_expansion_buffer)
                    self.macros[macrodef_param_list.name.value] = mde
                    inside_macrodef = False
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
                if lead_token_val == "#MACRO":
                    # Multi-line macro definition.
                    if inside_macrodef == True:
                        raise RecursionError(f"Illegal #MACRO nest at {tokenline.filename}:{tokenline.lineno}")
                    if len(tokens) < 2:
                        raise ValueError(f"Incomplete #MACRO operation at {tokenline.filename}:{tokenline.lineno}")
                    if tokens[1].type == "IDENTIFIER":
                        # Bare macro definition (no parameters).
                        mpl = MacroParamList(tokens[1], tuple(), 0, 0)
                        macrodef_param_list = mpl
                    elif tokens[1].type in ("MACRO_CALL", "DIRECTIVE_CALL"):
                        mpl = self.find_macro_and_param(tokenline)
                        macrodef_param_list = mpl
                    else:
                        raise ValueError(f"Unexpected token at {tokenline.filename}:{tokenline.lineno} pos {tokens[1].position}")
                    macrodef_expansion_buffer = list()
                    inside_macrodef = True
                # #DEFINE directives should be processed after other preprocessor directives.
                # NOTE: SPASM-ng appears to be parsing the macro body of a
                #   #DEFINE prior to assigning its contents. Run more assembler
                #   tests to verify exact behavior as it also varies between
                #   passes. Review SPASM-ng's source to discover more info.
                if lead_token_val == "#DEFINE":
                    if len(tokens) < 2:
                        raise ValueError(f"Incomplete #DEFINE operation at {tokenline.filename}:{tokenline.lineno}")
                    if tokens[1].type in ("IDENTIFIER","DIRECTIVE"):
                        # Bare macro definition (no parameters).
                        
                        mde_tokline = self.slice_tokline(tokenline, 2)
                        mpl = MacroParamList(tokens[1], tuple(), 0, 0)
                        mde = MacroDictEntry(mpl.name, mpl, [mde_tokline,])
                        self.macros[tokens[1].value] = mde
                    elif tokens[1].type in ("MACRO_CALL", "DIRECTIVE_CALL"):
                        mpl = self.find_macro_and_param(tokenline)
                        if mpl.endpos+1 >= len(tokenline.tokens):
                            raise ValueError(f"Disallowed empty macro body at {tokenline.filename}:{tokenline.lineno}")
                        mde_tokline = self.slice_tokline(tokenline,mpl.endpos)
                        mde = MacroDictEntry(mpl.name, mpl, [mde_tokline,] )
                        self.macros[tokens[1].value] = mde
                        pass
                    else:
                        raise ValueError(f"Unexpected token in {tokenline.filename}:{tokenline.lineno} pos {tokens[1].position}")
            # Macro expansion
            macexp_idx = 0
            macexp_tokens = list(tokens)
            restart_parse = False
            while True:
                token = macexp_tokens[macexp_idx]
                if token.type in ("DIRECTIVE", "IDENTIFIER", "DIRECTIVE_CALL", "MACRO_CALL"):
                    if token.value in self.macros:
                        mac_mde = self.macros[token.value]
                        if token.value.endswith("("):
                            # macexp_tokens is mutable so we can't trust that tokenline
                            # has the data we need in the place we need it in.
                            temp_tokline = self.splice_tokline(tokenline, None, None, macexp_tokens)
                            inv_mpl = self.find_macro_and_param(temp_tokline, macexp_idx)
                            assert(inv_mpl, "Macro expansion error. Tripping this error shouldn't be possible.")
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
                    elif token.type in ("DIRECTIVE_CALL", "MACRO_CALL"):
                        raise ValueError(f"Unknown or unidentified macro '{token.value}' or malformed call/expression at {tokenline.filename}:{tokenline.lineno}")
                # Ok. If we got here, we're still parsing the tokens on this line.
                macexp_idx += 1
                if macexp_idx >= len(macexp_tokens):
                    break

            if restart_parse:
                continue
            tokenline_index += 1
            '''
            TODO: Implement the other PREOP tokens. So far, we're working
            with #if, #ifdef, #ifndef, #else, #elif, #endif for flow control.
            '''
            # Next: Parsing the other PREOP tokens

            pass

        if self.DEBUG_MODE and passid == 1:
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
                elif baseval is not None and operator is None and token.value not in self.MATH_OPS:
                    # Ensure that a valid operator is now in place only if
                    # we have a prior operand existing.
                    operator = token
                    continue
                elif token.value == '(':
                    paren_level = 0
                    startpos = tokseg_idx+1
                    try:
                        while True:
                            tokseg_idx += 1
                            temptok = tokens[tokseg_idx]
                            if temptok.value == "(":
                                paren_level += 1
                            if temptok.value == ")":
                                paren_level -= 1
                                if paren_level < 0:
                                    endpos = tokseg_idx-1
                                    tokseg_idx += 1 #advance past close paren
                                    break
                    except:
                        raise ValueError(f"Closing parenthesis not found at {tokenline.filename}:{tokenline.lineno}")
                    if startpos > endpos:
                        # An empty paren sequence should result in zero.
                        newval = 0
                    else:
                        temptokline = TokenLine(tokens, tokenline.filename, tokenline.lineno)
                        temptokline = self.slice_tokline(temptokline, startpos, endpos)
                        try:
                            newval = self.parse_expression(temptokline)
                        except Exception as e:
                            raise e
                    if unary is not None and unary.value == '-':
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
        ''' Raises an exception on unrecognized tokens.
            Returns 0 on first-pass unidentified identifiers.
            Returns self.origin on operator "$"
            Returns None on other expression operators.
            Raises an exception on non-expression operators.
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
        elif token.type == "OPERATOR" and token.value in ",\\":
            raise ValueError(f"Non-expression symbol {token.value} used.")
        elif token.type == "OPERATOR":
            return None
        else:
            raise ValueError(f"Illegal token type {token.type} of value {token.value} found in expression.")
        
    def _expr_combine(self, baseval:int, operator_token:Token, newval:int) -> int:
        current_value = baseval
        operand_value = newval
        if operator_token.value == "+":
            current_value += operand_value
        elif operator_token.value == "-":
            current_value -= operand_value
        elif operator_token.value == "*":
            current_value *= operand_value
        elif operator_token.value == "/":
            if operand_value == 0:
                raise ZeroDivisionError(f"Division by zero at {operator_token.position}")
            current_value //= operand_value # Integer division
        elif operator_token.value == "&":
            current_value &= operand_value
        elif operator_token.value == "|":
            current_value |= operand_value
        elif operator_token.value == "^":
            current_value ^= operand_value
        elif operator_token.value == "<<":
            current_value <<= operand_value
        elif operator_token.value == ">>":
            current_value >>= operand_value
        elif operator_token.value == "==":
            current_value = 1 if current_value == operand_value else 0
        elif operator_token.value == "!=":
            current_value = 1 if current_value != operand_value else 0
        elif operator_token.value == "<":
            current_value = 1 if current_value < operand_value else 0
        elif operator_token.value == ">":
            current_value = 1 if current_value > operand_value else 0
        elif operator_token.value == "<=":
            current_value = 1 if current_value <= operand_value else 0
        elif operator_token.value == ">=":
            current_value = 1 if current_value >= operand_value else 0
        elif operator_token.value == "&&":
            current_value = 1 if (current_value != 0 and operand_value != 0) else 0
        elif operator_token.value == "||":
            current_value = 1 if (current_value != 0 or operand_value != 0) else 0
        else:
            raise ValueError(f"Unknown or unsupported operator '{operator_token.value}' at {operator_token.position}")
        return current_value

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
                elif tc == "'":
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
    def openfile(filepath):
        try:
            with open(filepath, 'r') as f:
                filedata = f.read()
        except FileNotFoundError:
            print(f"Error: File '{filepath}' not found.")
            return
        except IOError as e:
            print(f"Error reading file '{filepath}': {e}")
            return
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
        # For debugging, hardcode filename or use sys.argv
        # if len(sys.argv) != 2:
        #     print("Usage: python macroparse.py <filename>")
        #     sys.exit(1)
        # filename = sys.argv[1]
        filename = "tools/macrotest.z80"
    else:
        if len(sys.argv) != 2:
            print("Usage: python macroparse.py <filename>")
            sys.exit(1)
        filename = sys.argv[1]
    
    asm = MacroAssembler(filename)
    asm.process()
