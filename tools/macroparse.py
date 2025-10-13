# Macro assembler
#
#
#

import string, re, sys, random, copy
from typing import NamedTuple, Iterator, Optional

class IncludeDepthExceeded(RecursionError):
    pass

class Token(NamedTuple):
    ''' Self-explanitory. Tokens are important for parsing.
        See MacroAssembler.TOKEN_SPEC for token types.
    '''
    type: str
    value: str
    position: str

class TokenLine(NamedTuple):
    ''' A line of tokens and information about where it was tokenized from.
        Tokens are stored as a tuple to ensure immutability.
    '''
    tokens: tuple[Token, ...]
    filename: str
    lineno: int

class MacroParam(NamedTuple):
    ''' NOTE: See the doc for MacroParamList for this object's usage.
    '''
    paramid: int        #Starts at 0
    param: tuple[Token]

'''
Used for both macro definition and macro invocation.
NOTE: In this doc, MacroAssembler's class instances are referred to as `asm`
NOTE: In this doc, MacroParamList's class instances are referred to as `self`
NOTE: This entire text block is mostly geared towards planning *how* to use
      this object's instance rather than a documentation *of* it, though
      it ought to do a good job of that too.
Definition: See MacroDictEntry() documentation. Parmeters may only contain
  a 1-tuple containing a single token of type IDENTIFIER.
Invocation: Parameters may contain any number of tokens, comma-separated.
  Parameters may contain macro invocations. Do not evaluate parameters here.
  Collapse all tokens containing this macro's invocation into this object.
  NOTE: At this step, if the macro name is "eval(" or "concat(", stop here.
      These are built-in macros for SPASM-ng and perform special operations
      that standard macros cannot. Skip all other steps listed here and know
      that the results of this operation will expand in-place. This will
      likely involve some kind of evalulate method that I haven't yet made.
  Get macro definition (MacroDictEntry) object by doing 
  asm.macros[self.name.value]
  Then expand the entirety of its tokenlines into an expansion buffer.
  YOU MUST DO A DEEP COPY TO AVOID MUTATING THE DEFINITION MACRO BODY.
  Match any use of asm.macros[self.name.value].paramlist.params[N].param.value
  with any token's value of the IDENTIFIER type in the expansion buffer.
  Match asm.macros[self.name.value].paramlist.params[N].param.paramid with
  self.params.paramid and get the matching self's param. Replace all tokens
  matched in the expansion buffer with their corresponding self's param.
  Then call an evaluation method on this expansion buffer.
  NOTE: Whenever you evalulate these, it's highly likely that global state
      will mutate. This behavior is required for proper processing so long as
      you parse one line at a time. In the order they appear.
  It is entirely possible that the expansion buffer contains macro
  macro invocations as well. During evalulation, these too will create an
  expansion buffer. It is here you can check for macro expansion recursion
  depth.
NOTE: The endpos property's is intended to be used after collecting parameter
  tokens. Further use outside of that may end up unreliable since all those
  tokens are supposed to collapse into a single token via its caller, so
  the TokenLine it comes from ends up being mutated during processing.
'''
class MacroParamList(NamedTuple):
    name: Token
    params: tuple[MacroParam]
    startpos: int
    endpos: int

'''
Used in MacroAssembler().macros for the value. The key used to reference this
value should match MacroDictEntry().paramlist.name.value
tokenlines' first (and possibly only) entry should exclude all parts of the
line not part of the macro's body. For macros defined with #macro, this is
often empty, and may be omitted. 
A duplicate of the name was added. Just in case I need it.
'''
class MacroDictEntry(NamedTuple):
    name: Token
    paramlist: MacroParamList
    tokenlines: list[TokenLine]


class MacroAssembler(object):
    MAX_RECURSION_DEPTH = 8
    LBLCHARSTART = string.ascii_letters + '_'
    LBLCHARS = LBLCHARSTART + string.digits
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
        self.filedata = self.openfile(filepath)
        self.filepath = filepath
        self.defaultmacros = dict()
        self.reset()

    def reset(self):
        self.macros: dict[str, MacroDictEntry] = dict()
        self.symtable: dict[str, int|None] = dict()
        self.result = bytearray()

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
            except:
                break
            tokens = tokenline.tokens
            lead_token = tokens[0]
            # Begin processing the lines
            if inside_macrodef:
                # If defining a macro, simply collect the lines.
                ''' NOTE: WE ARE USING THE EXPANSION BUFFER WHILE COLLECTING
                    MACRO DATA FOR DEFINITION. WE ARE NOT ACTUALLY EXPANDING
                    THE MACRO AT THIS POINT, BUT WHEN A MACRO IS INVOKED, IT
                    IS USED. THIS MAY ALSO BE THE REASON WHY IN SPASM-NG, IT
                    IS NOT POSSIBLE TO DEFINE A MULTILINE MACRO INSIDE AN
                    EXISTING MULTILINE MACRO (I.E. NEST #MACRO PREOPS). THOUGH
                    THIS IS PRESUMING THAT THE EXPANSION BUFFER BEHAVIOR IS
                    THE SAME. FOR ALL I KNOW, IT'S JUST NESTED POINTERS.
                    WHICH DOESN'T ACTUALLY RESOLVE ANYTHING. ANYHOO, THIS IS
                    JUST RAMBLING SO THAT I CAN SIGN OFF WITHOUT ANY CONCERNS
                    THAT I LEFT A THOUGHT UNEXPRESSED, AND THUS, LIABLE FOR
                    FORGETTING.
                '''
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

            if lead_token.type == "PREOP":
                lead_token_val = lead_token.value.upper()
                if lead_token_val == "#MACRO":
                    # Must be first in preops. These are multi-line macro defs.
                    # The lines that begin and end the macro (#macro/#endmacro)
                    # may not contain the macro body. Not an error. Just ignored
                    if inside_macrodef == True:
                        raise RecursionError(f"Illegal #MACRO nest at {tokenline.filename}:{tokenline.lineno}")
                    if len(tokens) < 2:
                        raise ValueError(f"Incomplete #MACRO operation at {tokenline.filename}:{tokenline.lineno}")
                    if tokens[1].type == "IDENTIFIER":
                        # Bare macro def
                        # There... really isn't much to do.
                        mpl = MacroParamList(tokens[1], tuple(), 0, 0)
                        macrodef_param_list = mpl
                    elif tokens[1].type in ("MACRO_CALL", "DIRECTIVE_CALL"):
                        mpl = self.find_macro_and_param(tokenline)
                        macrodef_param_list = mpl
                    else:
                        raise ValueError(f"Unexpected token at {tokenline.filename}:{tokenline.lineno} pos {tokens[1].position}")
                    macrodef_expansion_buffer = list()
                    inside_macrodef = True
                # Insert #if #ifdef #ifndef #else #elif #endif here to allow
                # further flow control.

                #Really, #DEFINE needs to be among the last to be processed
                # among the preops. 
                if lead_token_val == "#DEFINE":
                    if len(tokens) < 2:
                        raise ValueError(f"Incomplete #DEFINE operation at {tokenline.filename}:{tokenline.lineno}")
                    if tokens[1].type in ("IDENTIFIER","DIRECTIVE"):
                        # This is a bare macro def. These have no parameters.
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

            tokenline_index += 1
            pass

        # Postprocess testing
        if passid == 1:
            #'''            
            #Print what the macros filled out to.
            print("Printing macros...")
            for k in self.macros:
                print(self.macros[k].name.value)
                #print(self.macros[k])
            #'''

            '''
            #Print the contents of each macro
            for k in self.macros:
                v = self.macros[k]
                print(f"Macro: {k}")
                print(f"Macro params: {v.paramlist.params}")
                for tokline in v.tokenlines:
                    print(tokline)
            '''

            # Let's put testing in here for now. We won't need both passes
            # until we're ready to perform label resolution.
            '''
            # Basic visual test for exposing underlying tokens
            for tokenline in tokenlines:
                print(tokenline)
            '''
            '''
            # Visual test for summarizing TokenList contents, space-delimit
            for item in tokenlines:
                tokval = printtok(item.tokens)
                print(f"TokenLine: file:{item.filename}, linenum: {item.lineno}, line: [{tokval}]")

            '''
            '''
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
            '''
        return
    
    def slice_tokline(self, tokenline:TokenLine, start, end=None):
        return TokenLine(tokenline.tokens[start:end], tokenline.filename, tokenline.lineno)

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
                incpath = self.unescape_string(tokens[1].value) #Verified.
                try:
                    tokenlines.extend(self.preparse(incpath, inccount+1))
                except IncludeDepthExceeded as e:
                    print(f"[{inccount}] -- #INCLUDE stack unwinding from {filepath}:{linenum}")
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
        #print(result)
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
    '''
    if len(sys.argv) != 2:
        print("Usage: python macroparse.py <filename>")
        sys.exit(1)
    filename = sys.argv[1]
'''
    filename = "tools/macrotest.z80"
    
    asm = MacroAssembler(filename)
    asm.process()
