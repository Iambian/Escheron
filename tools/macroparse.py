# Macro assembler
#
#
#

import string, re, sys, random
from typing import NamedTuple, Iterator

class IncludeDepthExceeded(RecursionError):
    pass

'''
Self-explanitory. Tokens are important for parsing.
See MacroAssembler.TOKEN_SPEC for token types.
'''
class Token(NamedTuple):
    type: str
    value: str
    position: str

'''
Contains a line of tokens and information about where it was tokenized from.
'''
class TokenLine(NamedTuple):
    tokens: list[Token]
    filename: str
    lineno: int

'''
NOTE: See the doc for MacroParamList for this object's usage.
'''
class MacroParam(NamedTuple):
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
'''
class MacroDictEntry(NamedTuple):
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
        self.reset()

    def reset(self):
        self.macros: dict[str, MacroDictEntry] = dict()
        self.symtable: dict[str, int|None] = dict()
        self.result = bytearray()

    def process(self):
        tokenlines = self.preparse()
        return self.parse(tokenlines)

    #===========================================================================
    # Marker to make the parse easier to find
    def parse(self, tokenlines:list[TokenLine]):
        def printtok(tokens: list[Token]):
            return ' '.join(t.value for t in tokens)
        for parser_pass in range(1,3):
            print(f"Pass {parser_pass} start.")
            if parser_pass == 1:
                # Let's put testing in here for now. We won't need both passes
                # until we're ready to perform label resolution.
                '''                
                for item in tokenlines:
                    tokval = printtok(item.tokens)
                    print(f"TokenLine: file:{item.filename}, linenum: {item.lineno}, line: [{tokval}]")

                '''
                for tokenline in tokenlines:
                    startpos = 0
                    while True:
                        v = self.find_macro_and_param(tokenline, startpos)
                        if v is None:
                            break
                        startpos = v.endpos
                        numparams = len(v.params)
                        print(f"Macro sig found in {tokenline.filename}: {tokenline.lineno}, pos: {v.startpos}, params: {numparams}")
                        print(f"Macroname: {v.name.value}")
                        for i in range(numparams):
                            print(f"Param {i}: {printtok(v.params[i].param)}")
        return


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
                tokenline = TokenLine(tokens, filepath, linenum)
                tokenlines.append(tokenline)
        return tokenlines
    
    def find_macro_and_param(self, tokenline:TokenLine, start=0) -> MacroParamList|None:
        paren_level = None
        macro_name:Token = None
        macro_params:list[MacroParam] = []
        param_buffer:list[Token] = []
        param_id = 0
        for pos in range(start,len(tokenline.tokens)):
            token = tokenline.tokens[pos]
            if token.type in ("DIRECTIVE_CALL", "MACRO_CALL") and token.value.endswith('('):
                start_pos = pos
                paren_level = 0
                macro_name = token
                continue
            if paren_level is None:
                continue
            # The above is a guard to prevent further processing if we're not
            # processing a functionlike macro.
            if token.type == "OPERATOR":
                if token.value == "(":
                    paren_level += 1
                if token.value == ")":
                    paren_level -= 1
                    if paren_level < 0:
                        param = MacroParam(param_id, tuple(param_buffer))
                        macro_params.append(param)
                        end_pos = pos
                        break
            if token.type == "COMMA" and paren_level == 0:
                param = MacroParam(param_id, tuple(param_buffer))
                macro_params.append(param)
                param_buffer = []
                param_id += 1
                continue
            # All other tokens are placed into the buffer with the above
            # ensuring that the commas and the matching close paren is not
            # added to the token stream.
            param_buffer.append(token)
        else:
            # Loop ended without breaking. Means that a function signature
            # wasn't found, or the matching close paren wasn't found.
            if not macro_name:
                return None
            raise ValueError(f"Matching parenthesis not found in {tokenline.filename}: Ln {tokenline.lineno}, starting at {macro_name.position}")
        return MacroParamList(macro_name, tuple(macro_params), start_pos, end_pos)
        
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
