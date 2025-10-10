# Macro assembler
#
#
#

import string, re, sys, random
from typing import NamedTuple, Iterator

class IncludeDepthExceeded(RecursionError):
    pass

class Token(NamedTuple):
    type: str
    value: str
    position: str

class TokenLine(NamedTuple):
    tokens: list[Token]
    filename: str
    lineno: int

'''
class TokenType(object):
    ERROR = 0
    NUMBER = 1
    LABEL = 2
    DIRECTIVE = 3
    MACRO = 4
    OPERATOR = 5

'''


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
        ("DEC_NUMBER",  r'\d+'),
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
        self.macros = dict()
        self.symtable = dict()
        self.result = bytearray()


    def openfile(self, filepath=None):
        if filepath is None:
            filepath = self.filepath
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
        


    def parse(self, filedata=None):
        if filedata is None:
            filedata = self.filedata
        lines = filedata.splitlines() # Split filedata into individual lines
        for i in range(2): # The original code had a loop for 2 passes, keeping it for now.
            for linenum,line in enumerate(lines): # Iterate over lines
                # Skip empty lines or lines that are just whitespace
                if not line.strip():
                    continue
                line = self.strip_comments(line)
                tokens = list(self.tokenize(line))
                print(f"Line number {linenum}: {line}")
                for token in tokens:
                    print(token)


    def unescape_string(self, raw: str) -> str:
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
    if asm.filedata:
        tokenlist = asm.preparse()
        for item in tokenlist:
            tokval = ' '.join(t.value for t in item.tokens)
            print(f"TokenLine: file:{item.filename}, linenum: {item.lineno}, line: [{tokval}]")
        #asm.parse()
