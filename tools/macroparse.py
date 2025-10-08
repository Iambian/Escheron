# Macro assembler
#
#
#

import string, re, sys
from typing import NamedTuple, Iterator

class Token(NamedTuple):
    type: str
    value: str
    position: str

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
        ("DIRECTIVE",   r'\.[A-Za-z_][A-Za-z0-9_]*'),
        # Preprocessor (e.g., #define)
        ("PREOP",       r'\#[A-Za-z_][A-Za-z0-9_]*'),
        # Identifiers
        ("IDENTIFIER",  r'[A-Za-z_][A-Za-z0-9_]*'),
        # Operators and special characters
        ("OPERATOR",    r'>>|<<|[+\-*/&|^~<>()=]'),
        ("COMMA",       r','),
        # Whitespace (to be skipped)
        ("SKIP",        r'[ \t\r\n]+'),
        # Any unknown character
        ("MISMATCH",    r'.'),
    ]

    TOK_REGEX = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in TOKEN_SPEC)
    get_token = re.compile(TOK_REGEX).match

    def __init__(self, filepath):
        self.symtable = dict()
        self.macros = dict()
        self.filedata = self.openfile(filepath)
        self.filepath = filepath

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

    def parse(self, filedata=None):
        if filedata is None:
            filedata = self.filedata
        lines = filedata.splitlines() # Split filedata into individual lines
        for i in range(2): # The original code had a loop for 2 passes, keeping it for now.
            for linenum,line in enumerate(lines): # Iterate over lines
                # Skip empty lines or lines that are just whitespace
                if not line.strip():
                    continue
                tokens = list(self.tokenize(line))
                print(f"Line number {linenum}: {line}")
                for token in tokens:
                    print(token)


    def unescape_string(self, raw: str) -> str:
        quote_char = raw[0]
        content = raw[1:-1]  # strip quotes
        return bytes(content, "utf-8").decode("unicode_escape")

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

    '''    
    def token_ishexdigit(self, string):
        for i in string:
            if i not in "0123456789abcdefABCDEF":
                return False
        return True
    
    '''

    #This has been obsoleted. For now. See: tokenize_old
    #def token_verify(self, token:str) -> TokenType:
    ''' This verifies if a number/label token is valid. This does not
        check for the validity of operator tokens - that is the tokenizer's
        job. For a number/label to be valid, it must adhere to
        the following rules, which are tested in this order:
        Numbers: All numeric are numbers. Prefix % or $ are binary/hex.
            Suffix b/B H/h are binary/hex. Prefix and suffix cannot coexist.
        Labels: Starts with nonnumeric word character. Rest are word chars.
        Directives: Starts with a '.', letters following are valid label.
        Preops: Starts with a '#', letters following are valid label.
    '''
    '''        
        #Check number
        if token[0] in "$%":
            # If a token starts with $ or %, it MUST be a number token.
            # If it is not, then an error should occur.
            if token[-1] in "HhBb":
                raise ValueError("Number token may not have both prefix and suffix.")
            if token[0] == "$" and self.token_ishexdigit(token[1:]):
                return TokenType.NUMBER
            if token[0] == "%" and all([i in "01" for i in token[1:]]):
                return TokenType.NUMBER
            raise ValueError("Number-prefix indicates number token but contains invalid characters.")
        if token[-1] in "HhBb":
            # If the token ends in a suffix, it *could* be a number, but fall
            # through if it isn't. It could be a label.
            if self.token_ishexdigit(token[:-1]):
                return TokenType.NUMBER
        if token.isdecimal():
            return TokenType.NUMBER
        #
        
    '''            

    # This routine has been decommissioned. I asked ChatGPT about this scenario
    # and it gave me a bunch of regexes and a routine to use. Let's try that
    # first but keep this here just in case that doesn't work out.
    def tokenize_old(self, linedata):
        ''' Tokens are strings denoting the minimal parsing unit. Some of these
            may be surprising since we are doing a bespoke operation. Each line
            is iterated over one character at a time so the current character,
            previous character, current token (colletion of characters), and
            the string/escape flags are known at the time.

            The following punctuation is always considered their own token:
            ~-+=*/)^
            The open parenthesis is NOT on that list. This is because labels
            that are immediately adjacent to an open parenthesis MUST be placed
            as part of the label to mark it as a function-like macro. This is
            a special case and provisions should be in place to check that
            appending the open paren to the token is done ONLY if it is a
            valid (i.e. not a number) macro name.

            The following punctuation are considered word characters for labels:
            #.
            These are for preops and directives, respectively. They must appear
            at the start of a label.

            The % and $ characters are used for binary and hexadecimal number
            prefixes. $ also has its own meaning if not part of a number so if
            that's the case, it must then be its own token.

            >> and << are operators. You'll also see > and < in preop
            conditionals so be able to correctly tokenize > vs >> and such.
        
        '''
        cls = self.__class__
        token = ""
        tokens = list()
        prev = ''
        isstring = False    # Is either false, or holds opening quote character.
        isescape = False    # For parsing backslashes inside strings
        char:str
        for char in linedata:
            if token:
                # If characters are buffered into token, see if we need to
                # flush it or keep adding characters to it.
                if isstring:
                    if char in "\t ":
                        if prev == "\\":
                            token += "\\"+char
                            prev = char
                            continue
                        token += char
                        prev = char
                        continue
                    if char == "\\":
                        if prev == char:
                            token += "\\"
                            continue
                        prev = char
                        continue
                    if char == isstring:
                        if prev == "\\":
                            token += prev+isstring
                        else:
                            tokens.append(token)
                            token = ""
                            prev = char
                            isstring = False
                else:
                    # Not parsing inside a string.
                    if char in string.whitespace:
                        # If now is a whitespace character, flush token.
                        tokens.append(token)
                        token = ""
                        prev = char
                        continue
                    if char in "~-+=*/)^><%$.#":
                        # The character is an unambiguous operator or a special 
                        # operator that is known to not be allowed anywhere but 
                        # at the start of a token. Flush token.
                        tokens.append(token)
                        token = ""
                        prev = char
                        continue
                    if char in cls.LBLCHARS:
                        # The next letter in a number or a label.
                        token += char
                        prev = char
                        continue
                    if char == '(':
                        # Special case - If prevchar is a word character, this
                        # needs to be attached to it and then the token closed
                        # off. Else, flush the token THEN flush this char as
                        # A separate token.
                        if prev in cls.LBLCHARS:
                            token += char
                            tokens.append(token)
                            token = ""
                            prev = char
                        else:
                            tokens.append(token)
                            tokens.append(char)
                            token = ""
                            prev = char
                        continue
                    if char in "\"'":
                        raise ValueError("Quote characters must start on their own token.")
            else:
                # Token is empty. 
                if char in string.whitespace and not tokens:
                    # Special case - Some lines MUST start with a whitespace
                    # We are recording whether or not this is true by storing
                    # a space at the start if true.
                    tokens.append(' ')
                    prev = char
                    continue
                if char in string.whitespace:
                    if prev in "><":
                        tokens.append(prev)
                        prev = char
                if char in "/*-+~^&|":
                    # Unconditional punctuation. Emit as own token.
                    # Token stays empty.
                    if prev in "><":
                        raise ValueError("Illegal use of brackets ")
                    tokens.append(char)
                    prev = char
                if char in "><":
                    # Possible binary expression
                    if prev == char:
                        tokens.append(prev+char)
                    prev = char
                token = char






        pass


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
        asm.parse()
