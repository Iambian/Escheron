# Second revision of the parser/assembler simulating SPASM-ng. Mostly.
#
#

import os, sys, random, copy, re, colorama
from typing import NamedTuple, Iterator, Optional, Tuple
from collections import deque

colorama.just_fix_windows_console()

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

def from_token(token:Token, toktype:str, value:int|str):
    return Token(toktype, value, token.col, token.row, token.file)

class MacroDef(NamedTuple):
    name: Token             # The token used to identify this macro
    params: tuple[Token]    # Only one token per param, each an IDENT type.
    body: "TokenStream"     # Yeah. This tracks.

def redmsg(msg):
    from colorama import Fore, Style
    return f"{Fore.RED}{msg}{Style.RESET_ALL}"
def yellowmsg(msg):
    from colorama import Fore, Style
    return f"{Fore.YELLOW}{msg}{Style.RESET_ALL}"
def errmsg(token:Token, msg:str):
    return redmsg(f"[{token.file}: LN {token.row}, COL {token.col}] {msg}")
def err(token:Token, msg:str):
    raise ValueError(errmsg(token, msg))
def printerr(token:Token, msg:str):
    print(errmsg(token, msg))


class Parser(object):
    MATH_OPS = ('+','-','*','/','&','|','^','<<','>>','==','!=','<','>','<=','>=','&&','||')
    def __init__(self, tokendata:"TokenStream"):
        self.tokens = tokendata
        self.symtable:dict[str, int|str]
        self.origin = 0
        self.parse(self.tokens, 1)
        self.results:bytearray = self.parse(self.tokens, 2)
        # Notes on self.if_stack:
        # True = is taking this branch. Always becomes None elsewise.
        # False = is not taking this branch. Is looking for condition change.
        # None = A branch has already been taken. Do not consider any others.
        self.if_stack:list[bool] = []
        pass

    def parse(self, tokens:"TokenStream", passid=1, depth=1, trace=False) -> "TokenStream|bytearray":
        # Only returns bytearray if passid=2 and depth=1.
        iterator = tokens.getline()
        for line in iterator:
            if len(line) < 1:
                continue
            token0 = line[0]
            token0v = token0.v.upper()
            # Must handle condition-changing preops first.
            if token0.type == "PREOP" and token0v == "#ENDIF":
                if len(self.if_stack) < 1:
                    err(token0, "#ENDIF used without corresponding #IF/#IFDEF/#IFNDEF.")
                self.if_stack.pop()
                continue
            if token0.type == "PREOP" and token0v == "#ELSE":
                if len(self.if_stack) < 1:
                    err(token0, "#ELSE used without corresponding #IF/#IFDEF/#IFNDEF.")
                b = self.if_stack[-1]
                if b is False:
                    self.if_stack[-1] = True
                else:
                    self.if_stack[-1] = None
                continue
            if token0.type == "PREOP" and token0v == "#ELIF":
                if len(self.if_stack) < 1:
                    err(token0, "#ELIF used without corresponding #IF/#IFDEF/#IFNDEF.")
                self.if_stack[-1] = not self.if_stack[-1]
                if len(line) < 2:
                    err(token0, f"Missing parameters for preop {token0.v}")
                ifexpr = self.parse(line[1:], 2, depth+1, trace)
                ifresult = True if self.eval_expr(ifexpr, 2) != 0 else False
                b = self.if_stack[-1]
                if b is False:
                    if ifresult is True:
                        self.if_stack[-1] = True
                else:
                    self.if_stack[-1] = None
            # Now that we did stuff that could change if_stack level or state...
            if len(self.if_stack) > 0 and not self.if_stack[-1]:
                # Consume lines if top of stack is False or None
                continue
            # This token line is reachable. Continue processing.
            if token0.type == "PREOP":
                if token0v in ("#IF", "#IFDEF", "#IFNDEF", "#DEFINE", "#MACRO", "#UNDEF"):
                    if len(line) < 2:
                        err(token0, f"Missing parameters for preop {token0.v}")
                    token1 = line[1]
                    if token0v in ("#IFDEF", "#IFNDEF"):
                        ifresult = False
                        if token0v in self.symtable:
                            ifresult = True
                        elif token0v+'(' in self.symtable:  # Function macrodef
                            ifresult = True
                        if token0v == "#IFNDEF":
                            ifresult = not ifresult
                        self.if_stack.append(ifresult)
                        continue
                    if token0v == "#IF":
                        ifexpr = self.parse(line[1:], 2, depth+1, trace)
                        ifresult = True if self.eval_expr(ifexpr, 2) != 0 else False
                        self.if_stack.append(ifresult)
                        continue
                    if token0v in ("#MACRO", "#DEFINE"):
                        if line[1].type not in ("IDENT", "MACRO", "DIRECTIVE", "DIR_CALL"):
                            err(token1, f"Invalid identifier assigned to {token0v}")
                    if token0v in ("#UNDEF", "#UNDEFINE"):
                        if token0v in self.symtable:
                            del self.symtable[token0v]
                            continue
                        withparen = token0v + '('
                        if withparen in self.symtable:
                            del self.symtable[withparen]
                            continue
                    if token0v == "#DEFINE":
                        if token1.type in ("IDENT", "DIRECTIVE"):
                            # No params. Simple macro. Could be a symtable entry
                            macname = token1.v
                            macbody = line[2:]
                            try:
                                macbody = self.parse(macbody, 2, depth+1, trace)
                                macbody = self.eval_expr(macbody, passid)
                            except:
                                # Suppress errors. Possible values of macbody:
                                # 1. Empty (no body) [instant error]
                                # 2. Tokenlist / Macro expanded [eval error]
                                # 3. int (no errors)
                                pass
                            if isinstance(macbody, int):
                                macdef = macbody
                            else:
                                macdef = MacroDef(token1, tuple(), macbody)
                            self.symtable[macname] = macdef
                        elif token1.type in ("MACRO", "DIR_CALL"):
                            if token1.v in ("eval(","concat("):
                                err(token1, f"Illegal redefinition of reserved macro name {token1.v}")
                            paramlist = self.get_paramlist(line, 1)
                            if paramlist:
                                if any([(len(param) > 1 or param[0].type != "IDENT") for param in paramlist]):
                                    err(token1, "Illegal identifier(s) found in macro signature.")
                                macrolen = sum([len(param) for param in paramlist])
                                macrolen += len(paramlist) #n-1 commas plus end paren = n
                                paramlist = [param[0] for param in paramlist] # Flatten list with 1st token ea.
                            else:
                                macrolen = 1    #empty params. No args.
                            macrobody = line[2+macrolen:]
                            macrodef = MacroDef(token1, tuple(paramlist), macrobody)
                            self.symtable[token1.v] = macrodef
                        else:
                            err(token1, "Illegal macro identifier")
                    if token0v == "#MACRO":
                        # We'll have to iterate through until we find an
                        # #encmacro
                        pass












        pass

    def get_paramlist(self,tokenline:list[Token], start:int) -> list[list[Token]]:
        if len(tokenline) < 1:
            raise ValueError(redmsg("You may not pass an empty line into find_closeparen()"))
        if start >= len(tokenline):
            err(tokenline[-1], f"Start value {start} is out of bounds.")
        paramlist:list[list[Token]] = []
        paramtokens:list[Token] = []
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
        return paramlist
        
            

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
        tokval = token.v.upper()
        if "NUM" in toktype:
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
                return self.symtable[token.value]
        elif token.type == "OPERATOR" and token.value == "$":
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
            if token.type == "OPERATOR" and token.v in ('+', '-', '~') and (baseval is None or operator is not None):
                if unary is not None: # Cannot have two unary operators in a row
                    err(token, "Invalid unary operator sequence")
                unary = token
                tokseg_idx += 1
                continue

            current_value:int = None
            if "NUM" in token.type or token.type == "IDENTIFIER" or (token.type == "OPERATOR" and token.value == '$'):
                current_value = self.eval_val(token, passnum)
                tokseg_idx += 1
            elif token.type == "OPERATOR" and token.v == '(':
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
                    raise ValueError(f"Closing parenthesis not found at around {tokenline.filename}:{tokenline.lineno} col {tokens[start_paren_pos].position}")

                if sub_expr_start_idx > sub_expr_end_idx:
                    current_value = 0 # Empty parenthesis sequence
                else:
                    sub_tokline = tokens[sub_expr_start_idx:sub_expr_end_idx + 1] # +1 for exclusive end
                    current_value_token = self.eval_expr(sub_tokline, (0, len(sub_tokline.tokens) - 1), passnum) # Recursive call
                    current_value = int(current_value_token.v) # Convert result token value to int
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
                if next_token.type == "OPERATOR" and next_token.v in self.MATH_OPS:
                    if operator is not None: # Cannot have two binary operators in a row
                        err(next_token, "Invalid operator sequence encountered.")
                    operator = next_token
                    tokseg_idx += 1
                elif next_token.type == "OPERATOR" and next_token.value in ('(', ')'):
                    # Parentheses are handled by the main loop, not as binary operators here
                    # This case should ideally not be reached if parsing is correct,
                    # as '(' would start a new value and ')' would end a sub-expression.
                    # If it's reached, it implies a structural issue or an unexpected token.
                    err(next_token, "Unexpected parenthesis encountered.")
                elif next_token.type == "IDENTIFIER" or "NUM" in next_token.type:
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
        tokens = []
        tokens_index = 0
        inside_define = False
        while tokens_index < len(self.tokens):
            #If there are any resubmitted tokens, consume them first.
            if self.resub_tokens:
                token = self.resub_tokens.popleft()
                pass
            else:
                token = self.tokens[tokens_index]
                tokens_index += 1
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
            tokenline.append(Token("NEWLINE", "\n", -1, lineno, filename))
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
    ts = TokenStream.from_filename("tools/macrotest.z80")
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