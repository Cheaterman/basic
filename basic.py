import collections
import math

from sly.lex import Lexer, Token
from sly.yacc import Parser


Variable = collections.namedtuple('Variable', ['name'])


class BasicLexer(Lexer):
    tokens = {
        ID,
        PRINT,
        IF,
        THEN,
        ELSE,
        STRING,
        LINENO,
        LIST,
        NUMBER,
        PLUS,
        MINUS,
        MULTIPLY,
        DIVIDE,
        EQUALS,
        COLON,
    }

    ignore = ' '
    ignore_comments = '(?:: *)?REM.*'

    def ignore_comments(self, token):
        if(
            token.index
            and self.text[:token.index] != token.index * ' '
            and not token.value.startswith(':')
        ):
            # These will be rejected by the parser
            token.type = 'ID'
            return token

    PLUS = r'\+'
    MINUS = r'-'
    MULTIPLY = r'\*'
    DIVIDE = r'/'
    EQUALS = r'='

    PRINT = r'PRINT'
    IF = r'IF'
    THEN = r'THEN'
    ELSE = r'ELSE'
    LIST = r'LIST *(?::.*)?'
    COLON = r':'

    ID = r'[A-Za-z_][A-Za-z0-9_]*'

    @_(r'(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)')
    def NUMBER(self, token):
        if(
            self.index
            and self.text[:token.index] != token.index * ' '
        ):
            float_value = float(token.value)
            int_value = int(float_value)
            token.value = (
                int_value
                if math.isclose(int_value, float_value)
                else float_value
            )

        else:
            if '.' not in token.value:
                token.value = int(token.value)

            else:
                dot_index = token.value.index('.')
                self.index -= len(token.value) - dot_index
                token.value = int(token.value[:dot_index])

            token.type = 'LINENO'

            if self.text[self.index:].strip(' '):
                self.begin(LineLexer)

        return token

    @_(r'"[^"]*"?')
    def STRING(self, token):
        token.value = token.value[1:]

        if token.value.endswith('"'):
            token.value = token.value[:-1]

        return token


class LineLexer(Lexer):
    tokens = {LINE}
    ignore = ' '

    @_(r'.+')
    def LINE(self, token):
        self.begin(BasicLexer)
        return token


class BasicParser(Parser):
    tokens = BasicLexer.tokens.union(LineLexer.tokens)
    precedence = (
        ('left', COLON),
        ('left', IF, THEN),
        ('left', ELSE),
        ('left', EQUALS),
        ('left', PLUS, MINUS),
        ('left', MULTIPLY, DIVIDE),
        ('right', UNARY_MINUS),
    )

    def __init__(self, interpreter):
        self.interpreter = interpreter

    @_('statement')
    def statements(self, parsed):
        if parsed.statement:
            return [parsed.statement]

    @_('statements COLON statement')
    def statements(self, parsed):
        parsed.statements.append(parsed.statement)
        return parsed.statements

    @_('LINENO LINE')
    def statement(self, parsed):
        return ('add_program_line', parsed.LINENO, parsed.LINE)

    @_('LINENO')
    def statement(self, parsed):
        return ('remove_program_line', parsed.LINENO)

    @_(
        'IF expr THEN statement',
        'IF expr THEN statement ELSE statement',
    )
    def statement(self, parsed):
        if parsed.expr:
            return parsed[3]

        elif len(parsed) > 5:
            return parsed.statement1

    @_('variable EQUALS expr')
    def statement(self, parsed):
        return ('set_variable', parsed.variable.name, parsed.expr)

    @_('PRINT exprs')
    def statement(self, parsed):
        return ('print', *parsed.exprs)

    @_('LIST')
    def statement(self, parsed):
        return ('list',)

    @_('expr')
    def exprs(self, parsed):
        return [parsed.expr]

    @_('exprs expr')
    def exprs(self, parsed):
        parsed.exprs.append(parsed.expr)
        return parsed.exprs

    @_('variable EQUALS expr')
    def expr(self, parsed):
        return self.interpreter.compare_variable(
            parsed.variable.name,
            parsed.expr,
        )

    @_('MINUS expr %prec UNARY_MINUS')
    def expr(self, parsed):
        return -parsed.expr

    @_('expr PLUS expr')
    def expr(self, parsed):
        return parsed.expr0 + parsed.expr1

    @_('expr MINUS expr')
    def expr(self, parsed):
        return parsed.expr0 - parsed.expr1

    @_('expr MULTIPLY expr')
    def expr(self, parsed):
        return parsed.expr0 * parsed.expr1

    @_('expr DIVIDE expr')
    def expr(self, parsed):
        return parsed.expr0 / parsed.expr1

    @_(
        'NUMBER',
        'STRING',
    )
    def expr(self, parsed):
        return parsed[0]

    @_('variable')
    def expr(self, parsed):
        return self.interpreter.variables[parsed.variable.name]

    @_('ID')
    def variable(self, parsed):
        if parsed.ID.startswith('REM'):
            raise SyntaxError('REM is a reserved keyword')

        return Variable(parsed.ID)

    def error(self, token):
        if not token:
            raise EOFError('Parse error in input, unexpected EOF')

        raise SyntaxError(
            f'Syntax error at line {token.lineno}, token={token.type}'
        )


class BasicInterpreter:
    def __init__(self):
        self.lexer = BasicLexer()
        self.parser = BasicParser(self)
        self.variables = collections.defaultdict(int)
        self.program = {}

    def interpret(self, line):
        try:
            instructions = self.parser.parse(self.lexer.tokenize(line))
        except EOFError:
            return

        for instruction, *args in instructions:
            getattr(self, instruction)(*args)

    def add_program_line(self, lineno, line):
        self.program[lineno] = line

    def remove_program_line(self, lineno):
        self.program.pop(lineno, None)

    def list(self):
        for lineno, line in self.program.items():
            print(f'{lineno} {line}')

    def set_variable(self, name, value):
        self.variables[name] = value

    def compare_variable(self, name, value):
        return -1 if self.variables[name] == value else 0

    def print(self, *args):
        print(*args)


if __name__ == '__main__':
    # TODO: REPL
    pass
