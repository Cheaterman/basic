import collections

from sly.lex import Lexer, Token
from sly.yacc import Parser, YaccError


Instruction = collections.namedtuple('Instruction', ('lineno', 'statements'))


class BasicLexer(Lexer):
    tokens = {
        ID,
        PRINT,
        IF,
        THEN,
        ELSE,
        STRING,
        LINENO,
        NUMBER,
        PLUS,
        MINUS,
        MULTIPLY,
        DIVIDE,
    }

    literals = {'='}

    ignore = ' '
    ignore_comments = '(?:^|:)REM.*'

    PRINT = r'PRINT'
    IF = r'IF'
    THEN = r'THEN'
    ELSE = r'ELSE'

    PLUS = r'\+'
    MINUS = r'-'
    MULTIPLY = r'\*'
    DIVIDE = r'\\'

    ID = r'[A-Za-z_][A-Za-z0-9_]*'

    @_(r'(:?[0-9]+(?:\.[0-9]*)?|\.[0-9]+)')
    def NUMBER(self, token):
        if(
            self.index
            and self.text[0:token.index] != token.index * ' '
        ):
            token.value = float(token.value)

        else:
            if '.' not in token.value:
                token.value = int(token.value)

            else:
                dot_index = token.value.index('.')
                self.index -= len(token.value) - dot_index
                token.value = int(token.value[:dot_index])

            token.type = 'LINENO'

            # Handle interpreter.add_program_line here (using text),
            # stop processing the line, and return None?
            # That's just if the line has data - pass LINENO to the parser
            # to let it handle line deletion
            # OTOH... add_program_line could be handled there too?
            # !!! use a new lexer state to handle rest of line when LINENO is encountered?

        return token

    @_(r'"[^"]*"?')
    def STRING(self, token):
        token.value = token.value[1:]

        if token.value.endswith('"'):
            token.value = token.value[:-1]

        return token


class BasicParser(Parser):
    tokens = BasicLexer.tokens
    precedence = (
        ('left', PLUS, MINUS),
        ('left', MULTIPLY, DIVIDE),
        ('right', UNARY_MINUS),
    )

    @_('statement')
    def statements(self, parsed):
        return [parsed.statement]

    @_('statements ":" statement')
    def statements(self, parsed):
        parsed.statements.append(parsed.statement)
        return parsed.statements

    @_('LINENO')
    def statement(self, parsed):
        return ('del_program_line', parsed.LINENO)

    # Nope?
    @_('LINENO statements')
    def statement(self, parsed):
        return ('add_program_line', parsed.LINENO, parsed.statements)

    @_(
        'IF expr THEN statement',
        'IF expr THEN statement ELSE statement',
    )
    def statement(self, parsed):
        statement = ['condition', parsed[1],  parsed[3]]

        if len(parsed) >= 6:
            statement.append(parsed[5])

        return tuple(statement)

    @_('variable "=" expr')
    def statement(self, parsed):
        return ('assign', parsed.variable, parsed[2])

    @_('PRINT exprs')
    def statement(self, parsed):
        return ('print', parsed.exprs)

    @_('exprs expr')
    def exprs(self, parsed):
        parsed.exprs.append(parsed.expr)
        return parsed.exprs

    @_('expr')
    def exprs(self, parsed):
        return [parsed.expr]

    @_('variable "=" expr')
    def expr(self, parsed):
        return ('compare', parsed.variable, parsed[2])

    @_('MINUS expr %prec UNARY_MINUS')
    def expr(self, parsed):
        return ('negate', parsed.expr)

    @_('expr PLUS expr')
    def expr(self, parsed):
        return ('add', parsed[0], parsed[2])

    @_('expr MINUS expr')
    def expr(self, parsed):
        return ('substract', parsed[0], parsed[2])

    @_('expr MULTIPLY expr')
    def expr(self, parsed):
        return ('multiply', parsed[0], parsed[2])

    @_('expr DIVIDE expr')
    def expr(self, parsed):
        return ('divide', parsed[0], parsed[2])

    @_(
        'NUMBER',
        'STRING',
    )
    def expr(self, parsed):
        return parsed[0]

    @_('variable')
    def expr(self, parsed):
        return parsed.variable

    @_('ID')
    def variable(self, parsed):
        if parsed.ID == 'REM':
            raise SyntaxError('REM is a reserved keyword')

        return ('variable', parsed.ID)

    def error(self, token):
        raise YaccError(
            f'Syntax error at line {token.lineno}, token={token.type}'
            if token else 'Parse error in input, unexpected EOF'
        )


if __name__ == '__main__':
    test_lines = (
        'A = 2',
        'A = A + 1',
        'PRINT A',
        '10 A = 5',
        ' 10 A = 5',
        '10.2 A = 5',
        'PRINT A = 3',
        'PRINTA=3',
        'PRINT"A"',
        'PRINT"A"A',
        'PRINT"A"REM',
        'PRINT"A":REMTHISISIGNORED',
        'REM THIS IS IGNORED',
        'IF A THEN PRINT "B" ELSE PRINT "C"',
        'PRINT 3 * 4 + 5',
        'PRINT 3 + 5 * -2',
    )

    lexer = BasicLexer()
    parser = BasicParser()

    for line in test_lines:
        print('Tokenizing:', line)

        for token in lexer.tokenize(line):
            print(token)

        print()


    for line in test_lines:
        print('Parsing:', line)

        try:
            print(parser.parse(lexer.tokenize(line)))

        except (YaccError, SyntaxError) as exception:
            print('ERROR:', exception)

        print()
