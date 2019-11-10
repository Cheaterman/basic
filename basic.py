import collections
import math

from sly.lex import Lexer, Token
from sly.yacc import Parser


Variable = collections.namedtuple('Variable', ['name'])
Expression = collections.namedtuple('Expression', ['operation', 'arguments'])
Statement = collections.namedtuple('Statement', ['operation', 'arguments'])


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
        ('nonassoc', IF, THEN),
        ('left', COLON),
        ('nonassoc', ELSE),
        ('left', EQUALS),
        ('left', CREATE_EXPRS, APPEND_EXPRS),
        ('left', PLUS, MINUS),
        ('left', MULTIPLY, DIVIDE),
        ('nonassoc', UNARY_MINUS),
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
        return Statement('add_program_line', (parsed.LINENO, parsed.LINE))

    @_('LINENO')
    def statement(self, parsed):
        return Statement('remove_program_line', [parsed.LINENO])

    @_('IF expr THEN statements')
    def statement(self, parsed):
        return Statement('conditional', (parsed.expr, parsed.statements))

    @_('IF expr THEN statements ELSE statement')
    def statement(self, parsed):
        return Statement(
            'conditional',
            (parsed.expr, parsed.statements, parsed.statement),
        )

    @_('variable EQUALS expr')
    def statement(self, parsed):
        return Statement('set_variable', (parsed.variable.name, parsed.expr))

    @_('PRINT exprs')
    def statement(self, parsed):
        return Statement('print', parsed.exprs)

    @_('LIST')
    def statement(self, parsed):
        return Statement('list', [])

    @_('expr %prec CREATE_EXPRS')
    def exprs(self, parsed):
        return [parsed.expr]

    @_('exprs expr %prec APPEND_EXPRS')
    def exprs(self, parsed):
        parsed.exprs.append(parsed.expr)
        return parsed.exprs

    @_('variable EQUALS expr')
    def expr(self, parsed):
        return Expression(
            'compare_variable',
            [parsed.variable.name, parsed.expr],
        )

    @_('MINUS expr %prec UNARY_MINUS')
    def expr(self, parsed):
        return Expression('negative', [parsed.expr])

    @_('expr PLUS expr')
    def expr(self, parsed):
        return Expression('add', [parsed.expr0, parsed.expr1])

    @_('expr MINUS expr')
    def expr(self, parsed):
        return Expression('subtract', [parsed.expr0, parsed.expr1])

    @_('expr MULTIPLY expr')
    def expr(self, parsed):
        return Expression('multiply', [parsed.expr0, parsed.expr1])

    @_('expr DIVIDE expr')
    def expr(self, parsed):
        return Expression('divide', [parsed.expr0, parsed.expr1])

    @_(
        'NUMBER',
        'STRING',
    )
    def expr(self, parsed):
        return parsed[0]

    @_('variable')
    def expr(self, parsed):
        return Expression('get_variable', [parsed.variable.name])

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
            statements = self.parser.parse(self.lexer.tokenize(line))
        except EOFError:
            if line.strip(' :').startswith('REM'):
                return
            raise SyntaxError('Unexpected EOF')

        for statement in statements:
            self.execute(*statement)

    def execute(self, instruction, arguments):
        return getattr(self, instruction)(*arguments)

    def evaluate(self, expression):
        evaluation_stack = collections.deque()
        argument_index_stack = collections.deque()
        node = expression
        last_visited_node = None

        while evaluation_stack or node is not None:
            if node is not None:
                evaluation_stack.append(node)

                if isinstance(node, Expression):
                    argument_index_stack.append(0)
                    node = node.arguments[0]
                else:
                    node = None

            else:
                next_node = evaluation_stack[-1]

                if(
                    isinstance(next_node, Expression)
                    and len(next_node.arguments) > 1
                    and last_visited_node != next_node.arguments[1]
                ):
                    argument_index_stack.append(1)
                    node = next_node.arguments[1]

                elif argument_index_stack:
                    last_visited_node = evaluation_stack.pop()
                    evaluation_stack[-1].arguments[
                        argument_index_stack.pop()
                    ] = self.visit(next_node)

                else:
                    return self.visit(next_node)

    def visit(self, node):
        if isinstance(node, float):
            int_node = int(node)
            return int_node if math.isclose(int_node, node) else node

        elif not isinstance(node, Expression):
            return node

        return self.execute(*node)

    def negative(self, a):
        return -a

    def add(self, a, b):
        return a + b

    def subtract(self, a, b):
        return a - b

    def multiply(self, a, b):
        return a * b

    def divide(self, a, b):
        return a / b

    def get_variable(self, name):
        return self.variables.get(name, 0)

    def set_variable(self, name, value):
        self.variables[name] = self.evaluate(value)

    def compare_variable(self, name, value):
        return -1 if self.variables[name] == value else 0

    def add_program_line(self, lineno, line):
        self.program[lineno] = line

    def remove_program_line(self, lineno):
        self.program.pop(lineno, None)

    def conditional(self, expr, then_statements, else_statement=None):
        if self.evaluate(expr):
            for statement in then_statements:
                self.execute(*statement)

        elif else_statement:
            self.execute(*else_statement)

    def list(self):
        for lineno, line in self.program.items():
            print(f'{lineno} {line}')

    def print(self, *args):
        print(*(self.evaluate(arg) for arg in args))
