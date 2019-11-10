import pytest


@pytest.fixture
def lexer():
    from basic import BasicLexer
    return BasicLexer()


@pytest.mark.parametrize(
    'test_string, expected_tokens',
    (
        ('A = 2', (
            ('ID', 'A'),
            ('EQUALS', '='),
            ('NUMBER', 2),
        )),
        ('A = A + 1', (
            ('ID', 'A'),
            ('EQUALS', '='),
            ('ID', 'A'),
            ('PLUS', '+'),
            ('NUMBER', 1),
        )),
        ('PRINT A', (
            ('PRINT', 'PRINT'),
            ('ID', 'A'),
        )),
        ('10 A = 5', (
            ('LINENO', 10),
            ('LINE', 'A = 5'),
        )),
        (' 10 A = 5', (
            ('LINENO', 10),
            ('LINE', 'A = 5'),
        )),
        ('10.2 A = 5', (
            ('LINENO', 10),
            ('LINE', '.2 A = 5'),
        )),
        ('10', (('LINENO', 10),)),
        ('  10', (('LINENO', 10),)),
        ('10  ', (('LINENO', 10),)),
        ('10     A = 5', (
            ('LINENO', 10),
            ('LINE', 'A = 5'),
        )),
        ('LIST', (('LIST', 'LIST'),)),
        ('PRINT 1 2 3', (
            ('PRINT', 'PRINT'),
            ('NUMBER', 1),
            ('NUMBER', 2),
            ('NUMBER', 3),
        )),
        ('PRINT A = 3', (
            ('PRINT', 'PRINT'),
            ('ID', 'A'),
            ('EQUALS', '='),
            ('NUMBER', 3),
        )),
        ('PRINTA=3', (
            ('PRINT', 'PRINT'),
            ('ID', 'A'),
            ('EQUALS', '='),
            ('NUMBER', 3),
        )),
        ('PRINT"A"', (
            ('PRINT', 'PRINT'),
            ('STRING', 'A'),
        )),
        ('PRINT"A"A', (
            ('PRINT', 'PRINT'),
            ('STRING', 'A'),
            ('ID', 'A'),
        )),
        ('PRINT"A"REM', (
            ('PRINT', 'PRINT'),
            ('STRING', 'A'),
            # This is supposed to be caught by parser as syntax error
            ('ID', 'REM'),
        )),
        ('PRINT"A":REMTHISISIGNORED', (
            ('PRINT', 'PRINT'),
            ('STRING', 'A'),
        )),
        ('PRINT "A" : REMTHISISIGNORED', (
            ('PRINT', 'PRINT'),
            ('STRING', 'A'),
        )),
        ('REM THIS IS IGNORED', []),
        ('  REM THIS IS IGNORED', []),
        ('IF A THEN PRINT "B" ELSE PRINT "C"', (
            ('IF', 'IF'),
            ('ID', 'A'),
            ('THEN', 'THEN'),
            ('PRINT', 'PRINT'),
            ('STRING', 'B'),
            ('ELSE', 'ELSE'),
            ('PRINT', 'PRINT'),
            ('STRING', 'C'),
        )),
        ('IF 0 THEN PRINT "B" ELSE PRINT "C"', (
            ('IF', 'IF'),
            ('NUMBER', 0),
            ('THEN', 'THEN'),
            ('PRINT', 'PRINT'),
            ('STRING', 'B'),
            ('ELSE', 'ELSE'),
            ('PRINT', 'PRINT'),
            ('STRING', 'C'),
        )),
        ('PRINT 3 * 4 + 5', (
            ('PRINT', 'PRINT'),
            ('NUMBER', 3),
            ('MULTIPLY', '*'),
            ('NUMBER', 4),
            ('PLUS', '+'),
            ('NUMBER', 5),
        )),
        ('PRINT 3 + 5 * -2', (
            ('PRINT', 'PRINT'),
            ('NUMBER', 3),
            ('PLUS', '+'),
            ('NUMBER', 5),
            ('MULTIPLY', '*'),
            ('MINUS', '-'),
            ('NUMBER', 2),
        )),
        ('PRINTC', (
            ('PRINT', 'PRINT'),
            ('ID', 'C'),
        )),
        ('C=3', (
            ('ID', 'C'),
            ('EQUALS', '='),
            ('NUMBER', 3),
        )),
        ('PRINTC', (
            ('PRINT', 'PRINT'),
            ('ID', 'C'),
        )),
        ('PRINT "A" : 10 PRINT "B"', (
            ('PRINT', 'PRINT'),
            ('STRING', 'A'),
            ('COLON', ':'),
            ('NUMBER', 10),
            ('PRINT', 'PRINT'),
            ('STRING', 'B'),
        )),
        ('B = A = 3', (
            ('ID', 'B'),
            ('EQUALS', '='),
            ('ID', 'A'),
            ('EQUALS', '='),
            ('NUMBER', 3),
        )),
        ('PRINT B', (
            ('PRINT', 'PRINT'),
            ('ID', 'B'),
        )),
    ),
)
def test_lexer(lexer, test_string, expected_tokens):
    tokens = list(lexer.tokenize(test_string))

    assert len(tokens) == len(expected_tokens)

    for token, (expected_type, expected_value) in zip(tokens, expected_tokens):
        assert token.type == expected_type
        assert token.value == expected_value


@pytest.fixture
def interpreter(request):
    from basic import BasicInterpreter

    interpreter = BasicInterpreter()

    if getattr(request, 'param', None):
        lines = request.param

        if isinstance(lines, str):
            lines = [lines]

        for line in lines:
            interpreter.interpret(line)

    return interpreter


@pytest.mark.parametrize('interpreter', indirect=True, argvalues=['A = 2'])
def test_interpreter_assignment(interpreter):
    assert interpreter.variables['A'] == 2


@pytest.mark.parametrize('interpreter', indirect=True, argvalues=[(
    'A = 2',
    'A = A + 1',
)])
def test_interpreter_simple_arithmetic(interpreter):
    assert interpreter.variables['A'] == 3


@pytest.mark.parametrize('interpreter', indirect=True, argvalues=[(
    'A = 3',
    'PRINT A',
)])
# capsys goes first, otherwise parametrized interpreter output isn't captured
def test_interpreter_print_variable(capsys, interpreter):
    captured = capsys.readouterr()
    assert captured.out == '3\n'


@pytest.mark.parametrize(
    'interpreter, line_number, expected_program_line',
    indirect=['interpreter'],
    argvalues=(
        ('10 A = 5', 10, 'A = 5'),
        (' 10 A = 5', 10, 'A = 5'),
        ('10     A = 5', 10, 'A = 5'),
        ('10.2 A = 5', 10, '.2 A = 5'),
        ('20 A = 5', 20, 'A = 5'),
    )
)
def test_interpreter_add_program_line(
    interpreter,
    line_number,
    expected_program_line,
):
    assert interpreter.program[line_number] == expected_program_line


@pytest.mark.parametrize(
    'interpreter, line_number',
    indirect=['interpreter'],
    argvalues=(
        (('10 A = 5', '10'), 10),
        (('10 A = 5', '  10'), 10),
        (('10 A = 5', '10  '), 10),
    )
)
def test_interpreter_remove_program_line(interpreter, line_number):
    assert interpreter.program.get(line_number) is None


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=(
        (('10 A = 5', 'LIST'), '10 A = 5'),
        ((' 10 A = 5', 'LIST'), '10 A = 5'),
        (('10     A = 5', 'LIST'), '10 A = 5'),
        (('10.2 A = 5', 'LIST'), '10 .2 A = 5'),
        (('10 A = 1', '20 A = A + 2', 'LIST'), '10 A = 1\n20 A = A + 2'),
    )
)
def test_interpreter_list(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + '\n'


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=(
        ('PRINT 1 2 3', '1 2 3'),
        (('A = 3', 'PRINT A = 3'), '-1'),
        (('A = 3', 'PRINTA=3'), '-1'),
        ('PRINT"A"', 'A'),
        (('A = 3', 'PRINT"A"A'), 'A 3'),
    )
)
def test_print_output(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + '\n'


def test_rem_syntax_error(interpreter):
    with pytest.raises(SyntaxError):
        interpreter.interpret('PRINT"A"REM')


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=(
        ('PRINT"A":REMTHISISIGNORED', 'A'),
        ('PRINT "A" : REMTHISISIGNORED', 'A'),
        ('REM THIS IS IGNORED', ''),
        ('  REM THIS IS IGNORED', ''),
    )
)
def test_rem_ignored(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + ('\n' if expected_output else '')


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=(
        (('A = 3', 'IF A THEN PRINT "B" ELSE PRINT "C"'), 'B'),
        ('IF 0 THEN PRINT "B" ELSE PRINT "C"', 'C'),
        ('IF 0 THEN PRINT "B"', ''),
        ('IF 1 THEN PRINT "B"', 'B'),
    )
)
def test_conditionals(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + ('\n' if expected_output else '')


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=(
        ('PRINT 3 + 5 * -2', '-7'),
        ('PRINT 3 - 4 * 5', '-17'),
        # FIXME: Should round automagically for output
        ('PRINT 3 - 10 / 5', '1.0'),
    )
)
def test_extra_arithmetic(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + '\n'


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=(
        (('PRINTC', 'C=3', 'PRINTC'), '0\n3'),
    )
)
def test_default_variable_value(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + '\n'


def test_program_line_mid_statement_invalid(interpreter):
    with pytest.raises(SyntaxError):
        interpreter.interpret('PRINT "A" : 10 PRINT "B"')


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=(
        (('A = 3', 'B = A = 3', 'PRINT B'), '-1'),
        (('A = 2', 'B = A = 3', 'PRINT B'), '0'),
    )
)
def test_boolean_values(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + '\n'


@pytest.mark.parametrize(
    'interpreter, expected_output',
    indirect=['interpreter'],
    argvalues=[('PRINT "A" : PRINT "B"', 'A\nB')],
)
def test_multiple_statements(capsys, interpreter, expected_output):
    captured = capsys.readouterr()
    assert captured.out == expected_output + '\n'


@pytest.mark.parametrize('interpreter', indirect=True, argvalues=[''])
def test_empty_statement(capsys, interpreter):
    captured = capsys.readouterr()
    assert captured.out == ''
