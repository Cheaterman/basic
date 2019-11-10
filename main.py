from basic import BasicInterpreter


if __name__ == '__main__':
    print("Cman's simple BASIC v0.1")
    print('2019 Cheaterman')

    try:
        import psutil
        print(f'\n{psutil.virtual_memory().available / 2**30:.3f} GB free')

    except ImportError:
        pass

    interpreter = BasicInterpreter()

    while True:
        line = ''

        try:
            print('\nReady')

            while not line:
                line = input()

        except KeyboardInterrupt:
            print()
            break

        except EOFError:
            break

        try:
            interpreter.interpret(line)

        except SyntaxError as exception:
            print(type(exception).__name__ + ':', exception)

    print('Bye!')
