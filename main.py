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
        try:
            print('\nReady')
            line = input()

        except KeyboardInterrupt:
            print()
            break

        except EOFError:
            break

        interpreter.interpret(line)

    print('Bye!')
