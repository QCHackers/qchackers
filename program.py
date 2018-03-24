import vm

class Program:
    "Consists of a list of instructions"
    def __init__(self, instructions):
        vm.evaluate(instructions, "string")

if __name__ == "__main__":
    p = """H 0
    CNOT 0 1
    MEASURE 1
    MEASURE 0"""
    Program(p)
