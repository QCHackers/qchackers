from qvm import vm

class Program:
    "Consists of a list of instructions"
    def __init__(self, instructions):
        self.instructions = instructions

    def eval(self):
        return vm.evaluate(self.instructions, "string")

def run(instructions):
    p = Program(instructions)
    return p.eval()

if __name__ == "__main__":
    instructions = """QUBITS 3
    H 1
    CNOT 1 2
    CNOT 0 1
    H 0
    MEASURE 0
    MEASURE 1"""


    p = Program(instructions)
    w, m = p.eval()
    print(m)
