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
    instructions = """QUBITS 2
    H 0
    CNOT 0 1
    MEASURE 1
    MEASURE 0"""
    p = Program(instructions)
    w, m = p.eval()
    print(m)
