"""Defines program object"""
import sys

from kite import vm

def H(qubit):
    return "H " + str(qubit) + "\n"


def QREG(num_qubits):
    return "QUBITS " + str(num_qubits) + "\n"


def X(qubit):
    return "X " + str(qubit) + "\n"


def CNOT(qubit, qubit1):
    return "CNOT " + str(qubit) + " " + str(qubit1) + "\n"


def MEASURE(qubit):
    return "MEASURE " + str(qubit) + "\n"

class API:
    "Provides a file API and a CLI API to the VM"

    @classmethod
    def file_api(self, file_name):
        a, b = vm.evaluate(file_name, "file")
        return a, b

    @classmethod
    def cli_api(self, instructions_string):
        a, b = vm.evaluate(instructions_string, "string")
        return a, b

class Program:
    "Consists of a list of instructions"

    def __init__(self, *instructions):
        program = "". join(instructions)
        self.instructions = program

    def run(self):
        return API.cli_api(self.instructions)


if __name__ == "__main__":
    result = API.file_api(sys.argv[1])
    print(result)
