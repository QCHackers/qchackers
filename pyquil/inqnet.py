# https://www.reddit.com/r/QuantumComputing/comments/7tfl74/what_does_this_circuit_do/?st=jcxukv2c&sh=f19ff94e
from pyquil.quil import Program
from pyquil.api import QVMConnection
from pyquil.gates import *

qvm = QVMConnection()

inqnet = Program(
    H(1),
    CNOT(0, 1),
    CNOT(1, 2),
    CNOT(0, 1),
    H(0),
    CNOT(1, 2),
    CZ(2, 0),
    H(0),
    H(0),

)
print("inqnet")
result = qvm.run_and_measure(inqnet, [0, 1, 2], 5)
print(result)
