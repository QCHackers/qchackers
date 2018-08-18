from pyquil.quil import Program
from pyquil.api import QPUConnection
from pyquil.gates import *


qpu = QPUConnection(device_name='19Q-Acorn')

grover = Program(
    H(1),
    H(2),
    S(1),
    S(2),
    H(2),
    CNOT(1, 2),
    H(2),
    S(1),
    S(2),
    H(1),
    H(2),
    X(1),
    X(2),
    H(2),
    CNOT(1, 2),
    H(2),
    X(1),
    X(2),
    H(1),
    H(2),

)
print("Grover N=2 A=00")
result = qpu.run_and_measure(grover, [1, 2], 100)
print(result)


grover = Program(
    H(1),
    H(2),
    S(2),
    H(2),
    CNOT(1, 2),
    H(2),
    S(2),
    H(1),
    H(2),
    X(1),
    X(2),
    H(2),
    CNOT(1, 2),
    H(2),
    X(1),
    X(2),
    H(1),
    H(2),

)
print("Grover N=2 A=01")
result = qpu.run_and_measure(grover, [1, 2], 100)
print(result)


grover = Program(
    H(1),
    H(2),
    S(1),
    H(2),
    CNOT(1, 2),
    H(2),
    S(1),
    H(1),
    H(2),
    X(1),
    X(2),
    H(2),
    CNOT(1, 2),
    H(2),
    X(1),
    X(2),
    H(1),
    H(2),

)
print("Grover N=2 A=10")
result = qpu.run_and_measure(grover, [1, 2], 100)
print(result)


grover = Program(
    H(1),
    H(2),
    H(2),
    CNOT(1, 2),
    H(2),
    H(1),
    H(2),
    X(1),
    X(2),
    H(2),
    CNOT(1, 2),
    H(2),
    X(1),
    X(2),
    H(1),
    H(2),

)
print("Grover N=2 A=11")
result = qpu.run_and_measure(grover, [1, 2], 100)
print(result)
