from pyquil.quil import Program
from pyquil.api import QPUConnection
from pyquil.gates import *


qpu = QPUConnection(device_name='19Q-Acorn')

grover = Program(
    I(0),
    I(1),
    I(4),
    I(5),
    CNOT(5, 2),
    CNOT(0, 2),
    CNOT(4, 2),
    CNOT(1, 2),

)
print("Plaquette Z 0000")
result = qpu.run_and_measure(grover, [2], 100)
print(result)
