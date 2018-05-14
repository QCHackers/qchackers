from pyquil.quil import Program
from pyquil.api import QPUConnection
from pyquil.gates import *

qpu = QPUConnection(device_name='19Q-Acorn')

bell_state = Program(H(0), CNOT(0, 1))
result = qpu.run_and_measure(bell_state, [0, 1], 100)
print(result)
