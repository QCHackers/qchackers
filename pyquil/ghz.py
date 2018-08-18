from pyquil.quil import Program
from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *


#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()
ghz_state = Program(H(0), H(1), X(2), CNOT(1, 2), CNOT(0, 2), H(0), H(1), H(2))
ghz_state_efficient = Program(H(0), CNOT(0, 1), CNOT(1, 2))
print("3Q GHZ State QVM")
result = qvm.run_and_measure(ghz_state, [0, 1, 2], 100)
print(result)
