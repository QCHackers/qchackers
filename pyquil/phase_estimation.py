from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *

#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()

phase_estimation_0 = Program(
    H(0),
    H(1),
    CNOT(0, 1),  # CNOT is another name for cX which is cU in this example
    H(0),
)
print("Expected answer is 0")
result = qvm.run_and_measure(phase_estimation_0, [0], 1)
print(result)

phase_estimation_1 = Program(
    H(0),
    Z(0),
    H(1),
    CNOT(0, 1),  # CNOT is another name for CX which is CU in this example
    H(0),
)
print("Expected answer is 1")
result = qvm.run_and_measure(phase_estimation_1, [0], 1)
print(result)
