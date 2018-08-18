from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *


#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()


# f0
zero_one = Program(
    # Put qubit 0 and qubit 1 into superposition
    H(0),
    CNOT(0, 1),
    X(0),
    Z(0),
    CNOT(0, 1),
    H(0),

)
wvf = qvm.wavefunction(zero_one)

print(wvf)
print("0 1")
result = qvm.run_and_measure(zero_one, [0, 1], 100)
print(result)
