# unfinished: https://www.nature.com/articles/s41467-017-01904-7

from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *


#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()
grover = Program(X(3), H(0), H(1), H(2), H(3), X(0) CNOT(0, 1), CCNOT(1, 2, 3), H(0), H(1), H(2), H(3), X(0), X(1), X(2))
print("3 Qubit Grover")
result = qvm.run_and_measure(grover, [0, 1, 2], 100)
print(result)
