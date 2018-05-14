from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *


#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()


# f0
qram = Program(
    #Put qubit 0 and qubit 1 into superposition
)

#print("Deutsch QVM N = 2 constant")
#result = qvm.run_and_measure(deutsch, [0], 5)
#print(result)
