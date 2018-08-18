from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *
from numpy import pi

#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()
exp = Program(RX(pi/2, 0),
              I(0),
              RZ(-pi/2, 0),

              RX(-pi/2, 0),
              I(0),
              RZ(pi/2, 0))

print("EXPERIMENT 1")
result = qvm.run_and_measure(exp, [0], 10)
print(result)
