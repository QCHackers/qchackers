from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *

qvm = QVMConnection()

ins = Program()

ins.inst(H(1), CNOT(1, 2))  # Creating B00
ins.inst(CNOT(0, 1), H(0))
ins.measure(0, 0).measure(1, 1).if_then(1, X(2)).if_then(0, Z(2))
wvf = qvm.wavefunction(ins, [0, 1])
#print( wvf)


ins = Program(
    H(0),
    H(1),
    CNOT(1, 2),
    CNOT(0, 1),
    H(0),
)


ins.measure(0, 0).measure(1, 1).if_then(1, X(2)).if_then(1, Z(2))
wvf = qvm.wavefunction(ins)

print(ins)
result = qvm.run_and_measure(ins, [2])
print(result)

print(wvf)
