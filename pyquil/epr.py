from pyquil.quil import Program
import pyquil.api as api
from pyquil.gates import *
qvm = api.QVMConnection()
p = Program()
p.inst(H(0), CNOT(0, 1), MEASURE(1, [1]))
wavefunction = qvm.wavefunction(p)
print(wavefunction)
