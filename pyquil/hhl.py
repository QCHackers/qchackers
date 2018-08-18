from pyquil.quil import Program
from pyquil.gates import H, X, CNOT, MEASURE
from pyquil.api import SyncConnection

p = Program()
p.inst("""DEFGATE CH(%theta):
    1, 0, 0, 0
    0, 1, 0, 0
    0, 0, cos(2*%theta), sin(2*%theta)
    0, 0, sin(2*%theta), cos(-2*%theta)""")
p.inst(H(3))
p.inst(CNOT(3, 2))
p.inst(CNOT(2, 1))
p.inst("CH(pi/8) 1 0")
p.inst("CH(pi/16) 2 0")
p.inst(H(1))
p.inst(H(2))
p.inst(X(0))
p.inst(MEASURE(0))
p.inst(MEASURE(1))
p.inst(MEASURE(2))

# run the program on a QVM
qvm = SyncConnection()

wvf, _ = qvm.wavefunction(p)
print(wvf)
