from pyquil.quil import Program
from pyquil.gates import *
from pyquil.parameters import Parameter, quil_sin, quil_cos
from pyquil.quilbase import DefGate
#from pyquil.api import QVMConnection
from referenceqvm.api import QVMConnection
import numpy as np
theta = Parameter('theta')
cry = np.array([[1.0, 0.0, 0.0, 0.0], [0.0, 1.0, 0.0, 0.0], [0.0, 0.0, quil_cos(
    theta / 2), -1 * quil_sin(theta / 2)], [0.0, 0.0, quil_sin(theta / 2), quil_cos(theta / 2)]])
dg = DefGate('CRY', cry, [theta])
CRY = dg.get_constructor()
p = Program()
p.inst(dg)
p.inst(X(0))
p.inst(X(1))
p.inst(CRY(4.304)(0, 2))
qvm = QVMConnection()
wf = qvm.wavefunction(p)
print(wf)
