from pyquil.quil import Program
p = Program()
from pyquil.gates import X
p.inst(X(0)).measure(0, 0)
print(p)

from pyquil.api import QVMConnection
qvm = QVMConnection()
print(qvm.run(p, [0]))
