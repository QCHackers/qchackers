from pyquil.paulis import ID, sX, sY, sZ
from pyquil import *
from pyquil.gates import H

import pyquil.paulis as pl

# Pauli term takes an operator "X", "Y", "Z", or "I"; a qubit to act on, and
# an optional coefficient.
a = 1 * ID()
b = -0.75 * sX(0) * sY(1) * sZ(3)
c = (5-2j) * sZ(1) * sX(2)

# Construct a sum of Pauli terms.
sigma = a + b + c
print("sigma = {}".format(sigma))
p = Program()
p.inst(pl.exponentiate_commuting_pauli_sum(sigma))

print(p)
