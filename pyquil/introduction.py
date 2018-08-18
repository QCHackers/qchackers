# http://pyquil.readthedocs.io/en/latest/intro.html

# Imports for pyQuil (ignore for now)
import numpy as np
from pyquil.quil import Program
from pyquil.api import QVMConnection
quantum_simulator = QVMConnection()

# pyQuil is based around operations (or gates) so we will start with the most
# basic one: the identity operation, called I. I takes one argument, the index
# of the qubit that it should be applied to.
from pyquil.gates import *

# Make a quantum program that allocates one qubit (qubit #0) and does nothing to it
p = Program(I(0))

print(p.inst(X(0)))

# Quantum states are called wavefunctions for historical reasons.
# We can run this basic program on our connection to the simulator.
# This call will return the state of our qubits after we run program p.
# This api call returns a tuple, but we'll ignore the second value for now.
wavefunction = quantum_simulator.wavefunction(p)

# wavefunction is a Wavefunction object that stores a quantum state as a list of amplitudes
alpha, beta = wavefunction

print("Our qubit is in the state alpha={} and beta={}".format(alpha, beta))
print("The probability of measuring the qubit in outcome 0 is {}".format(abs(alpha)**2))
print("The probability of measuring the qubit in outcome 1 is {}".format(abs(beta)**2))
