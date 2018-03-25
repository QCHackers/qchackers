import numpy as np
from .qubit import Qubit

class QuantumComputer:
    "Defines a quantum computer. Contains a wavefunction, quantum register, and classical register"
    cregister = []
    qregister = []
    applied_gates = []
    ket_zero = np.matrix([[1], [0]])
    ket_one = np.matrix([[0], [1]])
    wvf = ket_zero

    def __init__(self, size):
        for i in range(size):
            self.qregister.append(Qubit(str(i)))
            self.cregister.append(-1)
            if i != 1:
                self.wvf = np.kron(self.wvf, self.ket_zero)

    def set_cregister(self, register_addr, val):
        self.cregister[register_addr] = val
        return val
