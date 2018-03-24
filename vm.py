import numpy as np
from time import time
import sys
import random
import cmath
import itertools
import re
np.random.seed(int(time()))

class Qubit:
    "Define a qubit"
    def __init__(self, address):
        self.address = address
        self.measurement = None

class Gates:
    "Contains basic quantum gates"

    #Rotation
    def RX(self, theta):
        return np.matrix([[round(np.cos(theta/ 2 )), np.around(-1j * np.sin(theta / 2))],
                          [np.around(-1j * np.sin(theta / 2)), np.around(np.cos(theta/ 2 ))]])

    def RY(self, theta):
        return np.matrix([[np.cos(theta/ 2 ),  np.sin(theta / 2)],
                          [-1j * np.sin(theta / 2), np.cos(theta/ 2 )]])

    def RZ(self, theta):
        return np.matrix([[np.exp(-1j * theta / 2),  0],
                          [0, np.exp(1j * theta / 2)]])

    #pauli
    I = np.matrix([[1, 0], [0, 1]])
    X = np.matrix([[0, 1], [1, 0]])
    Y = np.matrix([[0, 0 - 1j], [0 + 1j, 0]])
    Z = np.matrix([[1, 0], [0, -1]])

    #universal gates
    H = (X + Z)/np.sqrt(2)
    T = np.matrix([[1, 0], [0, cmath.exp(1j * np.pi / 4)]])
    S = np.matrix([[1.0, 0.0], [0.0, 1.0j]])
    CNOT = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]])

    #miscellaneous
    CZ = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, -1, 0]])
    SWAP = np.matrix([[1, 0, 0, 0],
                      [0, 0, 1, 0],
                      [0, 1, 0, 0],
                      [0, 0, 0, 1]])

class QuantumComputer:
    "Defines a quantum computer. Contains a wavefunction, quantum register, and classical register"
    cregister = [0] * 5
    qregister = []
    applied_gates = []
    ket_zero = np.matrix([[1], [0]])
    ket_one = np.matrix([[0], [1]])
    wvf = ket_zero

    def __init__(self, size):
        for i in range(size):
            self.qregister.append(Qubit(str(i)))
            if i != 1:
                self.wvf = np.kron(self.wvf, self.ket_zero)


def two_n_size(wvf):
    "wvf = 2^n. Solve for n"
    return  int(np.log(len(wvf))/np.log(2))

def wavefunction(wvf, QC):
    "Returns the wavefunction in dirac notation"
    perm_list = ["".join(seq) for seq in itertools.product("01", repeat=int(np.sqrt(len(wvf))))]
    wvf_string = ""
    ap_gate_len = len(QC.applied_gates)

    for x in range(0, len(perm_list)):
        if wvf[x,0] != 0:
            if x != len(perm_list)-1 and len(wvf) != 2:
                wvf_string += wvf_string + str(np.around(wvf[x,0], decimals = 2)) +  "|" + perm_list[x][:ap_gate_len] + ">" + " + "
            else:
                wvf_string +=  wvf_string + str(np.around(wvf[x,0], decimals = 2)) +  "|" + perm_list[x] + ">"

    wvf_string = re.split(r'(\s+)', wvf_string)[:-3]
    wvf_string = "".join(wvf_string)

    return wvf_string

def i_gen(num):
    "Generates the tensor product of num identity gates"
    gates = Gates.I
    if num > 0:
        for x in range(1, num):
            gates = np.kron(Gates.I, gates)
    return gates

def build_gate(addr, wvf_size, x, spacing_num):
    #26print(f"==== {addr} \n {wvf_size} \n {x} \n {spacing_num}")
    "Generates the tensor product of quantum gate and spacing_num identity gates"

    if spacing_num == wvf_size - 1:
        gate = np.kron(x, i_gen(spacing_num))
    elif spacing_num == 0:
        gate = np.kron(i_gen(addr), x)
    elif spacing_num == -1:
        gate = x
    else:
        gate = np.kron(i_gen(addr), x)
        gate = np.kron(gate, i_gen(spacing_num))

    return gate

def append_gate(qubit, qubit1, QC):
    "Adds gate to apply_gates list to help with printing the wavefunction"
    if qubit.address not in QC.applied_gates:
        QC.applied_gates.append(qubit.address)

    if not qubit1 is None and qubit1.address not in QC.applied_gates:
        QC.applied_gates.append(qubit1.address)


def get_base_gate(gate_str, Gates):
    "Returns the gate that was applied"
    if gate_str == "H":
        base_gate = Gates.H
    elif gate_str == "X":
        base_gate = Gates.X
    elif gate_str == "S":
        base_gate = Gates.S
    elif gate_str == "Z":
        base_gate = Gates.Z
    elif gate_str == "CNOT":
         base_gate = Gates.CNOT
    else:
        raise Exception("Gate not implemented")
    return base_gate


def apply_gate(qubit, wvf, gate_str, Gates, QC, qubit1 = None):
    "Performs quantum gate operation on the wavefunction"
    #@TODO This is a little confusing - address can theoretically be arbitrary
    addr = int(qubit.address)
    wvf_size = two_n_size(wvf)
    # space 0 = Command

    # number of qubit spaces prior to the target qubit
    # @TODO this needs to be dynamic
    num_qubits_after = wvf_size - addr - 1

    # Grab matrix of the gate thats passed in
    base_gate = get_base_gate(gate_str, Gates)

    x = base_gate

    if not qubit1 is None:
        num_qubits_after -= 2

    # Build the base gate for the appropriate number of qubits
    # @TODO This doesn't work for 2 qubit gates with non-consecutive
    gate = build_gate(addr, wvf_size, x, num_qubits_after)
    append_gate(qubit, qubit1, QC)

    wvf = gate * wvf

    return wvf

def proj(qubit,basis, wvf, QC):
    "Computes the projector ono the wavefunction: P_(w_i) = |w_i><w_i|"

    addr = int(qubit.address)
    wvf_size = two_n_size(wvf) - 1

    if basis == 0:
        proj = np.outer(QC.ket_zero , QC.ket_zero)
    else:
        proj = np.outer(QC.ket_one , QC.ket_one)


    if addr == 0:
        proj = np.kron(proj, i_gen(wvf_size))
    elif addr == wvf_size:
         proj = np.kron(i_gen(addr), proj)
    else:
        proj = np.kron(i_gen(addr), proj)
        proj = np.kron(proj, i_gen(wvf_size - addr))

    return proj

def pr(qubit, wvf, basis, QC):
    "Computes probability of getting an outcome: Pr(|w_i>) = <v|Pw_i|v>"

    wvf_bra = wvf.getH()
    ket = proj(qubit, basis, wvf, QC) * wvf
    answer = wvf_bra * ket

    return answer[0,0]


def MEASURE(qubit, wvf, QC):
    "Performs a measurement on the qubit and modifies the wavefunction: |new wvf> = P_(w_i)|v/sqrt(Pr(|w_i>) "

    addr = qubit.address
    pr_zero = pr(qubit, wvf, 0, QC)
    pr_one = pr(qubit, wvf, 1, QC)

    sum = pr_zero + pr_one
    assert (round(sum) == 1.0),"Sum of probabilites does not equal 1"

    if random.random() <= pr_zero:
        print(wavefunction(wvf, QC))
        wvf = (proj(qubit, 0, wvf, QC) * wvf)/(np.sqrt(pr_zero))
        qubit.measurement = 0
        print(f"MEASUREMENT of qubit {addr} is 0")
        print(wavefunction(wvf, QC))
    else:
        print(wavefunction(wvf, QC))
        wvf = (proj(qubit, 1, wvf, QC) * wvf)/(np.sqrt(pr_one))
        qubit.measurement = 1
        print(f"MEASUREMENT of qubit {addr} is 1")
        print(wavefunction(wvf, QC))

    return wvf


def evaluate(program, option):
    "Executes program in file"

    global Gates
    global QuantumComputer

    #@TODO Dynamic sizing based on instructions
    QC = QuantumComputer(2)
    Gates = Gates()
    global wv
    wv = QC.wvf
    if option == "string":
        print("It's a string")
        fp = program.splitlines()
    else:
        print("It's a file")
        fp = open(program)

    for line in fp:
        args = line.split()
        nArgs = len(args)
        operator = args[0]

        if nArgs == 2:
            qubit = int(args[1])
            if (operator == "MEASURE"):
                MEASURE(QC.qregister[int(qubit)], wv, QC)
            else:
                wv = apply_gate(QC.qregister[int(qubit)], wv, operator, Gates, QC, None)
        elif nArgs == 3:
            operator = args[0]
            qubit = int(args[1])
            qubit1 = int(args[2])
            # @TODO Support rotation gates with rotation values for arg[1]
            wv = apply_gate(QC.qregister[int(qubit)], wv, operator, Gates, QC, QC.qregister[int(qubit1)])
        else:
            raise Exception("Exit(1)")

if __name__ == "__main__":
    evaluate(sys.argv[1], "file")
