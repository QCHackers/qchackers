import numpy as np
from time import time
import sys
import random
import itertools
import re
from .gates import Gates
from .quantumcomputer import QuantumComputer

np.random.seed(int(time()))

# @TODO Document code
# @TODO Implement try/catch where appropriate
# @TODO Figure out importing scheme so both top level can import as well as the ability to run tests
# @TODO revisit overall architecture
# @TODO Better print helper functions

def two_n_size(wvf):
    "wvf = 2^n. Solve for n"
    return  int(np.log(len(wvf))/np.log(2))

def wavefunction(wvf):
    "Returns the wavefunction in dirac notation"
    perm_list = ["".join(seq) for seq in itertools.product("01", repeat = int(np.log2(len(wvf))))]
    wvf_string = ""
    for x in range(0, len(perm_list)):
        if wvf[x, 0] != 0:
            wvf_string += str(np.around(wvf[x,0], decimals = 2)) +  "|" + perm_list[x] + "> + "
    wvf_string = re.split(r'(\s+)', wvf_string)[:-4]
    wvf_string = "".join(wvf_string)

    return wvf_string

def i_gen(num):
    "Generates the tensor product of num identity gates"
    gates = Gates.I
    if num > 0:
        for x in range(1, num):
            gates = np.kron(Gates.I, gates)
    return gates

def append_gate(qubit, qubit1, QC):
    "Adds gate to apply_gates list to help with printing the wavefunction"
    if qubit.address not in QC.applied_gates:
        QC.applied_gates.append(qubit.address)

    if not qubit1 is None and qubit1.address not in QC.applied_gates:
        QC.applied_gates.append(qubit1.address)


def get_base_gate(gate_str, Gates):
    "Returns the gate that was applied"

    if gate_str in Gates.gates_set:
        return Gates.gates_set[gate_str]
    else:
        raise Exception("Gate not implemented")

def build_gate(addr, wvf_size, x, spacing_num):
    #print(f"==== {addr} \n {wvf_size} \n {x} \n {spacing_num}")
    "Generates the tensor product of quantum gate and spacing_num identity gates"

    gate = x
    if addr == 0:
        if spacing_num > 0:
            gate = np.kron(gate, i_gen(spacing_num))
    else:
        if spacing_num > 0:
            gate = np.kron(gate, i_gen(spacing_num))
        if wvf_size - spacing_num > 0:
            gate = np.kron(i_gen(addr), gate)

    return gate

def apply_gate(qubit, wvf, gate_str, Gates, QC, qubit1 = None):
    "Performs quantum gate operation on the wavefunction"
    #@TODO This is a little confusing - address can theoretically be arbitrary
    addr = int(qubit.address)
    wvf_size = two_n_size(wvf)
    # space 0 = Command

    # number of qubit spaces prior to the target qubit
    num_qubits_after = wvf_size - addr - 1

    # Grab matrix of the gate thats passed in
    base_gate = get_base_gate(gate_str, Gates)

    x = base_gate

    if not qubit1 is None:
        num_qubits_after -= 1

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

    msg = ""
    addr = qubit.address
    pr_zero = pr(qubit, wvf, 0, QC)
    pr_one = pr(qubit, wvf, 1, QC)

    sum = pr_zero + pr_one
    assert (round(sum) == 1.0),"Sum of probabilites does not equal 1"
    pr_val = [pr_zero, pr_one]

    collapsed_val = 0 if random.random() <= pr_zero else 1
    msg += f"wavefunction before measurement: {wavefunction(wvf)}\n"
    wvf = (proj(qubit, collapsed_val, wvf, QC) * wvf) / (np.sqrt(pr_val[collapsed_val]))
    qubit.measurement = collapsed_val
    QC.set_cregister(int(qubit.address), collapsed_val)
    msg += f"====== MEASURE qubit {addr} : {collapsed_val}\n"
    msg += f"wavefunction after measurement: {wavefunction(wvf)}\n\n"

    return wvf, msg

def isolate_qubit(wvf, qubit):
    """ Extract just a qubit from a wavefunction eg:
        qbit 3 => 0.71|001>+0.71|000> => 0.71|1> + 0.71|0>
        qbit 2 => 0.71|001>+0.71|000> => 0.71|0> + 0.71|0>"""

    qbit_str = ""
    wvf_slist = wavefunction(wvf).split(' + ')
    regex_str = "(-?\d+\.\d+)\|"
    for i in range(0, qubit):
        regex_str += '\d'

    regex_str += '(\d)'
    for wvf_part in wvf_slist:
        matches = re.findall(regex_str, wvf_part)
        if not matches is None:
            qbit_str += matches[0][0] + "|" + matches[0][1] + "> + "

    return qbit_str[:-3]

def evaluate(program, option):
    "Executes program in file"
    global gates
    global QuantumComputer
    global wv

    msg = ""
    if option == "string":
        fp = program.splitlines()
        args = fp.pop(0).split()
    else:
        fp = open(program)
        args = fp.readline().split()

    if args[0] != 'QUBITS':
        raise ValueError('Program must start with QUBITS {num_qubits}')
    else:
        num_qubits = int(args[1])
        if not num_qubits > 0:
            raise ValueError('Number of qubits must be an integer > 0')

    QC = QuantumComputer(int(num_qubits))
    gates = Gates()
    wv = QC.wvf

    enumerated_instructions = enumerate(fp)
    for (index, line) in enumerated_instructions:
        args = line.split()
        nArgs = len(args)
        operator = args[0]

        if nArgs == 2:
            qubit = int(args[1])
            if operator == "MEASURE":
                wv, measure_msg = MEASURE(QC.qregister[int(qubit)], wv, QC)
                msg += measure_msg
            else:
                wv = apply_gate(QC.qregister[int(qubit)], wv, operator, Gates, QC, None)
        elif nArgs == 3:
            qubit = int(args[1])
            qubit1 = int(args[2])
            # @TODO Support rotation gates with rotation values for arg[1]
            wv = apply_gate(QC.qregister[int(qubit)], wv, operator, Gates, QC, QC.qregister[int(qubit1)])
        elif nArgs == 4:
            register = int(args[1])
            value = int(args[2])
            following_lines = int(args[3])
            print(register)
            print(QC.get_creg_val(register))
            print(operator)
            if operator == "CLASSICAL":
                print(f"{value} ---- {QC.get_creg_val(register)}")
                if value != QC.get_creg_val(register):
                    for i in range(following_lines):
                        next(enumerated_instructions, None)

        else:
            raise Exception("Exit(1)")

    msg += f"Final wavefunction: \n{wavefunction(wv)}"
    return wv, msg
if __name__ == "__main__":
    evaluate(sys.argv[1], "file")
