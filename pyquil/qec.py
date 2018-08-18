from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
import pyquil.api as api
from pyquil.gates import *
import numpy as np
from pyquil.kraus import add_noise_to_program
from numpy import pi

# single step damping probability
damping_per_I = 0.02


#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = api.QVMConnection()

qec = Program(
    H(1),
    T(1),
    H(1),
    H(0),
    H(1),
    H(2),
    CNOT(0, 1),
    CNOT(2, 1),
    H(0),
    H(1),
    H(2),

)
#print("Encoder into bit-flip code (qubits 0-2)")
#result = qpu.run_and_measure(qec, [0,1,2], 100)
# print(result)


qec = Program(
    I(0),
    I(1),
    I(4),
    I(5),
    CNOT(5, 2),
    CNOT(0, 2),
    CNOT(4, 2),
    CNOT(1, 2),

)
#print("Plaquette Z 0000")
#result = qpu.run_and_measure(qec, [2], 100)
# print(result)
"""
First qubit: 10
Second qubit: 11
Third qubit: 01

Measuring the Error syndrome doesn't destroy the superposition of the logical states.

This code is sensitive to phase flip errors.

"""
bit_flip = Program(
    X(1),
    CNOT(0, 3),
    CNOT(1, 3),
    CNOT(1, 4),
    CNOT(2, 4),
)

result = qvm.run_and_measure(bit_flip, [3, 4], 10)
#print("Bit Flip %s" % result)


bit_flip_paper = Program(

    CNOT(0, 1),
    CNOT(1, 2),
    CNOT(0, 3),
    CNOT(1, 3),
    CNOT(0, 4),
    CNOT(2, 4),
)

result = qvm.run_and_measure(bit_flip_paper, [3, 4], 10)
#print("Bit Flip Paper %s" % result)

"""
https://en.wikipedia.org/wiki/Quantum_error_correction
"""
bit_flip_wiki = Program(

    CNOT(0, 1),
    CNOT(0, 2),
    X(0),

    # Parity
    CNOT(0, 3),
    CNOT(1, 3),
    CNOT(1, 4),
    CNOT(2, 4),

    CNOT(0, 1),
    CNOT(0, 2),
    CCNOT(2, 1, 0),


)

result = qvm.run_and_measure(bit_flip_wiki, [3, 4], 10)
#print("Bit Flip Wiki %s" % result)


"""
The phase flip acts like a bit flip but in the |+> |-> basis. Hadamard converts between the |0>, |1> basis to the |+> |-> basis. In the |+> |-> basis state, the Z gates acts as a bit flip and the X gates acts as a phase flip. Hadamard switches the X and Z gates. 

To correct a phase error do the same bit flip code, but do it in a |+> and |-> basis.

This code is sensitive to bit flip errors

"""
phase_flip = Program(

    X(1),
    CNOT(0, 3),
    CNOT(1, 3),
    CNOT(1, 4),
    CNOT(2, 4),
    H(0),
    H(1),
    H(2),
)


#result = qvm.run_and_measure(qec, [3,4], 10)
#print("Phase Flip %s" % result)

phase_flip_paper = Program(
    CNOT(0, 1),
    CNOT(1, 2),
    X(2),
    CNOT(0, 3),
    CNOT(1, 3),
    CNOT(0, 4),
    CNOT(2, 4),
    H(0),
    H(1),
    H(2),
)

result = qvm.run_and_measure(phase_flip_paper, [3, 4], 10)
#print("Phase Flip Paper %s" % result)


"""
https://en.wikipedia.org/wiki/Quantum_error_correction
"""
phase_flip_wiki = Program(

    CNOT(0, 1),
    CNOT(0, 2),
    H(0),
    H(1),
    H(2),
    Z(1),
    Z(1),
    Z(1),
    H(0),
    H(1),
    H(2),
    CNOT(0, 1),
    CNOT(0, 2),
    CCNOT(2, 1, 0),
)

result = qvm.run_and_measure(phase_flip_wiki, [0], 10)
#print("Phase Flip Wiki %s" % result)

"""
Can correct for bit flip errors and phase errors. Like a Y gate which is a bit flip and a phase flip.

"""
shor_code = Program(

    CNOT(0, 3),
    CNOT(0, 6),
    H(0),
    H(3),
    H(6),
    CNOT(0, 1),
    CNOT(3, 4),
    CNOT(6, 7),
    CNOT(0, 3),
    CNOT(3, 5),
    CNOT(6, 8),



    CNOT(0, 1),
    CNOT(3, 4),
    CNOT(6, 7),
    CNOT(0, 2),
    CNOT(3, 5),
    CNOT(6, 8),
    CCNOT(2, 1, 0),
    CCNOT(5, 4, 2),
    CCNOT(8, 7, 6),
    H(0),
    H(3),
    H(6),
    CNOT(0, 3),
    CNOT(0, 6),
    CCNOT(6, 3, 0),


    # Parity bit flip block 0
    CNOT(0, 9),
    CNOT(1, 9),
    CNOT(1, 10),
    CNOT(2, 10),


    # Parity bit flip block 1
    CNOT(3, 11),
    CNOT(4, 11),
    CNOT(4, 12),
    CNOT(5, 12),


    # Parity bit flip block 2
    CNOT(6, 13),
    CNOT(7, 13),
    CNOT(7, 14),
    CNOT(8, 14),


    # Parity bit flip block 2
    CNOT(0, 15),
    CNOT(3, 15),
    CNOT(3, 16),
    CNOT(6, 16),
)


result = qvm.run_and_measure(shor_code, [9, 10], 10)
#print("Shor %s" % result)

"""
https://en.wikipedia.org/wiki/Quantum_error_correction
Z and Y error: 011 First qubit is protected.

Like the purely Phase flip code, the X gate acts as phase flip and Z acts as bit flip



Phase flip measured by qubit 15 and 16:

X: Passes for all bit flip tests
Z: Passes for all phase flip tests
Y: Passes for all Phase flip tests and bit flip!
"""
shor_code_wiki = Program(
    CNOT(0, 3),
    CNOT(0, 6),
    H(0),
    H(3),
    H(6),
    CNOT(0, 1),
    CNOT(3, 4),
    CNOT(6, 7),
    CNOT(0, 2),
    CNOT(3, 5),
    CNOT(6, 8),

    # Error
    Y(0),

    # Parity bit flip block 0: This works
    CNOT(0, 9),
    CNOT(1, 9),
    CNOT(1, 10),
    CNOT(2, 10),

    # Parity bit flip block 1
    CNOT(3, 11),
    CNOT(4, 11),
    CNOT(4, 12),
    CNOT(5, 12),

    # Parity bit flip block 2
    CNOT(6, 13),
    CNOT(7, 13),
    CNOT(7, 14),
    CNOT(8, 14),

    #Decoding and correcting
    CNOT(0, 1),
    CNOT(3, 4),
    CNOT(6, 7),
    CNOT(0, 2),
    CNOT(3, 5),
    CNOT(6, 8),
    CCNOT(2, 1, 0),
    CCNOT(5, 4, 2),
    CCNOT(8, 7, 6),
    H(0),
    H(3),
    H(6),
    CNOT(0, 3),
    CNOT(0, 6),
    CCNOT(6, 3, 0),

    # Parity bit Phase flip
    CNOT(0, 15),
    CNOT(3, 15),
    CNOT(3, 16),
    CNOT(6, 16),
)

result = qvm.run_and_measure(shor_code_wiki, [9, 10, 15, 16], 5)
#print("Shor Code Wiki %s" % result)


z_correction = Program(

    H(0),
    H(1),
    H(2),
    H(3),
    H(4),
    H(5),
    H(6),
    H(7),
    H(8),
    CNOT(0, 9),
    CNOT(1, 9),
    CNOT(2, 9),
    CNOT(3, 9),
    CNOT(3, 9),
    CNOT(0, 5),

)


result = qvm.run_and_measure(z_correction, [3, 4], 10)
#print("Shor %s" % result)


steane_code = Program(

    H(6),
    H(7),
    H(8),
    H(9),
    H(10),
    H(11),
    H(12),

    CNOT(6, 0),

    CNOT(7, 1),

    CNOT(8, 2),

    CNOT(9, 2),
    CNOT(9, 1),

    CNOT(10, 2),
    CNOT(10, 0),

    CNOT(11, 2),
    CNOT(11, 1),
    CNOT(11, 0),

    CNOT(12, 1),
    CNOT(12, 0),

    H(6),
    H(7),
    H(8),
    H(9),
    H(10),
    H(11),
    H(12),

    CNOT(6, 5),
    CNOT(6, 3),

    CNOT(7, 5),
    CNOT(7, 4),


    CNOT(8, 5),
    CNOT(8, 4),
    CNOT(8, 3),

    CNOT(9, 3),

    CNOT(10, 4),

    CNOT(11, 5),


    CNOT(12, 4),
    CNOT(12, 3),



)


result = qvm.run_and_measure(steane_code, [0, 1, 2, 3, 4, 5], 1)
print("Steane %s" % result)


def damping_channel(damp_prob=.1):
    """
    Generate the Kraus operators corresponding to an amplitude damping
    noise channel.

    :params float damp_prob: The one-step damping probability.
    :return: A list [k1, k2] of the Kraus operators that parametrize the map.
    :rtype: list
    """
    damping_op = np.sqrt(damp_prob) * np.array([[0, 1],
                                                [0, 0]])

    residual_kraus = np.diag([1, np.sqrt(1-damp_prob)])
    return [residual_kraus, damping_op]


def append_damping_to_gate(gate, damp_prob=.1):
    """
    Generate the Kraus operators corresponding to a given unitary
    single qubit gate followed by an amplitude damping noise channel.

    :params np.ndarray|list gate: The 2x2 unitary gate matrix.
    :params float damp_prob: The one-step damping probability.
    :return: A list [k1, k2] of the Kraus operators that parametrize the map.
    :rtype: list
    """
    return append_kraus_to_gate(damping_channel(damp_prob), gate)


num_I = 10


def append_kraus_to_gate(kraus_ops, g):
    """
    Follow a gate `g` by a Kraus map described by `kraus_ops`.

    :param list kraus_ops: The Kraus operators.
    :param numpy.ndarray g: The unitary gate.
    :return: A list of transformed Kraus operators.
    """
    return [kj.dot(g) for kj in kraus_ops]


def get_compiled_prog(theta):
    return Program([
        RZ(-pi/2, 0),
        RX(-pi/2, 0),
        RZ(-pi/2, 1),
        RX(pi/2, 1),
        CZ(1, 0),
        RZ(-pi/2, 1),
        RX(-pi/2, 1),
        RZ(theta, 1),
        RX(pi/2, 1),
        CZ(1, 0),
        RX(pi/2, 0),
        RZ(pi/2, 0),
        RZ(-pi/2, 1),
        RX(pi/2, 1),
        RZ(-pi/2, 1),
    ])


def H(qubit):
    return 1


def get_compiled_prog_1():
    return Program([
        RX(pi/2, 0),
        I(0),
        RZ(-pi/2, 0),

        RX(-pi/2, 0),
        I(0),
        RZ(pi/2, 0),
    ])


p = Program()
p.inst(X(0))
# want increasing number of I-gates
p.define_noisy_gate(
    "II", [0], append_damping_to_gate(np.eye(2), damping_per_I))
p.inst([I(0) for _ in range(num_I)])
# p.inst(H(0))
p.inst(MEASURE(0, [0]))
#print("Expected 1 %s" % qvm.run(p, [0]))

thetas = np.linspace(-pi, pi, num=20)
t1s = np.logspace(-6, -5, num=3)

# print(t1s[0])

prog = get_compiled_prog(pi/2)
noisy = add_noise_to_program(prog, T1=t1s[0]).inst([
    MEASURE(0, 0),
])
result = np.array(qvm.run(noisy, [0], 1))

# print(result)
