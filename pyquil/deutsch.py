from pyquil import *
from pyquil.gates import *

#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()


# f0
deutsch = Program(
    X(0),
    CCNOT(0, 1, 2),
    CNOT(0, 1)
)

#print("Deutsch QVM N = 2 constant")
#print(qvm.run_and_measure(deutsch, [0,1,2]))
print(qvm.wavefunction(deutsch))


#print("Deutsch QVM N = 2 constant")
#result = qvm.wavefunction(deutsch)
# print(result)

# f2
deutsch = Program(
    # Put qubit 0 and qubit 1 into superposition
    X(1),
    H(0),
    H(1),
    # oracle
    CNOT(0, 1),

    H(0),

)

print("Deutsch QVM N = 2 balanced")
result = qvm.run_and_measure(deutsch, [0], 5)
print(result)


# f3
p = Program()
p.inst("""DEFGATE ZCNOT:
    0, 1, 0, 0
    1, 0, 0, 0
    0, 0, 1, 0
    0, 0, 0, 1""")
p.inst(H(0))
p.inst(X(1))
p.inst(H(1))
p.inst("ZCNOT 0 1")
p.inst(H(0))
p.inst(MEASURE(0))


#print("Deutsch QVM N = 2 balanced")
#result = qvm.run_and_measure(p, [0], 5)
# print(result)


# N = 3 balanced
deutsch = Program(
    H(0),
    H(1),
    H(2),
    # oracle
    H(2),
    Z(0),
    CNOT(1, 2),
    H(2),

    H(0),
    H(1),
    H(2),


)

#print("Deutsch QVM N = 3 balanced")
#result = qvm.run_and_measure(deutsch, [0], 5)
# print(result)


# N = 3 constant
deutsch = Program(
    H(0),
    H(1),
    H(2),

    H(0),
    H(1),
    H(2),


)

#print("Deutsch QVM N = 3 constant")
#result = qvm.run_and_measure(deutsch, [0], 5)
# print(result)
