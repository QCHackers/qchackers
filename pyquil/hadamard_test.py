from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *


#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()


def measurement_distribution(result):
    num_zeros = 0
    num_ones = 0
    for x in result:
        if x == [0]:
            num_zeros += 1
        else:
            num_ones += 1
    return [num_zeros, num_ones]


def get_Re(result):
    dist = measurement_distribution(result)
    return dist[0] - dist[1]


hadamard_test0 = Program(
    X(0),
    H(0),
    CNOT(0, 1),
    H(0),
)
print("Expected value will be closer to 0 than 100 or -100")
result = qvm.run_and_measure(hadamard_test0, [0], 100)
print(get_Re(result))

hadamard_test1 = Program(
    H(1),
    H(0),
    CNOT(0, 1),
    H(0),
)

print("Expected value will 1")
result = qvm.run_and_measure(hadamard_test1, [0], 1)
print(get_Re(result))
