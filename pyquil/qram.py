from pyquil.quil import Program
#from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *


#qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection(use_queue=True)


# f0
qram = Program(

    X(10),
    CNOT(2, 6),
    CNOT(6, 10),
    CCNOT(6, 4, 5),
    CCNOT(1, 6, 4),
    CCNOT(1, 10, 8),
    CNOT(8, 10),
    CNOT(4, 6),
    CCNOT(0, 4, 3),
    CCNOT(0, 6, 5),
    CCNOT(0, 8, 7),
    CCNOT(0, 10, 9),
    CNOT(9, 10),
    CNOT(7, 8),
    CNOT(5, 6),
    CNOT(3, 4),
    CCNOT(10, 18, 19),
    CCNOT(9, 17, 19),
    CCNOT(8, 16, 19),
    CCNOT(7, 15, 19),
    CCNOT(6, 14, 19),
    CCNOT(5, 13, 19),
    CCNOT(4, 12, 19),
    CCNOT(3, 11, 19),

)


print(qram.instructions())
#result = qvm.run_and_measure(qram, [10,18,19])
#print("Expected 1 " , result)
