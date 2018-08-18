from pyquil.quil import Program
from pyquil.api import QPUConnection
from pyquil.api import QVMConnection
from pyquil.gates import *


qpu = QPUConnection(device_name='19Q-Acorn')
qvm = QVMConnection()


test_run_and_measure_empty = Program()

print(test_run_and_measure_empty)
result = qvm.run_and_measure(Program(), [0], 1)


test_run = Program(
    H(0),
    CNOT(0, 1)

)

print("test_run")
result = qvm.run(test_run, [0], 1)
print(result)


test_run_async = Program(
    H(0),
    CNOT(0, 1)

)

print("test_run_async")
result = qvm.run(test_run_async, [0], 1)
print(result)


test_run_and_measure = Program(
    H(0),
    CNOT(0, 1)

)

print("test_run_and_measure")
result = qvm.run_and_measure(test_run_and_measure, [0], 1)
print(result)


test_run_and_measure_async = Program(
    H(0),
    CNOT(0, 1)

)

print("test_run_and_measure_async")
result = qvm.run_and_measure(test_run_and_measure_async, [0], 1)
print(result)


test_run_empty = Program()

print("Test run empty")
result = qvm.run(test_run_empty, [0], 1)
print(result)


test_run_async_empty = Program()

#print("Test run async empty")
#result = qvm.run(test_run_async_empty, [0], 1)
# print(result)


test_run_and_measure_empty = Program()

#print("Test run_and_measure_empty")
#result = qvm.run_and_measure(test_run_and_measure_empty, [0], 1)
# print(result)


test_run_and_measure_async_empty = Program()

# print("test_run_and_measure_async")
#result = qvm.run_and_measure(test_run_and_measure_async_empty, [0], 1)
# print(result)
