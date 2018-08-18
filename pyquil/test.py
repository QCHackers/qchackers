from pyquil import *
from pyquil.gates import *
import copy

qvm = QVMConnection()
p = Program(H(0))

print(qvm.wavefunction(p))


#results = qvm.run_and_measure(p, [0])


# print(results)
"""
p1 = p.copy()

p1.inst(X(0))

print("P ", p)
print("P1 ", p1)



wf = qvm.wavefunction(p)
print(wf)


for device in get_devices():
    if device.is_online():
        print('Device {} is online'.format(device.name))
"""
