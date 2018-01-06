from pyquil.quil import Program
from pyquil.api import QPUConnection
from pyquil.gates import *


from pyquil.api import get_devices

"""
from pyquil.api import get_devices
for device in get_devices():
    if device.is_online():
        print('Device {} is online'.format(device.name))

"""

qpu = QPUConnection(device_name='19Q-Acorn')

ghz_state = Program(H(0), H(1), X(2), CNOT(1, 2), CNOT(0, 2), H(0), H(1), H(2))
print("3Q GHZ State")
result = qpu.run_and_measure(ghz_state, [0, 1, 2], 100)
print(result)


