from pyquil.quil import Program
from pyquil.parameters import Parameter, cos, sin, exp
import numpy as np

p = Program()

"""
Gate definitions
"""

theta = Parameter('theta')
crx = np.array([[1, 0, 0, 0],
                [0, 1, 0, 0],
                [0, 0, cos(theta / 2), -1j * sin(theta / 2)],
                [0, 0, -1j * sin(theta / 2), cos(theta / 2)]])

p = Program().defgate("CRX", crx, [theta])

cry = np.array([[1, 0, 0, 0],
                [0, 1, 0, 0],
                [0, 0, cos(theta / 2), -sin(theta / 2)],
                [0, 0, sin(theta / 2), cos(theta / 2)]])

p = Program().defgate("CRY", cry, [theta])


crz = np.array([[1, 0, 0, 0],
                [0, 1, 0, 0],
                [0, 0, exp(-1j*(theta/2)), 0],
                [0, 0, 0, exp(1j*(theta/2))]])

#p = Program().defgate("CRZ", crz, [theta])

print(p.out())
