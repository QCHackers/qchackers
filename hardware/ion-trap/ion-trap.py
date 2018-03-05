import numpy as np
from time import time
import sys
import random
import cmath
import itertools
np.random.seed(int(time()))

class QuantumComputer:
    qregister = []
    cregister = [0,0,0,0,0]
    class Gates:
        #rotation
        def R(theta, phi):
            return np.matrix([[round(np.cos(theta/ 2 )), np.around(-1j * np.exp(-1j * phi) * np.sin(theta / 2))],
                              [np.around(-1j * np.exp(-1j * phi)  * np.sin(theta / 2)), np.around(np.cos(theta/ 2 ))]])

        def XX(x):
            return np.matrix([[round(np.cos(x)), 0, 0, -1j * round(np.sin(x))],
                              [0 , round(np.cos(x)), -round(np.sin(x)), 0 ],
                              [0 , -round(np.cos(x)), -round(np.cos(x)), 0 ],
                              [ -round(np.cos(x)), 0 , 0, round(np.cos(x)) ]]

            )
        

    class Qubit:
        "Define a qubit"
        
        def __init__(self, address):
            self.address = address
            self.entangled = []
            self.state = np.matrix([[1], [0]])
            self.measurement = None 
        
        def entangle(self, qubit):
            self.entangled.append(qubit)
