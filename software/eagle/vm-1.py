import numpy as np
from time import time
import sys
import random
import cmath

np.random.seed(int(time()))


class Gates:
    #universal gates
    H = (1 / np.sqrt(2)) * np.matrix([[1, 1], [1, -1]])
    T = np.matrix([[1, 0], [0, cmath.exp(1j * np.pi / 4)]])
    CNOT = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]])
    
    #pauli
    X = np.matrix([[0, 1], [1, 0]])
    Y = np.matrix([[0, 0 - 1j], [0 + 1j, 0]])
    Z = np.matrix([[1, 0], [0, -1]])
    

class Qubit:
    "Define a qubit"

    def __init__(self, address):
        self.address = address
        self.entangled = []
        self.state = np.matrix([[1], [0]])
        self.measurement = None 
    
    def entangle(self, qubit):
        self.entangled.append(qubit)
   


register = []
q0 = Qubit("q0")
q1 = Qubit("q1")
q2 = Qubit("q2")


register.append(q0)
register.append(q1)

Gates = Gates()


def H(qubit):
     qubit.state = Gates.H * qubit.state

def X(qubit):
     qubit.state = Gates.X * qubit.state    

def CNOT(qubit0, qubit1):
    qubit0.entangle(qubit1.address)
    qubit1.entangle(qubit0.address)
    
   
   
    if qubit0.state.size != qubit1.state.size:
        if qubit0.state.size > qubit1.state.size:
            c = qubit1.state.copy()
            c.resize((qubit0.state.size, 1))
            qubit1.state = c
        else:
             c = qubit0.state.copy()
             c.resize((qubit1.state.size, 1))
             qubit0.state = c
            
         
    #print(qubit0.state.size)
    #print(qubit1.state.size)

    state = Gates.CNOT * np.kron(qubit0.state,qubit1.state)

    qubit0.state.resize((qubit0.state.size*2, 1))
    
    qubit0.state = state
    qubit1.state = state
    

def MEASURE(qubit):
    #print(qubit.state.size)
    sum = 0
    for x in np.nditer(qubit.state):
        sum = sum + np.square(np.abs(x))
        #print (x)
   
    assert (sum != 1.0),"Sum of probabilites does not equal"
    proba = np.square(np.abs(qubit.state[0]))
    
    if random.random() <= proba:
        qubit.state = 0
    else:
        qubit.state = 1

    print(qubit.state)

H(q0)
CNOT(q0, q1)
MEASURE(q1)
