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

    SWAP = np.array([[1, 0, 0, 0],
                     [0, 0, 1, 0],
                     [0, 1, 0, 0],
                     [0, 0, 0, 1]])
    
    #pauli
    I = np.matrix([[1, 0], [0, 1]])
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
        

q0 = Qubit("q0")
q1 = Qubit("q1")
q2 = Qubit("q2")
q3 = Qubit("q3")

Gates = Gates()

def H(qubit):
    length = len(qubit.state)
    if length != 2:
        for x in range(0, int(np.sqrt(length))-1):
            Gates.H = np.kron(Gates.H, Gates.I)
            
    qubit.state = Gates.H * qubit.state
    return qubit.state

def T(qubit):
    length = len(qubit.state)
    if length != 2:
        for x in range(0, int(np.sqrt(length))-1):
            Gates.T = np.kron(Gates.T, Gates.I)
            
    qubit.state = Gates.T * qubit.state
    return qubit.state

def I(qubit):
    length = len(qubit.state)
    if length != 2:
        for x in range(0, int(np.sqrt(length))-1):
            Gates.I = np.kron(Gates.I, Gates.I)
            
    qubit.state = Gates.I * qubit.state
    
    return qubit.state

def X(qubit):
    length = len(qubit.state)
    if length != 2:
        for x in range(0, int(np.sqrt(length))-1):
            Gates.X = np.kron(Gates.X, Gates.I)
            
    qubit.state = Gates.X * qubit.state
    
    return qubit.state

def Y(qubit):
    length = len(qubit.state)
    if length != 2:
        for x in range(0, int(np.sqrt(length))-1):
            Gates.Y = np.kron(Gates.Y, Gates.I)
            
    qubit.state = Gates.Y * qubit.state
    
    return qubit.state

def Z(qubit):
    length = len(qubit.state)
    if length != 2:
        for x in range(0, int(np.sqrt(length))-1):
            Gates.Z = np.kron(Gates.Z, Gates.I)
            
    qubit.state = Gates.Z * qubit.state
    
    return qubit.state

def CNOT(qubit0, qubit1):
    
    qubit0.entangle(qubit1)
    qubit1.entangle(qubit0)
    length1 = len(qubit1.state)
    length = len(qubit0.state)
    
    if length != 2:
        for x in range(0, int(np.sqrt(length1))):
            Gates.CNOT = np.kron(Gates.CNOT, Gates.I)
            
    state =  Gates.CNOT * np.kron(qubit0.state,qubit1.state)
    
    for x in qubit0.entangled:
        x.state = state
    for x in qubit1.entangled:
        x.state = state
        
    return state

def MEASURE(qubit):
    sum = 0
    print("qubit state", qubit.state)
    half_state = 0
    for x in np.nditer(qubit.state):
        sum = sum + np.square(np.abs(x))
        
    assert (round(sum) == 1.0),"Sum of probabilites does not equal"     
    for i in range(int(len(qubit.state)/2)):        
         half_state = half_state + np.square(np.abs(qubit.state.item(i)))
         
    proba = np.square(np.abs(half_state))
    if random.random() <= proba:
       return 0
    else:
        return 1

print("T", Z(q3), "\n")
print("H", H(q0), "\n")
print("CNOT", CNOT(q0, q1), "\n")
print("CNOT", CNOT(q1, q2), "\n")
#print("H", H(q0, QC.state), "\n")
#print("H", H(q0, QC.state), "\n")
print(MEASURE(q3), "\n")
#print(MEASURE_1(q1), "\n")



                      
    
    
    
 
