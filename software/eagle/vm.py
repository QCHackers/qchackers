import numpy as np
from time import time
import sys
import random
import cmath
import itertools

np.random.seed(int(time()))
filepath =  sys.argv[1]

class QuantumComputer:
    class Gates:
        #rotation
        def RX(theta):
            return np.matrix([[round(np.cos(theta/ 2 )), np.around(-1j * np.sin(theta / 2))],
                              [np.around(-1j * np.sin(theta / 2)), np.around(np.cos(theta/ 2 ))]])

        def RY(theta):
            return np.matrix([[np.cos(theta/ 2 ),  np.sin(theta / 2)],
                              [-1j * np.sin(theta / 2), np.cos(theta/ 2 )]])

        def RZ(theta):
            return np.matrix([[np.exp(-1j * theta / 2),  0],
                              [0, np.exp(1j * theta / 2)]])
        
        #pauli
        I = np.matrix([[1, 0], [0, 1]])
        X = np.matrix([[0, 1], [1, 0]])
        Y = np.matrix([[0, 0 - 1j], [0 + 1j, 0]])
        Z = np.matrix([[1, 0], [0, -1]])
        
        #universal gates
        H = (X + Z)/np.sqrt(2)
        T = np.matrix([[1, 0], [0, cmath.exp(1j * np.pi / 4)]])
        CNOT = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]])

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
cregister = [0,0,0,0,0]
q0 = Qubit("0")
q1 = Qubit("1")
q2 = Qubit("2")
q3 = Qubit("3")

register.append(q0)
register.append(q1)
register.append(q2)
register.append(q3)
Gates = QuantumComputer.Gates()

def H(qubit):
    state_length = len(qubit.state)
    gate = Gates.H
    qubit.entangle(qubit)
    
    if state_length != 2:        
        for x in range(0, int(np.sqrt(state_length))-1):
            gate = np.kron(gate, Gates.I)
        state = gate * qubit.state
    else:
        state = Gates.H * qubit.state

    for x in qubit.entangled:
        x.state = state

    #print("H", state, "\n")
    
    return state

def T(qubit):
    state_length = len(qubit.state)
    gate = np.matrix([[1, 0], [0, 1]])
    
    for x in range(0, int(np.sqrt(state_length))-1):
        gate = np.kron(Gates.T, Gates.I)
            
    qubit.state = gate * qubit.state

    print("T", state, "\n")
    
    return qubit.state

def I(qubit):
    state_length = len(qubit.state)
    gate = np.matrix([[1, 0], [0, 1]])
    
    for x in range(0, int(np.sqrt(state_length))-1):
        gate = np.kron(Gates.I, Gates.I)
            
    qubit.state = gate * qubit.state
    return qubit.state

def X(qubit):
    state_length = len(qubit.state)
    gate = Gates.X
    qubit.entangle(qubit)
    
    if state_length != 2:        
        for x in range(0, int(np.sqrt(state_length))-1):
            gate = np.kron(gate, Gates.I)
        state = gate * qubit.state
    else:
        state = Gates.X * qubit.state

    for x in qubit.entangled:
        x.state = state

    #print("X", state, "\n")
    
    return state

def Y(qubit):
    gate_length = len(qubit.state)
    gate = np.matrix([[1, 0], [0, 1]])
    
    for x in range(0, int(np.sqrt(gate_length))-1):
        gate = np.kron(Gates.Y, Gates.I)
            
    qubit.state = gate * qubit.state
    
    return qubit.state

def Z(qubit):
    length = len(qubit.state)
    gate = np.matrix([[1, 0], [0, 1]])
    
    for x in range(0, int(np.sqrt(length))-1):
        gate = np.kron(Gates.Z, Gates.I)
            
    qubit.state = gate * qubit.state
    
    return qubit.state

def get_addr(qubit):
    return register[int(qubit)]

def CNOT(qubit0, qubit1):
    gate = np.matrix([[1, 0], [0, 1]])
    length1 = len(qubit1.state)
    length = len(qubit0.state)

    if qubit1 not in qubit0.entangled:
        #print("Not entangled")        
        qubit0.entangle(qubit1)
        qubit1.entangle(qubit0)
        if length != 2:
            for x in range(0, int(np.sqrt(length))):
                gate = np.kron(Gates.CNOT, Gates.I)
            state =  gate * np.kron(qubit0.state,qubit1.state)
        else:
            state =  Gates.CNOT *  np.kron(qubit0.state,qubit1.state)
            

    else:
        state =  Gates.CNOT * qubit0.state
    
    for x in qubit0.entangled:
        x.state = state
    for x in qubit1.entangled:
        x.state = state

    #print("CNOT", qubit0.state, qubit1.state, "\n")
        
    return state

def MEASURE(qubit):
    sum = 0
    #print("Wavefunction", qubit.state, "\n")
    half_state = 0
    for x in np.nditer(qubit.state):
        sum = sum + np.square(np.abs(x))
        
    assert (round(sum) == 1.0),"Sum of probabilites does not equal"     
    for i in range(int(len(qubit.state)/2)):        
        half_state = half_state + np.square(np.abs(qubit.state.item(i)))

    perm_list = ["".join(seq) for seq in itertools.product("01", repeat=int(np.sqrt(len(qubit.state))))]

    for f, x in zip(perm_list, qubit.state):
        proba = np.square(np.abs(x.item(i-1)))
        j = 0
        
        if random.random() <= proba:
            
            for c in f:
                cregister[j] = int(c)
                j = j + 1
                
    return cregister[int(qubit.address)]
    
     
def evaluate():
    
    with open(filepath) as fp:
        for line in fp:
            args = line.split()
            nArgs = len(args)
            
            if nArgs == 2:
                operator = args[0]
                qubit = int(args[1])
            elif nArgs == 3:
                 operator = args[0]
                 qubit = int(args[1])
                 qubit1 = int(args[2])
                
               
            if(operator == "RX"):
                RX(register[int(qubit)])

            elif(operator == "RY"):
                RY(register[int(qubit)])

            elif(operator == "RZ"):
                 RZ(register[int(qubit)])

            elif(operator == "I"):
                 I(register[int(qubit)])
                
            elif(operator == "X"):
                X(register[int(qubit)])

            elif (operator == "Y"):
                Y(register[int(qubit)])

            elif (operator == "Z"):
                Z(register[int(qubit)])

            elif (operator == "H"):
                H(register[int(qubit)])
                
            elif (operator == "T"):
                T(register[int(qubit)])
                
            elif (operator == "CNOT"):
                CNOT(register[int(qubit)], register[int(qubit1)])
                
            elif (operator == "MEASURE"):
                print("MEASUREMENT of qubit", qubit, "is" , MEASURE(register[int(qubit)]), "\n")           
                
            else:
                print("Your operator does not exist")


if __name__ == "__main__":
    evaluate()    
