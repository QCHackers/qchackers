import numpy as np
from time import time
import sys
import random
import cmath
import itertools
import re
np.random.seed(int(time()))

class QuantumComputer:
    "A quantum compute with gates, qubits and registers."
    qregister = []
    cregister = [0,0,0,0,0]
    class Gates:
        "Unitary operations on qubits"
        I = np.matrix([[1, 0], [0, 1]])
        
        def RX(self, theta):
            return np.matrix([[round(np.cos(theta/ 2 )), np.around(-1j * np.sin(theta / 2))],
                              [np.around(-1j * np.sin(theta / 2)), np.around(np.cos(theta/ 2 ))]])

        def RY(self, theta):
            return np.matrix([[np.cos(theta/ 2 ),  np.sin(theta / 2)],
                              [-1j * np.sin(theta / 2), np.cos(theta/ 2 )]])

        def RZ(self, theta):
            return np.matrix([[np.exp(-1j * theta / 2),  0],
                              [0, np.exp(1j * theta / 2)]])
        
        CZ = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, -1, 0]])
        

    class Qubit:
        "Two level quantum system"
        
        def __init__(self, address):
            self.address = address
            self.entangled = []
            self.state = np.matrix([[1], [0]])
            self.measurement = None 
            
        def entangle(self, qubit):
            self.entangled.append(qubit)
            

def RX(angle, qubit):
    "Rotate the bloch sphere about X at the specified angle."
    state_length = len(qubit.state)
    gate = Gates.RX(angle)
    qubit.entangle(qubit)
    
    if state_length != 2:        
        for x in range(0, int(np.sqrt(state_length))-1):
            gate = np.kron(gate, Gates.I)
            state = gate * qubit.state
    else:
        state = Gates.RX(angle) * qubit.state

    for x in qubit.entangled:
        x.state = state

    print("RX", state, "\n")
    
    return state

def RY(angle, qubit):
    "Rotate the bloch sphere about Y at the specified angle."
    state_length = len(qubit.state)
    gate = Gates.RY(angle)
    qubit.entangle(qubit)
    
    if state_length != 2:        
        for x in range(0, int(np.sqrt(state_length))-1):
            gate = np.kron(gate, Gates.I)
            state = gate * qubit.state
    else:
        state = Gates.RY(angle) * qubit.state

    for x in qubit.entangled:
        x.state = state

    print("RY", state, "\n")
    
    return state

def RZ(angle, qubit):
    "Rotate the bloch sphere about Z at the specified angle."
    state_length = len(qubit.state)
    gate = Gates.RZ(angle)
    qubit.entangle(qubit)
    
    if state_length != 2:        
        for x in range(0, int(np.sqrt(state_length))-1):
            gate = np.kron(gate, Gates.I)
            state = gate * qubit.state
    else:
        state = Gates.RZ(angle) * qubit.state

    for x in qubit.entangled:
        x.state = state

    print("RZ", state, "\n")
    
    return state

def CZ(qubit0, qubit1):
    "Entangle 2 qubits."
    gate = np.matrix([[1, 0], [0, 1]])
    length1 = len(qubit1.state)
    length = len(qubit0.state)

    if qubit1 not in qubit0.entangled:
        #print("Not entangled")        
        qubit0.entangle(qubit1)
        qubit1.entangle(qubit0)
        if length != 2:
            for x in range(0, int(np.sqrt(length))):
                gate = np.kron(Gates.CZ, Gates.I)
                state =  gate * np.kron(qubit0.state,qubit1.state)
        else:
            state =  Gates.CZ *  np.kron(qubit0.state,qubit1.state)
            

    else:
        state =  Gates.CZ * qubit0.state
        
    for x in qubit0.entangled:
        x.state = state
    for x in qubit1.entangled:
        x.state = state

    print("CZ", state, "\n")
    
    return state

def MEASURE(qubit):
    "Collapse wavefunction"
    sum = 0
    print("Wavefunction", qubit.state, "\n")
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
                QC.cregister[j] = int(c)
                j = j + 1

    print("MEASUREMENT of qubit", qubit.address, "is" , QC.cregister[int(qubit.address)], "\n")           
    
    return QC.cregister[int(qubit.address)]


def convert_angle (angle):
    "Covert angle string to number"
    if angle == "PI":
        return np.pi

    elif angle == "PI/2":
        return np.pi/2
    else:
        print("NOTHING")

def init():
    "Initialize quantum computer"
    q0 = QuantumComputer.Qubit("0")
    q1 = QuantumComputer.Qubit("1")
    q2 = QuantumComputer.Qubit("2")
    q3 = QuantumComputer.Qubit("3")

    QC.qregister.append(q0)
    QC.qregister.append(q1)
    QC.qregister.append(q2)
    QC.qregister.append(q3)

    
def evaluate(filepath):
    "Execute instructions from file"
    
    with open(filepath) as fp:
        for line in fp:
            stripped_exp = re.sub('[\(\)\{\}<>]', '', line)
            args = stripped_exp.split()
            nArgs = len(args)
            
            if nArgs == 2:
                operator = args[0]
                qubit = int(args[1])
            elif nArgs == 3:
                operator = args[0]

                if operator == "CZ":
                    qubit = int(args[1])
                    qubit1 = int(args[2])

                else:
                    angle = args[1]
                    qubit = int(args[2])                   
                     
               
            if(operator == "RX"):
                RX(convert_angle(angle), QC.qregister[int(qubit)])

            elif(operator == "RY"):
                RY(convert_angle(angle), QC.qregister[int(qubit)])

            elif(operator == "RZ"):
                RZ(convert_angle(angle), QC.qregister[int(qubit)])

            elif (operator == "CZ"):
                CZ(QC.qregister[int(qubit)], QC.qregister[int(qubit1)])
                
            elif (operator == "MEASURE"):
                MEASURE(QC.qregister[int(qubit)])             
                
            else:
                print("Your operator does not exist")


if __name__ == "__main__":
    QC = QuantumComputer()
    Gates = QC.Gates()
    init()
    evaluate(sys.argv[1])    
