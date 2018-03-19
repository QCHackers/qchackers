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
    comp_list = []
    counter = 0
    ket_zero = np.matrix([[1], [0]])
    ket_one = np.matrix([[0], [1]])
    class Gates:
              
        #pauli
        I = np.matrix([[1, 0], [0, 1]])
        X = np.matrix([[0, 1], [1, 0]])
        Y = np.matrix([[0, 0 - 1j], [0 + 1j, 0]])
        Z = np.matrix([[1, 0], [0, -1]])
        
        #universal gates
        H = (X + Z)/np.sqrt(2)
        T = np.matrix([[1, 0], [0, cmath.exp(1j * np.pi / 4)]])
        S = np.matrix([[1.0, 0.0], [0.0, 1.0j]])
        CNOT = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]])

        CNOT_H = np.matrix([[1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0]])

        CZ = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, -1, 0]])
        REV_CNOT = np.matrix([[0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]])

        SWAP = np.matrix([[1, 0, 0, 0],
                 [0, 0, 1, 0],
                 [0, 1, 0, 0],
                 [0, 0, 0, 1]])

    class Qubit:
        "Define a qubit"
        
        def __init__(self, address):
            self.address = address
            self.entangled = []
            self.state = np.matrix([[1], [0]])
            self.measurement = None
            self.counter = 0
        
        def entangle(self, qubit):
            self.entangled.append(qubit)
        


def wavefunction (wvf):
    perm_list = ["".join(seq) for seq in itertools.product("01", repeat=int(np.sqrt(len(wvf))))]
    #print(wvf[0,0])
    wvf_string = ""
    
    for x in range(0, len(perm_list)):
        if wvf[x,0] != 0:
            if x != len(perm_list)-1 and len(wvf) != 2:               
                wvf_string =  wvf_string + str(np.around(wvf[x,0], decimals = 2)) +  "|" + perm_list[x] + ">" + " + "
            else:             
                wvf_string =  wvf_string + str(np.around(wvf[x,0], decimals = 2)) +  "|" + perm_list[x] + ">" 
               
                
    return wvf_string



def apply_gate(qubit, wvf, gate_str):
    addr = int(qubit.address)
    
    if gate_str == "H":
        base_gate = Gates.H
    elif gate_str == "X":
        base_gate = Gates.X
    elif gate_str == "S":
        base_gate = Gates.S
    elif gate_str == "Z":
        base_gate = Gates.Z
    else:
        raise Exception("Gate not implemented")    

    if addr == 0:
        gate = np.kron(base_gate, Gates.I)        
        
    elif addr == 1:         
        gate = np.kron(Gates.I, base_gate)
         
    else:
        raise Exception ("Not implemented")

    wvf = gate * wvf 
    print(gate_str, wavefunction(wvf), "\n")
    
    return wvf

def apply_two_qubit_gate(qubit0, qubit1, wvf, gate_str):

    if gate_str == "CNOT":
        base_gate = Gates.CNOT
    else:
        raise Exception("Gate not implemented")

    addr0 = int(qubit0.address)
    addr1 = int(qubit1.address)

    if addr0 == 0 and addr1 == 1:
        wvf = Gates.CNOT * wvf
   

    print("CNOT", wavefunction(wvf), "\n")
        
    return wvf



def pr(qubit, wvf, basis):
    
    wvf_bra = wvf.getH()
    addr = int(qubit.address)
    wvf_len = len(wvf)
    
    if basis == 0:
        proj = np.outer(QC.ket_zero , QC.ket_zero)
    else:
        proj = np.outer(QC.ket_one , QC.ket_one)

        
    if wvf_len != 2 :
        if addr == 0:
            proj = np.kron(proj, Gates.I)
        elif addr == 1:
            proj = np.kron(Gates.I, proj)
        else:
            raise Exeception ("Not implemented")
        
        
    ket = proj * wvf
    answer = wvf_bra * ket

    return answer[0,0]


def MEASURE(qubit, wvf):
    
    print("MEASURE", wavefunction(qubit.state), "\n")

    pr_zero = pr(qubit, wvf, 0)
    pr_one = pr(qubit, wvf, 1)
    
    sum =  + pr_zero + pr_one    
    assert (round(sum) == 1.0),"Sum of probabilites does not equal"   

    perm_list = ["".join(seq) for seq in itertools.product("01", repeat=int(np.sqrt(len(qubit.state))))]
    
    if random.random() <= pr_zero: 
        
        print("MEASUREMENT of qubit", qubit.address, "is" , 0, "\n")
    else:
         print("MEASUREMENT of qubit", qubit.address, "is" , 1, "\n")
        
                
    return QC.cregister[int(qubit.address)]

def init():
    q0 = QuantumComputer.Qubit("0")
    q1 = QuantumComputer.Qubit("1")
    q2 = QuantumComputer.Qubit("2")
    q3 = QuantumComputer.Qubit("3")

   

    QC.qregister.append(q0)
    QC.qregister.append(q1)
    QC.qregister.append(q2)
    QC.qregister.append(q3)

    QC.qregister[0].state =  np.matrix([[1], [0]])
    QC.qregister[1].state =  np.matrix([[1], [0]])
    QC.qregister[2].state =  np.matrix([[1], [0]])
    QC.qregister[3].state =  np.matrix([[1], [0]])

    
def evaluate(filepath):
    
    with open(filepath) as fp:
        for line in fp:
            args = line.split()
            nArgs = len(args)
            reset = 0
            if nArgs == 1:
                if args[0] == "init()":
                    reset = 1
            if nArgs == 2:
                operator = args[0]
                qubit = int(args[1])
            elif nArgs == 3:
                 operator = args[0]
                 qubit = int(args[1])
                 qubit1 = int(args[2])
                 
            if reset:
                init()
            elif(operator == "RX"):
                RX(QC.qregister[int(qubit)])

            elif(operator == "RY"):
                RY(QC.qregister[int(qubit)])

            elif(operator == "RZ"):
                 RZ(QC.qregister[int(qubit)])

            elif(operator == "I"):
                 I(QC.qregister[int(qubit)])
                
            elif(operator == "X"):
                X(QC.qregister[int(qubit)])

            elif (operator == "Y"):
                Y(QC.qregister[int(qubit)])

            elif (operator == "Z"):
                Z(QC.qregister[int(qubit)])

            elif (operator == "H"):
                H(QC.qregister[int(qubit)])

            elif (operator == "T"):
                T(QC.qregister[int(qubit)])
                
            elif (operator == "S"):
                S(QC.qregister[int(qubit)])
                
            elif (operator == "CNOT"):
                CNOT(QC.qregister[int(qubit)], QC.qregister[int(qubit1)])
                
            elif (operator == "MEASURE"):
                MEASURE(QC.qregister[int(qubit)])               
                
            else:
                print("Your operator does not exist")

  
    
if __name__ == "__main__":
    QC = QuantumComputer()
    Gates = QC.Gates()
    #init()
    #evaluate(sys.argv[1])
    q0 = QuantumComputer.Qubit("0")
    q1 = QuantumComputer.Qubit("1")
    q2 = QuantumComputer.Qubit("2")
    q3 = QuantumComputer.Qubit("3")
    state_zero = np.matrix([[1], [0]])
   
    wv= np.kron(state_zero, state_zero)
    
    wv = apply_gate(q0, wv, "H")
    wv = apply_gate(q1, wv, "X")
    wv = apply_gate(q1,wv, "H")
    #wv = apply_gate(q1, wv, "X")
    wv = apply_two_qubit_gate(q0,q1, wv, "CNOT")
    wv = apply_gate(q0, wv, "H")
    wv = apply_gate(q1,wv, "H")
    MEASURE(q0, wv)
    MEASURE(q1, wv)
    

    
    
