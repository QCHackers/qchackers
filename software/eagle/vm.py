import numpy as np
from time import time
import sys
import random
import cmath
import itertools
import re
np.random.seed(int(time()))


class QuantumComputer:
    qregister = []
    cregister = [0,0,0,0,0]
    applied_gates = []
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
        CZ = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, -1, 0]])

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
            

def two_n_size(wvf):
    return  int(np.log(len(wvf))/np.log(2))

def wavefunction(wvf):    
    perm_list = ["".join(seq) for seq in itertools.product("01", repeat=int(np.sqrt(len(wvf))))]
    wvf_string = ""    
    ap_gate_len = len(QC.applied_gates)
    
    for x in range(0, len(perm_list)):
        if wvf[x,0] != 0:
            if x != len(perm_list)-1 and len(wvf) != 2:               
                wvf_string =  wvf_string + str(np.around(wvf[x,0], decimals = 2)) +  "|" + perm_list[x][:ap_gate_len] + ">" + " + "           
            else:             
                wvf_string =  wvf_string + str(np.around(wvf[x,0], decimals = 2)) +  "|" + perm_list[x] + ">"
                
    wvf_string = re.split(r'(\s+)', wvf_string)[:-3]
    wvf_string = "".join(wvf_string)               
                
    return wvf_string


def i_gen(num):
    gates = Gates.I

    if num != 0:
        for x in range(1, num):
            gates = np.kron(Gates.I, gates)

    return gates


def build_gate(addr, wvf_size, x, spacing_num):    
    if spacing_num == wvf_size -2:
        gate = np.kron(x, i_gen(spacing_num))        
    elif spacing_num == 0:
        gate = np.kron(i_gen(addr), x)        
    elif spacing_num == -1:
        gate = np.kron(i_gen(addr-1), x_last)        
    else:       
        gate = np.kron(i_gen(addr), x)
        gate = np.kron(gate, i_gen(spacing_num))
        
    return gate

def append_gate(qubit, qubit1):
    if qubit.address not in QC.applied_gates:
        QC.applied_gates.append(qubit.address)

    if qubit1 != "MISSING" and qubit1.address not in QC.applied_gates:
        QC.applied_gates.append(qubit1.address)
    

def get_base_gate(gate_str):
    if gate_str == "H":
        base_gate = Gates.H
    elif gate_str == "X":
        base_gate = Gates.X
    elif gate_str == "S":
        base_gate = Gates.S
    elif gate_str == "Z":
        base_gate = Gates.Z
    elif gate_str == "CNOT":
         base_gate = Gates.CNOT
    else:
        raise Exception("Gate not implemented")
    return base_gate
    
    
def apply_gate(qubit, wvf, gate_str, qubit1 = "MISSING"):
    addr = int(qubit.address)
    wvf_size = two_n_size(wvf)
    spacing_num = wvf_size - 2 - addr
    
    base_gate = get_base_gate(gate_str)

    if qubit1 == "MISSING":
        x = np.kron(base_gate, Gates.I)
        x_last = np.kron(Gates.I, base_gate)
    else:
        x = base_gate

    if spacing_num == -1:
        x = x_last
        
    gate = build_gate(addr, wvf_size, x, spacing_num)    

    append_gate(qubit, qubit1)
    
    wvf = gate * wvf        
    #print(gate_str, wavefunction(wvf), "\n")
    
    return wvf


def proj(qubit,basis, wvf):
    addr = int(qubit.address)
    wvf_size = two_n_size(wvf) - 1
    
    if basis == 0:
        proj = np.outer(QC.ket_zero , QC.ket_zero)
    else:
        proj = np.outer(QC.ket_one , QC.ket_one)

        
    if addr == 0:
        proj = np.kron(proj, i_gen(wvf_size))
    elif addr == wvf_size:
         proj = np.kron(i_gen(addr), proj)         
    else:        
        proj = np.kron(i_gen(addr), proj)
        proj = np.kron(proj, i_gen(wvf_size - addr))

    return proj
    


def pr(qubit, wvf, basis):    
    wvf_bra = wvf.getH()       
    ket = proj(qubit, basis, wvf) * wvf    
    answer = wvf_bra * ket
    
    return answer[0,0]


def MEASURE(qubit, wvf):    
    print("MEASURE", wavefunction(wvf), "\n")

    pr_zero = pr(qubit, wvf, 0)
    pr_one = pr(qubit, wvf, 1)
    
    sum =  + pr_zero + pr_one    
    assert (round(sum) == 1.0),"Sum of probabilites does not equal"   

    perm_list = ["".join(seq) for seq in itertools.product("01", repeat=int(np.sqrt(len(qubit.state))))]
    
    if random.random() <= pr_zero:
        wvf = (proj(qubit, 0, wvf) * wvf)/(np.sqrt(pr_zero))
        print("MEASUREMENT of qubit", qubit.address, "is" , 0, "\n")
    else:
        wvf = (proj(qubit, 1, wvf) * wvf)/(np.sqrt(pr_one))
        print("MEASUREMENT of qubit", qubit.address, "is" , 1, "\n")       
                
    return wvf


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
    wv = np.kron (wv, state_zero)
    wv = np.kron (wv, state_zero)

    #setup
    wv = apply_gate(q1, wv, "X")
    wv = apply_gate(q0, wv, "H")    
    wv = apply_gate(q1, wv, "H")

    #Oracle
    #wv = apply_gate(q0, wv, "CNOT", q1)
    #wv = apply_gate(q1, wv, "X")
    
    #Finishing lap    
    wv = apply_gate(q0, wv, "H")
    wv = apply_gate(q1,wv, "H")
    wv = MEASURE(q0, wv)
    wv = MEASURE(q1, wv)

    
    
