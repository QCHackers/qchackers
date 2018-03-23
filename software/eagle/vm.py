import numpy as np
from time import time
import sys
import random
import cmath
import itertools
import re
np.random.seed(int(time()))

class Qubit:
    "Define a qubit"
    def __init__(self, address):
        self.address = address
        self.measurement = None

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

    #miscellaneous
    CZ = np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, -1, 0]])
    SWAP = np.matrix([[1, 0, 0, 0],
                      [0, 0, 1, 0],
                      [0, 1, 0, 0],
                      [0, 0, 0, 1]]) 

class QuantumComputer:
    qregister = []
    cregister = [0,0,0,0,0]
    qregister = []
    applied_gates = []
    ket_zero = np.matrix([[1], [0]])
    ket_one = np.matrix([[0], [1]])
    wvf = ket_zero

    def __init__(self, size):
        for i in range(size):
            self.qregister.append(Qubit(str(i)))

            if size != 1:
                self.wvf= np.kron(self.wvf, self.ket_zero)      
    
       
            

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
    addr = qubit.address
    pr_zero = pr(qubit, wvf, 0)
    pr_one = pr(qubit, wvf, 1)
    
    sum =  + pr_zero + pr_one    
    assert (round(sum) == 1.0),"Sum of probabilites does not equal 1"

    #print("MEASURE", wavefunction(wvf), "\n")
    
    if random.random() <= pr_zero:
        wvf = (proj(qubit, 0, wvf) * wvf)/(np.sqrt(pr_zero))
        qubit.measurement = 0
        print("MEASUREMENT of qubit", addr, "is" , 0, "\n")
    else:
        wvf = (proj(qubit, 1, wvf) * wvf)/(np.sqrt(pr_one))
        qubit.measurement = 1
        print("MEASUREMENT of qubit", addr, "is" , 1, "\n")       
                
    return wvf


def evaluate(filepath):
    global wv
    
    with open(filepath) as fp:
        for line in fp:
            args = line.split()
            nArgs = len(args)
            operator = args[0]
        
            if nArgs == 2:
                qubit = int(args[1])
                if (operator == "MEASURE"):
                    MEASURE(QC.qregister[int(qubit)], wv)
                else:
                    wv= apply_gate(QC.qregister[int(qubit)], wv, operator, "MISSING")
            elif nArgs == 3:
                operator = args[0]
                qubit = int(args[1])
                qubit1 = int(args[2])
                wv= apply_gate(QC.qregister[int(qubit)], wv, operator, QC.qregister[int(qubit1)])
                
            else:
                raise Exception("Exit(1)")
            
    
if __name__ == "__main__":
    QC = QuantumComputer(4)
    Gates = Gates()
    wv = QC.wvf
    evaluate(sys.argv[1])
    
    
