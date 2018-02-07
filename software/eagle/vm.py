import numpy as np
from time import time
import sys
import random
import cmath

filepath =  sys.argv[1]


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
            
         
    print(qubit0.state.size)
    print(qubit1.state.size)

    state = Gates.CNOT * np.kron(qubit0.state,qubit1.state)

    qubit0.state.resize((qubit0.state.size*2, 1))
    
    qubit0.state = state
    qubit1.state = state
    

def MEASURE(qubit):
    #print(qubit.state.size)
    sum = 0
    for x in np.nditer(qubit.state):
        sum = sum + np.square(np.abs(x))
        print (x)
   
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

state_zero = np.matrix([[1] , [0]])
state_hadamard = np.matrix([[1/np.sqrt(2)], [1/np.sqrt(2)]])
state_one = np.array([[0] , [1]])

qregister = [state_zero, state_zero, state_zero, state_zero, state_zero]
cregister = [0, 0, 0, 0, 0]

#primitive gates
x = np.matrix ([[0,  1], [1,0]])
y = np.array ([[0,  0-1j], [0+1j,0]])
z = np.array ([[1,  0], [0,-1]])
cnot = np.array ([[1,  0, 0, 0], [0 ,1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]])
cnot_gate=np.matrix('1 0 0 0; 0 1 0 0; 0 0 0 1; 0 0 1 0')
entangled_qubits = []
#print ("THE APPENED ", np.split(np.append(state_hadamard, state_one, axis =0), 2))



#np.split(cnot_gate * np.append(qregister[qubit], qregister[qubit2], axis=0), 2)


zero_zero = np.array([[1],[0],[1],[0]])
zero_one = np.array([[1],[0],[0],[1]])
one_zero = np.array([[0],[1],[1],[0]])
one_one = np.array([[0],[1],[0],[1]])


#derived gates
h = (x + z)/np.sqrt(2)

        

def measure (qubit, bit_address, qindex):
    #check if it is entangled, if it is, measure the state of the qubit that it is etangled with
    #print ("Entangled qubits ", entangled_qubits)
    lst2 = [item[0] for item in entangled_qubits]   

    

    if (qindex in lst2):
        #print ("This qubit is entangled")
        control = entangled_qubits[0]
        application_qubit = control[1]
        target = qubit
        #print("TARGET ", application_qubit );
        c = np.absolute(application_qubit[0,0])**2
        d = np.absolute(application_qubit[1,0])**2


        if(1 - (c + d)  < 0.000001):
            bit = np.random.choice([0,1], p=[c,d])
           # print("THE BIT" ,bit)
            if(bit == 1):                           
                #print ("APPLICATION QUBIT BEFORE FLIP ", target)
                target = np.array([[target[1,0]], [target[0,0]]])               
                e = np.absolute(target[0,0])**2                
                f = np.absolute(target[1,0])**2
                

                if(1 - (e + f)  < 0.000001):
                    cregister[bit_address] = np.random.choice([0,1], p=[e,f])
                    #print("Register result CNOT ", cregister[bit_address])

            else:

           
                e = np.absolute(target[0,0])**2
                
                f = np.absolute(target[1,0])**2
               

                if(1 - (e + f)  < 0.000001):
                    cregister[bit_address] = np.random.choice([0,1], p=[e,f])
                    #print("Register result CNOT ", cregister[bit_address])
                
            

        else:

            print("Probabilities must be equal to 1, but they are equal to {}".format(c + d))
    else:    
        a = np.absolute(qubit[0,0])**2
        b = np.absolute(qubit[1,0])**2

        if(1 - (a + b)  < 0.000001):
            cregister[bit_address] = np.random.choice([0,1], p=[a,b])
           

        else:

            print("Probabilities must be equal to 1, but they are equal to {}".format(a + b))


#print("cregister ", cregister)
def print_lines ():
    global entangled_qubits
    with open(filepath) as fp:
        cnt = 1
        for line in fp:
            args = line.split()
            nArgs = len(args)

            if nArgs >= 2:
                operator = args[0]
                qubit = int(args[1])
            if nArgs >= 3:
                if args[2][0] == "[" and args[2][-1] == "]":
                    bit = int(args[2][1:-1])
                else:
                    qubit2 = int(args[2])

            #print("Line {}: {}".format(cnt, line.strip()))

            if(operator == "X"):
                #print("The operator is X")
                qregister[qubit] = x * qregister[qubit]
            elif (operator == "Y"):
                #print("The operator is Y")
                qregister[qubit] = y * qregister[qubit]

            elif (operator == "H"):
                #print("The operator is H")
                qregister[qubit] = h * qregister[qubit]

            elif (operator == "Z"):
                #print("The operator is Z")
                qregister[qubit] = z * qregister[qubit]
            elif (operator == "CNOT"):

                print(qubit2)
                print(cnot * np.kron(qregister[qubit],qregister[qubit2]))
                entangled_qubits = [[qubit, qregister[qubit]], [qubit2, qregister[qubit2]]]
               
                #qregister[qubit], qregister[qubit2] = np.split(cnot_gate * np.append(qregister[qubit], qregister[qubit2], axis=0), 2)
                #print("Appened qubit ", np.append(qregister[qubit], qregister[qubit2]))
               
            elif (operator == "MEASURE"):
                #print()
                measure(qregister[qubit], bit, qubit)
                #print("The operator is MEASURE")
            else:
                print("Your operator does not exist")
            cnt += 1


print_lines()
print()

#print("ENTANGLED QUBITS FOLKS ", entangled_qubits)

#print(state_zero * x)
print("Quantum Register:")
for i in range(len(qregister)):
    print("Qubit {}: ({}, {})".format(i, qregister[i][0, 0], qregister[i][1, 0]))

print()
print("Classical Register:")
for i in range(len(cregister)):
    print("Bit {}: {}".format(i, cregister[i]))
