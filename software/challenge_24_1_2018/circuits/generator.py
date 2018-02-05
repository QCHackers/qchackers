# -*- coding: utf-8 -*-

# Copyright 2017 IBM RESEARCH. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# =============================================================================

"""
Generate randomized circuits for Quantum Volume analysis.
Example run:
  python RandomCircuits.py -n 5 -d 5
"""

import sys
import math
import time
import argparse
import numpy as np
from scipy.linalg import qr
from qiskit import QuantumProgram
from qiskit.mapper import two_qubit_kak

if sys.version_info < (3,0):
    raise Exception("Please use Python version 3 or greater.")
 
def random_unitary(n):
    """Return an n x n Haar distributed unitary matrix.
    Return numpy array.
    """
    X = (1.0/math.sqrt(2.0))*(np.random.randn(n, n) +
                              1j*np.random.randn(n, n))
    Q, R = qr(X)
    R = np.diag(np.diag(R)/np.abs(np.diag(R)))
    U = np.dot(Q, R)
    return U

def build_model_circuits(name, n, depth, num_circ=1):
    """Create a quantum program containing model circuits.
    The model circuits consist of layers of Haar random
    elements of SU(4) applied between corresponding pairs
    of qubits in a random bipartition.
    name = leading name of circuits
    n = number of qubits
    depth = ideal depth of each model circuit (over SU(4))
    num_circ = number of model circuits to construct
    Return a quantum program.
    """
    qp = QuantumProgram()
    q = qp.create_quantum_register("q", n)
    c = qp.create_classical_register("c", n)
    # Create measurement subcircuit
    meas = qp.create_circuit("meas", [q], [c])
    for j in range(n):
        meas.measure(q[j], c[j])   
    # For each sample number, build the model circuits
    for i in range(num_circ):
        # Initialize empty circuit Ci without measurement
        circuit_i = qp.create_circuit("%s_%d" % (name, i), [q], [c])
        # For each layer
        for j in range(depth):
            # Generate uniformly random permutation Pj of [0...n-1]
            perm = np.random.permutation(n)
            # For each pair p in Pj, generate Haar random SU(4)
            # Decompose each SU(4) into CNOT + SU(2) and add to Ci
            for k in range(math.floor(n/2)):
                qubits = [int(perm[2*k]), int(perm[2*k+1])]
                U = random_unitary(4)
                for gate in two_qubit_kak(U):
                    i0 = qubits[gate["args"][0]]
                    if gate["name"] == "cx":
                        i1 = qubits[gate["args"][1]]
                        circuit_i.cx(q[i0], q[i1])
                    elif gate["name"] == "u1":
                        circuit_i.u1(gate["params"][2], q[i0])
                    elif gate["name"] == "u2":
                        circuit_i.u2(gate["params"][1], gate["params"][2],
                                     q[i0])
                    elif gate["name"] == "u3":
                        circuit_i.u3(gate["params"][0], gate["params"][1],
                                     gate["params"][2], q[i0])
                    elif gate["name"] == "id":
                        pass  # do nothing
        circuit_i.barrier(q)  # barrier before measurement - currently needed for determining the permutation
        # Create circuit with final measurement
        qp.add_circuit("%s_%d_meas" % (name, i), circuit_i + meas)
    return qp

def main():
  parser = argparse.ArgumentParser(
      description="Create randomized circuits for quantum volume analysis.")
  parser.add_argument('--name', default='random', help='circuit name')  
  parser.add_argument('-n', '--qubits', default=5, type=int, help='number of circuit qubits')
  parser.add_argument('-d', '--depth', default=5, type=int, help='SU(4) circuit depth')
  parser.add_argument('--num-circ', default=1, type=int, help='how many circuits?')    
  args = parser.parse_args()

  qp = build_model_circuits(name=args.name, n=args.qubits, depth=args.depth, num_circ=args.num_circ)

  for i in range(args.num_circ):
    circuit_name = args.name+str(i)+'_n'+str(args.qubits)+'_d'+str(args.depth)
    f = open(circuit_name+'.qasm', 'w')
    f.write(qp.get_qasm(args.name+'_'+str(i)+'_meas'))
    f.close()

if __name__ == "__main__":
  main()
