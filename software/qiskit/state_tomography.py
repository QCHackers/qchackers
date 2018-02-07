from qiskit import QuantumCircuit, QuantumProgram
import Qconfig
import qiskit.tools.qcvv.tomography as tomo
import numpy as np
from qiskit.tools.visualization import plot_state, plot_histogram
from qiskit.tools.qi.qi import state_fidelity, concurrence, purity, outer

p = QuantumProgram()
p.set_api(Qconfig.APItoken, Qconfig.config['url'])

qr = p.create_quantum_register('qr', 2)
cr = p.create_classical_register('cr', 2)

# quantum circuit to make an entangled bell state 
bell = p.create_circuit('bell', [qr], [cr])
bell.h(qr[0])
bell.cx(qr[0], qr[1])


bell_result = p.execute(['bell'], backend='local_qasm_simulator', shots=1)
bell_psi = bell_result.get_data('bell')['quantum_state']
bell_rho = outer(bell_psi) # construct the density matrix from the state vector


#plot_state(bell_rho,'paulivec')

rho_mixed = np.array([[1,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,1]])/2
plot_state(rho_mixed, 'paulivec')
