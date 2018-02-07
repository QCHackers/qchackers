import numpy as np
from qiskit import QuantumCircuit, QuantumProgram
import Qconfig


import qiskit.tools.qcvv.tomography as tomography

from qiskit.tools.visualization import plot_state, plot_histogram
from qiskit.tools.qi.qi import state_fidelity, concurrence, purity, outer

QProgram = QuantumProgram() 
QProgram.set_api(Qconfig.APItoken, Qconfig.config['url'])

qr = QProgram.create_quantum_register('qr',2)
cr = QProgram.create_classical_register('cr',2)

bell = QProgram.create_circuit('bell', [qr], [cr])
bell.h(qr[0])
bell.cx(qr[0], qr[1])

bell_result = QProgram.execute(['bell'], backend = 'local_qasm_simulator', shots = 1)
bell_psi = bell_result.get_data('bell') ['quantum_state']
bell_rho = outer(bell_psi)

plot_state(bell_rho, 'paulivec')
