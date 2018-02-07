from qiskit import QuantumProgram
import Qconfig
from qiskit.backends import local_backends
from pprint import pprint

local_backends()

backend = 'local_qasm_simulator'
qp = QuantumProgram()
qp.set_api(Qconfig.APItoken, Qconfig.config['url'])
qr = qp.create_quantum_register('qr', 2)
cr = qp.create_classical_register('cr', 2)
qc = qp.create_circuit('Bell', [qr], [cr])

qc.h(qr[0])
qc.cx(qr[0], qr[1])
qc.measure(qr[0], cr[0])
qc.measure(qr[1], cr[1])

source = qp.get_qasm('Bell')
print(source)

#result = qp.execute('Bell')


circuits = ['Bell']

qobj = qp.compile(circuits, backend)

result = qp.run(qobj, wait=2, timeout=240)


#print(result.get_counts('Bell'))

pprint(qp.available_backends())

#pprint(qp.get_backend_status('ibmqx2'))


pprint(qp.get_backend_configuration('ibmqx5'))




# quantum register for the first circuit
q1 = qp.create_quantum_register('q1', 4)
c1 = qp.create_classical_register('c1', 4)
# quantum register for the second circuit
q2 = qp.create_quantum_register('q2', 2)
c2 = qp.create_classical_register('c2', 2)
# making the first circuits
qc1 = qp.create_circuit('GHZ', [q1], [c1])
qc2 = qp.create_circuit('superpostion', [q2], [c2])
qc1.h(q1[0])
qc1.cx(q1[0], q1[1])
qc1.cx(q1[1], q1[2])
qc1.cx(q1[2], q1[3])
for i in range(4):
    qc1.measure(q1[i], c1[i])
# making the second circuits
qc2.h(q2)
for i in range(2):
    qc2.measure(q2[i], c2[i])
# printing the circuits
#print(qp.get_qasm('GHZ'))
#print(qp.get_qasm('superpostion'))
