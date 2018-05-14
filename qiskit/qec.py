from qiskit import QuantumProgram
import Qconfig
from qiskit.backends import local_backends
from pprint import pprint



backend = 'ibmqx2'
shots = 1000
max_credits = 5
qp = QuantumProgram()
qp.set_api(Qconfig.APItoken, Qconfig.config['url'])
pprint(qp.available_backends())
qr = qp.create_quantum_register('qr', 1)
cr = qp.create_classical_register('cr', 1)
qc = qp.create_circuit('Bell', [qr], [cr])

qc.h(qr[0])
for i in range(4):
    qc.iden(qr[0])
qc.h(qr[0])
qc.measure(qr[0], cr[0])



circuits = ['Bell']
qobj = qp.compile(circuits, backend)
#result = qp.run(qobj, wait=2, timeout=240)
#print(result.get_counts('Bell'))

result = qp.execute(circuits, backend=backend, shots=shots,
                           max_credits=max_credits,wait=20, timeout=240)
print(result)
