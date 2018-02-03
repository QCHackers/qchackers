from qiskit import QuantumProgram
import Qconfig
from qiskit.backends import local_backends
from pprint import pprint
# Import your solution function
from challenge_submission import compiler_function

# Import submission evaluation and scoring functions
from challenge_evaluation import evaluate, score

# Possibly useful other helper function
from challenge_evaluation import qasm_to_dag_circuit, load_coupling, get_layout

# Select the simulation backend to calculate the quantum states resulting from the circuits
# On Windows platform the C++ Simulator is not yet available with pip install
qp = QuantumProgram()
qp.set_api(Qconfig.APItoken, Qconfig.config['url'])
pprint(qp.available_backends())
backend = 'local_qasm_simulator'


ex_nr = 1
test_circuit_filenames = {}


for i in range(ex_nr):
    test_circuit_filenames['circuits/random8_n5_d5.qasm'] = get_layout(5)
for i in range(ex_nr):
    test_circuit_filenames['circuits/random9_n16_d16.qasm'] = get_layout(16)
for i in range(ex_nr):
    test_circuit_filenames['circuits/random9_n20_d20.qasm'] = get_layout(20)

    test_circuits = {}

for filename, cmap in test_circuit_filenames.items():
    with open(filename, 'r') as infile:
        qasm = infile.read()        
        test_circuits[filename] = {"qasm": qasm, "coupling_map": cmap}

#result = evaluate(compiler_function, test_circuits, verbose=True, backend = backend)

myres = score(compiler_function, backend = backend)
print("Your compiler scored %6.5f x better \
and was %6.5f x faster than the QISKit reference compiler." % myres)
