# Import your solution function
from challenge_submission import compiler_function

# Import submission evaluation and scoring functions
from challenge_evaluation import evaluate, score

# Possibly useful other helper function
from challenge_evaluation import qasm_to_dag_circuit, load_coupling, get_layout

# Select the simulation backend to calculate the quantum states resulting from the circuits
# On Windows platform the C++ Simulator is not yet available with pip install
backend = 'local_projectq_simulator'
#backend = 'local_qasm_simulator'     # uncomment this line if you are a Windows user

# Load example circuits and coupling maps

ex_nr = 1 # examples to add per qubit number. maximum is 10 with the provided circuits
test_circuit_filenames = {}

for i in range(ex_nr):
    test_circuit_filenames['circuits/random%d_n5_d5.qasm' % i] = get_layout(5)
for i in range(ex_nr):
    test_circuit_filenames['circuits/random%d_n16_d16.qasm' % i] = get_layout(16)
for i in range(ex_nr):
    test_circuit_filenames['circuits/random%d_n20_d20.qasm' % i] = get_layout(20)

# store circuit, coupling map pairs in test_circuits. Circuits are in qasm form.
test_circuits = {}
for filename, cmap in test_circuit_filenames.items():
    with open(filename, 'r') as infile:
        qasm = infile.read()        
        test_circuits[filename] = {"qasm": qasm, "coupling_map": cmap}

#result = evaluate(compiler_function, test_circuits, verbose=True, backend = backend)

#test_circuits['circuits/random0_n5_d5.qasm'].keys()

myres = score(compiler_function, backend = backend)
print("Your compiler scored %6.5f x better \
and was %6.5f x faster than the QISKit reference compiler." % myres)
