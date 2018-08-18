import cirq

# Pick a qubit.
qubit = cirq.GridQubit(0, 0)
prag = ""
fname = "hello.cirq"


prog = """
    cirq.X(qubit),  # Square root of NOT.
    cirq.measure(qubit, key=\'m\')  # Measurement.
"""


with open(fname) as f:
    for line in f:
        prag += line

print(prag)


begin = "cirq.Circuit.from_ops({})".format(prag)

# print(begin)
# Create a circu
circuit = eval(begin)

simulator = cirq.google.XmonSimulator()
result = simulator.run(circuit, repetitions=20)
print("Results:")
print(result)


#print (circuit.to_qasm())
