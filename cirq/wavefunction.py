import cirq

Q1 = cirq.GridQubit(0, 0)

circuit = cirq.Circuit.from_ops(cirq.H(Q1))
simulator = cirq.google.XmonSimulator()
result = simulator.simulate(circuit)

print(simulator.wavefunction(result))
