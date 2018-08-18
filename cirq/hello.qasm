OPENQASM 2.0
include "qelib1.inc"


// Qubits: [(0, 0)]
qreg q[1]
creg m_m[1]


x q[0]
measure q[0] -> m_m[0]
