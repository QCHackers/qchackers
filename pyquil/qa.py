import numpy as np
from grove.pyqaoa.maxcut_qaoa import maxcut_qaoa
import pyquil.api as api
qvm_connection = api.QVMConnection()


square_ring = [(0, 1), (1, 2), (2, 3), (3, 0)]

steps = 2
inst = maxcut_qaoa(graph=square_ring, steps=steps)
betas, gammas = inst.get_angles()

t = np.hstack((betas, gammas))
param_prog = inst.get_parameterized_program()
prog = param_prog(t)
wf = qvm_connection.wavefunction(prog)
wf = wf.amplitudes

for state_index in range(inst.nstates):
    print(inst.states[state_index], np.conj(wf[state_index])*wf[state_index])
