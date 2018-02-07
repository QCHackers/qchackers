
#https://github.com/QISKit/qiskit-tutorial/blob/stable/4_applications/quantum_chemistry.ipynb
# useful additional packages

'''
This is used to map out the orbital, which is important because chemists can use it to predict reaction rate among other predictions
'''
import matplotlib.pyplot as plt
import numpy as np
from scipy import linalg as la
from functools import partial

# importing the QISKit
from qiskit import QuantumProgram
import Qconfig

# import basic plot tools
from qiskit.tools.visualization import plot_histogram


# import optimization tools
from qiskit.tools.apps.optimization import trial_circuit_ryrz, SPSA_optimization, SPSA_calibration
from qiskit.tools.apps.optimization import Hamiltonian_from_file, make_Hamiltonian
from qiskit.tools.apps.optimization import eval_hamiltonian, group_paulis

# Ignore warnings due to chopping of small imaginary part of the energy 
import warnings
warnings.filterwarnings('ignore')


n=2
m=6
device='local_qasm_simulator'

initial_theta=np.random.randn(2*n*m)

entangler_map={0: [1]}
shots = 1
max_trials=100

ham_name='H2/H2Equilibrium.txt'

pauli_list = Hamiltonian_from_file(ham_name)
H=make_Hamiltonian(pauli_list)

exact =np.amin(la.eig(H)[0]).real

print(exact)

pauli_list_grouped=group_paulis(pauli_list)

Q_program = QuantumProgram()
Q_program.set_api(Qconfig.APItoken, Qconfig.config["url"])



def cost_function(Q_program, H, n, m, entangler_map, shots, device, theta):

    return eval_hamiltonian(Q_program, H, trial_circuit_ryrz(n,m,theta, entangler_map, None, False), shots, device).real


initial_c = 0.01
target_update=2*np.pi*0.1
save_step = 20

if shots ==1:
    SPSA_params=SPSA_calibration(partial(cost_function,Q_program,H,n,m,entangler_map,
                                         shots,device),initial_theta,initial_c,target_update,25)
    output=SPSA_optimization(partial(cost_function,Q_program,H,n,m,entangler_map,shots,device),
                         initial_theta,SPSA_params,max_trials,save_step,1);
else:
    SPSA_params=SPSA_calibration(partial(cost_function,Q_program,pauli_list_grouped,n,m,entangler_map,
                                         shots,device),initial_theta,initial_c,target_update,25)
    output=SPSA_optimization(partial(cost_function,Q_program,pauli_list_grouped,n,m,entangler_map,shots,device),
                         initial_theta,SPSA_params,max_trials,save_step,1);


plt.plot(np.arange(0, max_trials,save_step),output[2],label='E(theta_plus)')
plt.plot(np.arange(0, max_trials,save_step),output[3],label='E(theta_minus)')
plt.plot(np.arange(0, max_trials,save_step),np.ones(max_trials//save_step)*output[0],label='Final Energy')
plt.plot(np.arange(0, max_trials,save_step),np.ones(max_trials//save_step)*exact,label='Exact Energy')
plt.legend()
plt.xlabel('Trial state')
plt.ylabel('Energy')
plt.show()
