from program import run
from vm import wavefunction

def test_bell_states():
    state_prep = {
        "B00" : "",
        "B01" : "X 1\n",
        "B10" : "X 0\n",
        "B11" : "X 0\nX 1\n",
    }

    #@TODO do this with matrices values instead of strings
    bell_states = {
        "B00" : "0.71|00> + 0.71|11>",
        "B01" : "0.71|01> + 0.71|10>",
        "B10" : "0.71|00> + -0.71|11>",
        "B11" : "0.71|01> + -0.71|10>",
    }
    epr_program = "H 0\nCNOT 0 1"

    for i in state_prep:
        bell_program = "QUBITS 2\n" + state_prep[i] + epr_program
        wvf = run(bell_program)
        assert wavefunction(wvf) == bell_states[i]
