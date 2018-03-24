from gates import Gates
import numpy as np

def test_gate_matrices():

    #I X Y Z H T S  CNOT CZ SWAP

    assert np.isclose(Gates.I, np.eye(2)).all()
