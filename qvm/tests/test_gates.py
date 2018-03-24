from gates import Gates
import numpy as np

def test_base_gate_matrices():

    #I X Y Z H T S  CNOT CZ SWAP

    #I
    assert np.isclose(Gates.I, np.eye(2)).all()

    # X
    assert np.isclose(Gates.X, np.matrix([[0, 1], [1, 0]])).all()

    # Y
    assert np.isclose(Gates.Y, np.matrix([[0, -1j], [1j, 0]])).all()

    #Z
    assert np.isclose(Gates.Z, np.matrix([[1, 0], [0, -1]])).all()

    #H
    assert np.isclose(Gates.H, 1/np.sqrt(2) * np.matrix([[1, 1], [1, -1]])).all()

    #T
    assert np.isclose(Gates.T, np.matrix([[1, 0], [0, np.exp(1j * np.pi/4)]])).all()

    #S
    assert np.isclose(Gates.S, np.matrix([[1, 0], [0, 1j]])).all()

    #CNOT
    assert np.isclose(Gates.CNOT, np.matrix([[1, 0, 0, 0], [0, 1, 0 ,0], [0, 0, 0, 1], [0, 0, 1, 0]])).all()

    #CZ
    assert np.isclose(Gates.CZ, np.matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, -1]])).all()

    #SWAP
    assert np.isclose(Gates.SWAP, np.matrix([[1, 0, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1]])).all()


def test_rot_gate_matrices():
    rx = Gates.RX(np.pi / 4)
    assert np.isclose(rx, (np.cos(np.pi / 8) * np.eye(2)) - (1j * sin(np.pi / 8) * np.matrix([[0, 1], [1, 0]]))).all()

    ry = Gates.RY(np.pi / 4)
    assert np.isclose(ry, (np.cos(np.pi / 8) * np.eye(2)) - (1j * sin(np.pi / 8) * np.matrix([[0, -1j], [1j, 0]]))).all()

    rz = Gates.RZ(np.pi / 4)
    assert np.isclose(rz, (np.cos(np.pi / 8) * np.eye(2)) - (1j * sin(np.pi / 8) * np.matrix([[1, 0], [0, -1]]))).all()
