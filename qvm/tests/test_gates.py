from gates import Gates
import numpy as np

def test_base_gate_matrices():
    "Test the gates which are hardcoded matrices"
    g = Gates()
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

    #X with RX
    assert np.isclose(Gates.X, g.RY(np.pi)).all()

def test_rot_gate_matrices():
    "Test rotation gates which are passed a parametere"
    g = Gates()
    
    rx = g.RX(np.pi / 4)
    assert np.isclose(rx, (np.cos(np.pi / 8) * np.eye(2)) - (1j * np.sin(np.pi / 8) * np.matrix([[0, 1], [1, 0]]))).all()

    ry = g.RY(np.pi / 4)
    assert np.isclose(ry, (np.cos(np.pi / 8) * np.eye(2)) - (1j * np.sin(np.pi / 8) * np.matrix([[0, -1j], [1j, 0]]))).all()

    rz = g.RZ(np.pi / 4)
    assert np.isclose(rz, (np.cos(np.pi / 8) * np.eye(2)) - (1j * np.sin(np.pi / 8) * np.matrix([[1, 0], [0, -1]]))).all()
