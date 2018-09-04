from eagle import api
from eagle.api import *

x_prog = api.Program(
    QREG(2),
    X(0),
    MEASURE(0))


deutch_balance = api.Program(
    QREG(2),
    X(1),
    H(0),
    H(1),

    H(0),
    H(1),
    MEASURE(0),
)

deutch_balance.eval()


deutch_constant = api.Program(
    QREG(2),
    X(1),
    H(0),
    H(1),
    # oracle
    CNOT(0, 1),
    H(0),
    H(1),
    MEASURE(0)
)

deutch_constant.eval()


deutch_constant = api.Program(
    QREG(2),
    X(1),
    H(0),
    H(1),
    # oracle
    X(1),

    H(0),
    H(1),
    MEASURE(0)
)

deutch_constant.eval()


deutch_balanced = api.Program(
    QREG(2),
    X(1),
    H(0),
    H(1),
    # oracle
    CNOT(0, 1),
    X(1),

    H(0),
    H(1),
    MEASURE(0)
)

deutch_balanced.eval()
