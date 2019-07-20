import kite as kt

circuit = kt.Program(
    kt.QREG(1),
    kt.H(0),
    kt.MEASURE(0))

before, after = circuit.run()
print("Circuit ", after)
