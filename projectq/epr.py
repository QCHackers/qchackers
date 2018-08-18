from projectq import MainEngine  # import the main compiler engine
# import the operations we want to perform (Hadamard and measurement)
from projectq.ops import H, Measure

eng = MainEngine()  # create a default compiler (the back-end is a simulator)
qubit = eng.allocate_qubit()  # allocate 1 qubit

H | qubit  # apply a Hadamard gate
Measure | qubit  # measure the qubit

eng.flush()  # flush all gates (and execute measurements)
print("Measured {}".format(int(qubit)))  # output measurement result
