from pyquil.api import QVMConnection
from itertools import product
from mock import patch, Mock
import itertools

for i in range(0, 5):
    # i=1
    target_bitstring = [i+1]
    print(target_bitstring)
    bit = ("0", "1")
    bitstring_map = {}
    target_bitstring_phase = -1
    nontarget_bitstring_phase = 1
