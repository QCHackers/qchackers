#!/usr/bin/python
# -*- coding: utf-8 -*-
import numpy as np
from pyquil import *
from pyquil.gates import *
import itertools
from operator import xor


def eval_answer(ist):
    r = int(ist[0])
    s = int(ist[1])
    t = int(ist[2])
    a = int(ist[3])
    b = int(ist[4])
    c = int(ist[5])

    if (a ^ b ^ c) == (r or s or t):
        return 'They won'
    else:
        return 'They lost'


perm = ["".join(seq) for seq in itertools.product("01", repeat=3)]

question_str = []
for x in perm:
    if "1" not in x or x.count('1') == 2:
        question_str.append(x)

print(question_str)


qvm = QVMConnection()

ch = np.matrix([[1, 0, 0, 0],
                [0, 1, 0, 0],
                [0, 0,  1/np.sqrt(2),  1/np.sqrt(2)],
                [0, 0,  1/np.sqrt(2),  -1/np.sqrt(2)]])

p = Program()


def GHZ_game_classical(p):
    p.inst(X(3),
           CNOT(3, 4),
           CNOT(3, 5),
           MEASURE(0, 0),
           MEASURE(1, 1),
           MEASURE(2, 2),
           MEASURE(3, 3),
           MEASURE(4, 4),
           MEASURE(5, 5))

    answer = qvm.run(p)[0]
    print(answer)
    return eval_answer(answer)


#p.inst(X(0), X(1))
print(GHZ_game_classical(p))


p = Program().defgate("CH", ch)


# p.inst(X(0))


def GHZ_game_quantum(p):
    p.inst(H(0),
           H(1),
           H(2),
           H(3),
           CNOT(3, 4),
           CNOT(3, 5),
           ("CH", 0, 3),

           ("CH", 1, 4),

           ("CH", 2, 5),

           MEASURE(0, 0),
           MEASURE(1, 1),
           MEASURE(2, 2),
           MEASURE(3, 3),
           MEASURE(4, 4),
           MEASURE(5, 5))

    print(p)
    print(qvm.wavefunction(p))
    answer = qvm.run(p)[0]
    print(answer)
    return eval_answer(answer)


print(GHZ_game_quantum(p))


print((1 ^ 1 ^ 1) == (0 or 1 or 1))
