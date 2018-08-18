import numpy as np
from numpy import linalg as LA

a = np.array([[1.5, 0.5], [0.5, 1.5]])

np.transpose(a)

LA.inv(a)


def H(theta):
    return np.matrix([[np.cos(2 * theta), np.sin(2 * theta)], [np.sin(2 * theta), -np.cos(2 * theta)]])


# print H (np.pi/8)


print H(np.pi/16)
