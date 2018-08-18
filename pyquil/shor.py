import random


def gcd(a, n):
    if n == 0:
        return a
    else:
        return gcd(n, a % n)


def factor(n):
    a = random.randint(1, n-1)
    divisor = gcd(a, n)

    if divisor != 1:
        return a
    else:
    r = period()

    if r % 2 != 0:

    print(a)


factor(5)
print(gcd(2, 4))
