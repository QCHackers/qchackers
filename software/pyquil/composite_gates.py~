from pyquil.quil import Program

p = Program()

"""
Gate definitions
"""
p.inst("""DEFGATE CRX(%theta):
    1, 0, 0, 0
    0, 1, 0, 0
    0, 0, cos(%theta/2), -i*sin(%theta/2)
    0, 0, -i*sin(%theta/2), cos(%theta/2)""")

p.inst("""DEFGATE CRY(%theta):
    1, 0, 0, 0
    0, 1, 0, 0
    0, 0, cos(%theta/2), -sin(%theta/2)
    0, 0, sin(%theta/2), cos(%theta/2)""")

p.inst("""DEFGATE CRZ(%theta):
    1, 0, 0, 0
    0, 1, 0, 0
    0, 0, e^(-i*%theta/2), 0
    0, 0, 0, e^(i*%theta/2)""")
