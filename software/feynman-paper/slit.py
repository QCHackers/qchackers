import numpy as np
b = raw_input("Enter the slit width(microns)=")
print "b=",b    
b=b*np.exp(-06)
lamda=float(600)*1.E-09
D=float(100)*1.E-02
thetamin1=-np.pi/200
thetamax1=np.pi/200
pas=(thetamax1-thetamin1)/2001
Theta1=[]

for i in range(2001):
    T1 = thetamin1+i*pas
    Theta1.append(T1)
    U1 = np.pi*b*np.sin(Theta1)/lamda
    Amplitude = np.sine(U1)/U1
    Interns_1f=Amplitude**2
    U1 = 1.E+2*np.tan(Theta1)*np.array(D)
