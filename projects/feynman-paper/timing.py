import time
from threading import Thread

def square(x):
    return x * x

import datetime

past_time =  datetime.datetime.utcnow()
present_time =  datetime.datetime.utcnow()
for i in range(10):
    while((present_time - past_time).seconds < 1):
        present_time = datetime.datetime.utcnow()
    print (square(2))
    past_time = present_time
print(square(2))


