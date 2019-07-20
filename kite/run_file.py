import sys
from kite import vm

if len(sys.argv) == 2:
    a, b = vm.evaluate(sys.argv[1], "file")
    print(b)
else:
    raise Exception("Add a file to run")
