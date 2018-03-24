import vm
import sys

if len(sys.argv) == 2:
    vm.evaluate(sys.argv[1], "file")
else:
     raise Exception("Add a file to run")
