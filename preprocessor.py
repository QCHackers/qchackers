import re

fp = open("a.ir")
fp1= open("a.eg","w+")

for line in fp:
    fp1.write(re.sub('[(){}<>]', '', line))
