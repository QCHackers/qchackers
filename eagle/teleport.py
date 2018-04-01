import sys
import requests
import program
import re

url ='http://127.0.0.1:5000/api/teleport'

def run(path):
    with open(path, 'r') as myfile:
        data=myfile.read()
        wvf, msg = program.run(data)
        print(msg)
        
        # Very hacky way of doing this... @TODO make this better
        m = re.findall('====== MEASURE qubit (\d) : (\d)', msg)
        if m[0][0] == '0':
            q0 = int(m[0][1])
            q1 = int(m[1][1])
        else :
            q1 = int(m[0][1])
            q0 = int(m[1][1])


        res = requests.post(url, json={
            "q0": q0,
            "q1" : q1
        })

        if res.ok:
            j = res.json()
            print (j["results"])


if __name__ == "__main__":
    run(sys.argv[1])
