import sys
import re
import requests
import kite as kt

url ='http://127.0.0.1:5000/'

def run(path):
    _, msg = kt.API.file_api(f"{path}")
    print(msg)
    # Very hacky way of doing this... @TODO make this better
    m = re.findall(r'====== MEASURE qubit (\d) : (\d)', msg)
    if m[0][0] == '0':
        q0 = int(m[0][1])
        q1 = int(m[1][1])
    else :
        q1 = int(m[0][1])
        q0 = int(m[1][1])
    res = requests.post(url, data={
            "q0": q0,
            "q1" : q1
        })
    print(res.status_code, res.reason)


if __name__ == "__main__":
    run(sys.argv[1])
