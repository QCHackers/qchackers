from flask import Flask, request, jsonify, render_template
from datetime import datetime
from qvm import vm
from qvm.vm import isolate_qubit
import program
import re
import requests
import os

name = "hello"
f'He said his name is {name!r}.'

 
app = Flask(__name__)
 

@app.route('/')
def homepage():
    return render_template('index.html')

@app.route('/compiler')
def compiler():
    return render_template('compiler.html')

@app.route('/qpl')
def qpl():
    return render_template('qpl.html')

@app.route('/vm')
def vm():
    return render_template('vm.html')


@app.route('/api/add_message/<uuid>', methods=['GET', 'POST'])
def add_message(uuid):
    content = request.json
    print (content['mytext'])
    p = content['mytext']
   
  
    #os.chmod("program.txt", 777)
    text_file = open("compiler/program.txt", "w")
    #os.chmod("program.txt", "w")
    text_file.write(p)
    text_file.close()
    os.system ("bash -c './compiler/compile.lisp program.txt'")

    os.system ("bash -c 'python preprocessor.py a.ir'")
    
    with open('compiler/a.eg', 'r') as myfile:
        p=myfile.read()
     
    wvf, msg = program.run(p)
    return jsonify({"results" : msg})



@app.route('/api/teleportsend', methods=['get', 'post'])
def teleportsend():
    sendit = request.form.get('sendit')
    if not sendit is None:
        url = request.url_root + 'api/teleportrecieve'
        p = """QUBITS 3
H 1
CNOT 1 2
CNOT 0 1
H 0
MEASURE 0
MEASURE 1"""
    wvf, msg = program.run(p)

    # Very hacky way of doing this... @TODO make this better
    m = re.findall('====== MEASURE qubit (\d) : (\d)', msg)
    if m[0][0] == '0':
        q0 = int(m[0][1])
        q1 = int(m[1][1])
    else :
        q1 = int(m[0][1])
        q0 = int(m[1][1])
    print(url)
    res = requests.post(url, json={
        "q0": q0,
        "q1" : q1
    })

    if res.ok:
        j = res.json()
        p = j['program']
        p_split = p.splitlines()
        p_str = "<ol>"
        for i in p_split:
            p_str += '<li><samp class="code-block">' + i + '</li>'
        p_str += '</ol>'
        j['program'] = p_str
        j['a'] = f"Qubit 0 : {q0}<br />Qubit 1 : {q1}"
        return (jsonify(j))



@app.route('/api/teleportrecieve', methods=['get', 'post'])
def teleportrecieve():
    content = request.json
    q0 = content["q0"]
    q1 = content["q1"]

    p = """QUBITS 4
MEASURE 0
MEASURE 1
H 2
CNOT 2 3
CLASSICAL 1 1 1
X 3
CLASSICAL 0 1 1
Z 3"""

    p_list = p.splitlines()
    if q0 == 1:
        p_list.insert(1, "X 0")
    if q1 == 1:
        p_list.insert(1, "X 1")

    p = "\n".join(p_list)
    wvf, msg = program.run(p)

    msg = isolate_qubit(wvf, 3)
    return jsonify({"program" : p, 'wvf' : msg})

@app.route('/teleportation', methods=['GET', 'POST'])
def teleporation():
    return render_template('teleportation.html')
 
if __name__ == "__main__":
 app.run()
