import vm
import requests
from quantumcomputer import QuantumComputer
from flask import Flask, request
app = Flask(__name__)

@app.route("/receiveTeleport")
def receiveTeleport():
    #Define bit0 and bit1 requests
    #
    #bit0 corresponds to mesurement 0 of Qubit 0 from sender
    #
    #bit1 corresponds to measurment 1 of Qubit 1 from sender
    #
    bit0 = request.args['bit0']
    bit1 = request.args['bit1']
    
    #TODO: Make sure address is not bigger than size of vm and not already taken
    #q0 = Qubit(1)
    return bit0 + bit1

@app.route("/TeleportTemp")
def teleportTemp():
    return requests.get('http://127.0.0.1:5001/receiveTeleport?bit0=0&bit1=1').content
    

if __name__ == "__main__":
    app.run(port=5000)
    #TODO: Let arguments set size of qc
    qc = QuantumComputer(2);
    
