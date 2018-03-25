
import sys
import requests

url ='http://ec2-18-188-142-193.us-east-2.compute.amazonaws.com/api/add_message/1234'

def run(path):    
    with open(path, 'r') as myfile:
        data=myfile.read() 
        res = requests.post(url, json={"mytext":data})
    
        if res.ok:
            j = res.json()
            print (j["results"])


if __name__ == "__main__":    
    run(sys.argv[1])
