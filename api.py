
import sys
import requests

url ='https://whispering-mountain-51676.herokuapp.com//api/add_message/1234'

def run(path):    
    with open(path, 'r') as myfile:
        data=myfile.read() 
        res = requests.post(url, json={"mytext":data})
    
        if res.ok:
            j = res.json()
            print (j["results"])


if __name__ == "__main__":    
    run(sys.argv[1])
