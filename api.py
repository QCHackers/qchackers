
import requests

url ='https://whispering-mountain-51676.herokuapp.com//api/add_message/1234'

with open('programs/epr.eg', 'r') as myfile:
    data=myfile.read() 

res = requests.post(url, json={"mytext":data})

if res.ok:
    j = res.json()
    print (j["results"])
