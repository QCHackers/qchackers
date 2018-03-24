import vm


url ='http://127.0.0.1:5000/api/add_message/1234'


with open('epr.eg', 'r') as myfile:
    data=myfile.read()

print(data)
    
import requests
res = requests.post(url, json={"mytext":data})
if res.ok:
    print (res.json())
