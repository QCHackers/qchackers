import json
import requests

url = "https://api.rigetti.com/qvm"


api_key = "nmRPAVunQl19TtQz9eMd11iiIsArtUDTaEnsSV6u"
user_id = "4c2166b5-a99d-433c-a289-afcfcd1f1bda"

headers = {'Accept': 'application/octet-stream',
           'X-User-Id': user_id,
           "X-Api-Key": api_key}

payload = {
    "type": "multishot",
    "addresses": [0, 1],
    "trials": 1,
    "compiled-quil": "I 0\nMEASURE 0 [0]\n"

}

# print(payload)

response = requests.post(url, data=json.dumps(payload), headers=headers)


print(type(response.text))
