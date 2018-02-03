# Before you can use the jobs API, you need to set up an access token.
# Log in to the Quantum Experience. Under "Account", generate a personal 
# access token. Replace "None" below with the quoted token string.
# Uncomment the APItoken variable, and you will be ready to go.

APItoken = "c413835f96f5c9ce144faa39b7cab48d9f7b06cdaa7a5c00f2da33b3c56dbf453680ae1e9fa47cf58329404ab9ba7b8a7f2b85d961a0d4dac5ff2220e42272d1"

config = {
  "url": 'https://quantumexperience.ng.bluemix.net/api'
}

if 'APItoken' not in locals():
  raise Exception("Please set up your access token. See Qconfig.py.")
