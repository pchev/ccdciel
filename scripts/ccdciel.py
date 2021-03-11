# Python function to use the CCDciel JSON-RPC interface.
# For more information and reference of the available methods see: 
# https://www.ap-i.net/ccdciel/en/documentation/jsonrpc_reference

import sys
import os
try:
  from urllib import request
except:
  print('Cannot import urllib.request, your version of python is probably too old, try python3')
  sys.exit(1)
try:
  import json
except:
  print('Cannot import json, your version of python is probably too old, try python3')
  sys.exit(1)

id = 0

# Define the function to call CCDciel method
def ccdciel(method, params=''):
    
    # Increment the request id
    global id
    id += 1

    # Get host and port from environment variable
    host = os.getenv('CCDCIEL_HOST')
    if host == None :
       host='localhost' 
    port = os.getenv('CCDCIEL_PORT')
    if port == None :
       port='3277'
       
    # The request URL
    ccdciel_url = "http://" + host + ":" + port + "/jsonrpc"

    # Prepare JSON request
    jsondata = json.dumps({"jsonrpc": "2.0", "id": id,
        "method": method,
        "params": [params] }
        ).encode('utf8')

    try:
       # Execute the request using POST method
       req = request.Request(ccdciel_url)
       req.add_header('Content-Type', 'application/json; charset=utf-8')
       res = request.urlopen(req, jsondata).read()
    except Exception as inst:
       print(type(inst))
       print(inst.args)
       sys.exit(1)
    
    # Return result as JSON
    return json.loads(res.decode("utf8"))
