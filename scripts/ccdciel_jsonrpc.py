# Example of Python program that use the CCDciel JSON-RPC interface.
# For more information and reference of the available methods see: 
# https://www.ap-i.net/ccdciel/en/documentation/jsonrpc_reference

import requests
import json
import time

id = 0

# Define the function to call CCDciel method
def ccdciel(method, params):
    # increment the request id
    global id
    id += 1
    
    # To use remotely, replace "localhost" by the hostname of the computer where CCDciel is running
    ccdciel_url = "http://localhost:3277/jsonrpc"
    cmd = { "jsonrpc": "2.0", "id": id,
        "method": method,
        "params": [params] }
    
    # Send the request and return the result
    try:
      return requests.post(ccdciel_url, json=cmd).json()
    except Exception as inst:
      print(type(inst))
      print(inst.args)
      exit(1)
    
def main():

    connected = (ccdciel("Devices_Connected","")["result"])
    print("Devices_Connected %r" %(connected))
    
    if not connected :
       # Connect the devices to CCDciel if this is not already done   
       print("DevicesConnection %r" %(ccdciel("DevicesConnection","true")["result"]))
    
       # Wait unitl the devices are connected
       wait = True
       while wait:
           time.sleep(1)      
           wait = not(ccdciel("Devices_Connected","")["result"])
           print("Devices_Connected %r" %(not wait))
    
    # Get the telescope RA position
    ra = ccdciel("TelescopeRA","")["result"]
    
    # Get the telescope DEC position
    de = ccdciel("TelescopeDE","")["result"]
    
    # Print the telescope position
    print("Telescope RA=%6.4f DEC=%6.4f" %(ra, de))

if __name__ == "__main__":
    main()
