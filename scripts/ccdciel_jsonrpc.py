# Example of Python program that use the CCDciel JSON-RPC interface.
# For more information and reference of the available methods see: 
# https://www.ap-i.net/ccdciel/en/documentation/jsonrpc_reference

import requests
import json
import time

id = 0

# Define the function to call CCDciel method
def ccdciel(method, params):
    global ccdcielresult
    global id
    id += 1
    
    # To use remotely, replace "localhost" by the hostname of the computer where CCDciel is running
    ccdciel_url = "http://localhost:3277/jsonrpc"
    cmd = { "jsonrpc": "2.0", "id": id,
        "method": method,
        "params": [params] }
    
    # global variable "ccdcielresult" contain the result of the method
    ccdcielresult = requests.post(ccdciel_url, json=cmd).json()
    
def main():
    
    # Connect the devices to CCDciel if this is not already done
    ccdciel("devicesconnection","true")
    print("devicesconnection %r" %(ccdcielresult["result"]))
    
    # Wait unitl the devices are connected
    wait = True
    while wait:
        time.sleep(1)
        ccdciel("devices_connected","")
        print("devices_connected %r" %(ccdcielresult["result"]))
        wait = not(ccdcielresult["result"])
    
    # Get the telescope RA position
    ccdciel("telescopera","")
    ra=ccdcielresult["result"]
    
    # Get the telescope DEC position
    ccdciel("telescopede","")
    de=ccdcielresult["result"]
    
    # Print the telescope position
    print("Telescope RA=%6.4f DEC=%6.4f" %(ra, de))

if __name__ == "__main__":
    main()
