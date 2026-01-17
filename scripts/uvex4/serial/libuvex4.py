# UVEX4 function library
# see: https://spectro-uvex.tech/?p=3178
#

import serial
import time
import logging
import platform
from configparser import ConfigParser

verbose=False

# read config file and return values
def UVEX4readconfig(configfile) :
    parser = ConfigParser()
    parser.read(configfile)                          # read the configuration file
    portCom = parser['settings']['portCOM']          # mandatory serial port, COM4 or /dev/ttyUSB0
    serialtimeout = int(parser['settings'].get('serialtimeout','10'))  # timeout to use to create the serial
    cmdtimeout = int(parser['settings'].get('commandtimeout','60'))    # maximum elapse time for a UVEX4 command
    return portCom, serialtimeout, cmdtimeout 

# connect serial link, command timeout, and wait the end of initialization
def UVEX4init(portCom,st,ct,callback=0) :
    global ser, serialtimeout, globaltimeout
    serialtimeout = st
    globaltimeout = ct 
    BAUD=115200

    ser= serial.Serial(port=None, baudrate=BAUD, timeout=1)
    ser.port = portCom
    if platform.system() == 'Windows':
       ser.dtr = False                              # prevent Arduino reset
       ser.rts = False                              # on Linux use "/bin/stty -F /dev/ttsUSB0 -hupcl"
    ser.open()
    time.sleep(0.1)

    r = "no response"
    ok = False                                       # if nothing is read
    while not (r == "") :                            # purge the information send after connection
      r = ser.readline().decode("ascii", "ignore")
      ser.timeout = 0.5                              # change timeout to 0.5 to quickly detect the last message
      if len(r) == 0 :
        break                                        # timeout, end of initialization, exit
      else :
        ok = True                                    # we read something set OK

    # get initialization informations
    if callback != 0 :
       ser.timeout = serialtimeout
       command = ':INIT;#' 
       ser.write(command.encode('ascii'))               # write command
       r = "no response"
       while not (r == "") :                            # read the INIT information
         r = ser.readline().decode("ascii", "ignore")
         if r.startswith(':IIOK'):                      # last message before calibrex
            ser.timeout = 0.5                           # change timeout to 0.5 to quickly detect the last message
         if len(r) == 0 :
           break                                        # timeout, end of initialization, exit
         if callback != 0 :
           callback(r)                                  # process information message
                                                     # end of initialization
    ser.timeout = serialtimeout                      # set the configured timeout for the next operations
 
    return ok

# close the serial link
def UVEX4close() :
    if 'ser' in globals() :
      ser.close

# query command that send response prefixed by the command name
# example :SPOS;#  ->  :SPOS;6;#
def UVEX4query(command, resp='', minlen=3, numresp=1) :
    if resp=='' :
      resp = command[:-1]                      # default response is command without the last #
    if verbose :
              logging.warning('Send query: {!r}'.format(command))
              logging.warning('Waiting for: {!r}'.format(resp))
    ser.reset_input_buffer()
    ser.write(command.encode('ascii'))         # write command
    r = "no response"
    countresp = 0
    start=time.time()
    while not ((r.startswith(resp)) or (r == "")) :
      r = ser.readline().decode("ascii", "ignore")       # read response
      if verbose :
              logging.warning('Receive: {!r}'.format(r))
      if len(r) == 0 :
        logging.error('timeout')
        break                                  # timeout, exit
      if time.time()-start > globaltimeout:    # global command timeout exceded
         raise Exception('Command {!r} take too long to complete'.format(command))
      l = r.split(';')
      if len(l)<minlen :                       # check response length to skip command echo
        r = "no response"  
      if r.startswith(resp) :
        countresp += 1                         # trick to bypass bug with missing ; after value: :CMAX;1#
        if countresp < numresp :
           r = "no response"  
        
    return r                                   # return response string

# command that set a new value and wait this is done by checking the resp message
# example :GGTL;5500;#
def UVEX4cmd(command, resp, testbusy=True) :
    if verbose :
              logging.warning('Send command: {!r}'.format(command))
              logging.warning('Waiting for: {!r}'.format(resp))
    ser.reset_input_buffer()
    ser.write(command.encode('ascii'))         # write command
    r = "no response"
    if testbusy :
      busy=True
    else :
      busy=False
    start=time.time()
    while not (((not busy) and (resp in r)) or (r == "")) :
      r = ser.readline().decode("ascii", "ignore")       # read response
      if verbose :
              logging.warning('Receive: {!r}'.format(r))
      if len(r) == 0 :
        logging.error('timeout')
        break                                  # serial timeout, exit
      if time.time()-start > globaltimeout:    # global command timeout exceded
         raise Exception('Command {!r} take too long to complete'.format(command))
      if testbusy :
         l = r.split(';')
         for index, value in enumerate(l):
            if (":IBSY" in value) :            # detect busy status change
               busy = (l[index+1]=='0')
               break
    return r                                   # return last response, must be equal to resp parameter

# one slit information
class SlitDef :
    Offset = 0
    Label = ''

# UVEX4 class for use in application script
# This is normally the only to import
class UVEX4 :

    # Create with the config file name in parameter, serial is connected
    def __init__(self, configfile):
        self.Initialized = False
        self.Version_Arduino = "1"
        self.Grating_motor_enabled = False
        self.Slit_motor_enabled = False
        self.Focuser_motor_enabled = False
        self.Ethernet_module_enabled = False
        self.WiFi_module_enabled = False
        self.Slit_photodiode_enabled = False
        self.Calibrex_enabled = False
        self.Focuser_hall_enabled = False
        self.Temperature_active = False
        self.CalibrationNb = 0
        self.GratingRes = 0
        self.GratingSpRes = 0
        self.GratingAnRes = 0
        self.SlitSet = 0
        self.NumSlit = 0
        self.CameraPixelSize = 0
        self.CameraWidth = 0
        self.InitTemperature = 0
        self.Slits = []
        self.CalNames = []
        # read the config file
        portCom, serialtimeout, cmdtimeout = UVEX4readconfig(configfile)
        # connect the UVEX4
        UVEX4init(portCom, serialtimeout, cmdtimeout, self.Init_Callback)

    # Destructor try close serial
    def __del__(self):
        try:
          UVEX4close()
        except: 
          pass

    # process initialization messages from UVEX4init
    # this set some static properties, ignore property that can be modified later
    def Init_Callback(self,event) :

        if event.startswith(':IVE1') :                   # Version arduino
           ev = event.split(';')
           if len(ev)>1 :
             self.Version_Arduino = ev[1]
        elif event.startswith(':IST0') :                 # Config spectro
           ev = event.split(';')
           if len(ev)>1 :
              flag = int(ev[1]) 
              self.Grating_motor_enabled   = flag and 1
              self.Slit_motor_enabled      = flag and 2
              self.Focuser_motor_enabled   = flag and 4
              self.Ethernet_module_enabled = flag and 8
              self.WiFi_module_enabled     = flag and 16
              self.Slit_photodiode_enabled = flag and 32
              self.Calibrex_enabled        = flag and 64
              self.Focuser_hall_enabled    = flag and 128
        elif event.startswith(':IGTE') :                 # Temperature available
           ev = event.split(';')
           if len(ev)>1 :
             self.Temperature_active = (ev[1] == '1' )
        # ignore :IGMO;
        elif event.startswith(':IGAM') :                 # Camera information
           ev = event.split(';')
           if len(ev)>2 :
             self.CameraPixelSize = float(ev[1]) / 10
             self.CameraWidth = int(ev[2])
       # ignore :GSBG; GOFS; :GPOS; :GSPI;
        elif event.startswith(':GGSL') :                 # Grating line/mm
           ev = event.split(';')
           if len(ev)>1 :
             self.GratingRes = int(ev[1])
        elif event.startswith(':ISPE') :             # Grating/Camera resolution
           ev = event.split(';')
           if len(ev)>2 :
             self.GratingSpRes = float(ev[1])
             self.GratingAnRes = float(ev[2])
        elif event.startswith(':SGYP') :             # Set of slits
           ev = event.split(';')
           if len(ev)>1 :
             self.SlitSet = int(ev[1])
        elif event.startswith(':SMAX') :             # Nb of slit position
           ev= event.split(';')
           if len(ev)>1 :
             self.NumSlit = int(ev[1])
             for x in range(self.NumSlit):
                self.Slits.append(SlitDef())
        elif event.startswith(':SNAM') :             # Slit Name
           ev = event.split(';')
           if len(ev)>2 :
             for x in range(self.NumSlit):
                 self.Slits[x].Label = ev[x+1]
        # ignore :STEP;
        elif event.startswith(':SGOF') :             # Slit offset
           ev = event.split(';')
           if len(ev)>2 :
             x = int(ev[1])
             self.Slits[x-1].Offset = int(ev[2])
        # ignore :SPOS; :SGPH; :SGTS;
        # ignore :FPOS; :FSTE;
        elif event.startswith(':CMAX') :             # Number Calibration switch configured in UVEX4
           ev = event.split(';')
           if len(ev)>1 :
             self.CalibrationNb = int(ev[1])
        elif event.startswith(':CNAM') :             # Number Calibration switch configured in UVEX4
           self.CalNames = event.split(';')
           if len(self.CalNames)>2 :
              self.CalNames.pop(0)                   # remove first and last
              self.CalNames.pop()
        elif event.startswith(':ITEM') :             # Temperature
           ev = event.split(';')
           if len(ev)>1 :
             self.InitTemperature = float(ev[1])
        elif event.startswith(':IIOK') :             # End of initialization
           self.Initialized = True
        else:
           if verbose :
              logging.warning('unhandled event: {!r}'.format(event))

    # return the temperature
    @property
    def temperature(self) :
        if not self.Temperature_active :
           raise Exception("Temperature probe not active")
        r = UVEX4query(":ITEM;#")
        l = r.split(';')
        t = l[1]
        if t == 0.0 :
           t = self.InitTemperature
        return float(t)

    # return the grating resolution
    @property
    def grating_resolution(self) :
        return self.GratingRes

    # return the grating position [central, mini, maxi] in A
    @property
    def wavelength(self) -> list:
       if not self.Grating_motor_enabled :
          raise Exception("Grating motor option disabled")
       r = UVEX4query(":GPOS;#")              # query the grating position
       w = r.split(';')
       w.pop(0)                               # remove first and last
       w.pop()
       return w

    # set a new central wavelength
    @wavelength.setter
    def wavelength(self,w):
       if not self.Grating_motor_enabled :
          raise Exception("Grating motor option disabled")
       # send command to change the central wavelength
       r = UVEX4cmd(":GGTL;"+str(w)+";#", ":IMES; GSTP Power off grating motor;")
       if r == "" :
          raise Exception("Error timeout")

    # return the position number of the slit wheel
    # use the list Slits[slitpos-1] for details
    @property
    def slitpos(self):
       if not self.Slit_motor_enabled :
          raise Exception("Slit motor option disabled")
       # query the slit position
       r = UVEX4query(":SPOS;#")
       l = r.split(';')
       return int(l[1])

    # set a new slit position, parameter can be the position number or the slit name
    @slitpos.setter
    def slitpos(self,s):
       if not self.Slit_motor_enabled :
          raise Exception("Slit motor option disabled")
       try:
         # try if parameter is slit number
         snum = int(s)
       except:
          # parameter s is slit name
          snum = -1
          for sn in range(self.NumSlit) :
             if self.Slits[sn].Label == s :
                snum = sn+1
          if snum <= 0 :
             raise Exception('Slit {!r} not found'.format(s))

       # use the photodiode if enabled
       if self.Slit_photodiode_enabled :
          photodiode = "1"
       else :
          photodiode = "0"
       # send command to change the slit number
       r = UVEX4cmd(":SMOV;"+str(snum)+";"+photodiode+";#", ":IMES; SSTP Power off slit motor;")
       if r == "" :
          raise Exception("Error timeout")
       # check the slit position
       p = self.slitpos
       if p != snum :
          raise Exception('Wrong slit set {!r}'.format(p))

    # return the list of calibration switch name
    @property
    def calibrations_name(self) -> list:
       if not self.Calibrex_enabled :
          raise Exception("Calibrex option disabled")
       return self.CalNames

    # list of all switch values
    @property
    def calibration(self)  -> list :
       if not self.Calibrex_enabled :
          raise Exception("Calibrex option disabled")
       r = UVEX4query(":CGAC;#")
       w = r.split(';')
       w.pop(0)                               # remove first and last
       w.pop()
       return w

    # set a switch on/off, parameter is a list or tupple ( switch number or name , 0 or 1 state )
    @calibration.setter
    def calibration(self,vals) :
       if not self.Calibrex_enabled :
          raise Exception("Calibrex option disabled")
       c, cstate = vals
       try:
         # try if parameter is switch number
         cnum = int(c)
       except:
          # parameter c is switch name
          cl = self.calibrations_name
          cnum=-1
          # search the position of the calibration name
          for cn in range(len(cl)):
             if cl[cn] == c :
                cnum = cn+1
          if cnum <= 0 :
             raise Exception('Calibration {!r} not found'.format(c))

       # send command to change the calibration state
       r = UVEX4cmd(":CACT;"+str(cnum)+";"+str(cstate)+";#", ":CGAC;",testbusy=False)
       if r == "" :
          raise Exception("Error timeout")
       # query the calibration state
       r = UVEX4query(":CGAC;#")
       w = r.split(';')
       if w[cnum] != str(cstate) :
          raise Exception('Calibration state do not change: {!r}'.format(r))

    # Move the focus in by steps
    def focusin (self, steps) :
       if not self.Focuser_motor_enabled :
          raise Exception("Focuser motor option disabled")
       r = UVEX4cmd(":FGIN;"+str(steps)+";#", ':IMES;Power off focuser')
       if r == "" :
          raise Exception("Error timeout")

    # Move the focus out by steps
    def focusout (self, steps) :
       if not self.Focuser_motor_enabled :
          raise Exception("Focuser motor option disabled")
       r = UVEX4cmd(":FGOU;"+str(steps)+";#", ':IMES;Power off focuser')
       if r == "" :
          raise Exception("Error timeout")

    # return the focuser absolute position
    @property
    def focuspos(self):
       if not self.Focuser_motor_enabled :
          raise Exception("Focuser motor option disabled")
       if not self.Focuser_hall_enabled :
          raise Exception("Focuser hall option disabled")
       r = UVEX4query(":FSTE;#")
       p = r.split(';')
       return int(p[1])

    # return the focuser relative position
    @property
    def focusrelpos(self):
       if not self.Focuser_motor_enabled :
          raise Exception("Focuser motor option disabled")
       r = UVEX4query(":FPOS;#")
       p = r.split(';')
       return int(p[1])
