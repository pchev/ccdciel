# -*- coding: utf-8 -*-
# -----------------------------------------------------------------------------
# UVEX4 - Copied from alpyca switch.py
#         Implements a special ASCOM Alpaca Switch class with connection retry and
#         increased timeout to allow enough time to position the motors when some switch are set.
#         The change add tmo=60.0 to the _put call of SetSwitch and SetSwitchValue.
#
# Part of the Alpyca application interface package
#
# Author:   Robert B. Denny <rdenny@dc3.com> (rbd)
#
# Python Compatibility: Requires Python 3.9 or later
# Doc Environment: Sphinx v5.0.2 with autodoc, autosummary, napoleon, and autoenum
# GitHub: https://github.com/ASCOMInitiative/alpyca
#
# -----------------------------------------------------------------------------
# MIT License
#
# Copyright (c) 2022-2024 Ethan Chappel and Bob Denny
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# -----------------------------------------------------------------------------
# Edit History:
# 02-May-22 (rbd) Initial Edit
# 13-May-22 (rbd) 2.0.0-dev1 Project now called "Alpyca" - no logic changes
# 01-Jan-23 (rbd) 2.0.4 https://github.com/ASCOMInitiative/alpyca/issues/8
#                 Change 'ID' to 'Id' for switch parameters.
# 05-Mar-24 (rbd) 3.0.0 New members for Platform 7
# 07-Mar-24 (rbd) 3.0.0 Add Master Interfaces refs to all members
# 08-Mar-24 (rbd) 3.0.0 Clarify switch vs driver vs device etc. No
#                 logic changes.
# 23-Nov-24 (rbd) 3.0.1 For PDF rendering no change to logic
# -----------------------------------------------------------------------------

from alpaca.device import Device
from typing import List
import time

class UVEX4(Device):
    """ASCOM Standard ISwitch V2 Interface"""

    def __init__(
        self,
        address: str,
        device_number: int,
        protocol: str = "http"
    ):
        """Initialize the Switch device.

        Args:
            address (str): IP address and port of the device (x.x.x.x:pppp)
            device_number (int): The index of the device (usually 0)
            protocol (str, optional): Only if device needs https. Defaults to "http".

        """
        super().__init__(address, "switch", device_number, protocol)

    def __get(self,attribute: str, tmo=5.0, **data) -> str:
        """ get with retry in case of failure """
        maxretry=15
        waitretry=1
        for n in range(maxretry):
           try:
              result=self._get(attribute, tmo, **data)
           except Exception as ex:
              lastex=ex
              exname=type(ex).__name__
              if (exname == "ConnectionError") or (exname == "ConnectTimeout") or (exname == "ReadTimeout"):
                 # retry in case the UVEX4 connection is busy
                 if n<(maxretry-2):
                    #print(attribute,'retry',n)
                    time.sleep(waitretry)
              else:
                 # other exception to not retry
                 raise lastex from None
           else:
              # normal exit when all is fine
              break
        else:
           # number of retry excedded
           raise lastex from None
        # return value
        return result

    def __put(self,attribute: str, tmo=5.0, **data) -> str:
        """ put with retry in case of failure """
        maxretry=15
        waitretry=1
        for n in range(maxretry):
           try:
              result=self._put(attribute, tmo, **data)
           except Exception as ex:
              lastex=ex
              exname=type(ex).__name__
              if (exname == "ConnectionError") or (exname == "ConnectTimeout") or (exname == "ReadTimeout"):
                 # retry in case the UVEX4 connection is busy
                 if n<(maxretry-2):
                    #print(attribute,'retry',n)
                    time.sleep(waitretry)
              else:
                 # other exception to not retry
                 raise lastex from None
           else:
              # normal exit when all is fine
              break
        else:
           # number of retry excedded
           raise lastex from None
        # return value
        return result

    # other properties normally in device.py we want to make work with retries
    @property
    def Connected(self) -> bool:
        return self.__get("connected")
    @Connected.setter
    def Connected(self, ConnectedState: bool):
        self.__put("connected", Connected=ConnectedState)
    @property
    def DeviceState(self) ->List[dict]:
        response = self.__get("devicestate")
        return response


    @property
    def MaxSwitch(self) -> int:
        """Count of switches managed by this device.

        Raises:
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Number of switches managed by this device. Switches are numbered from 0
              to MaxSwitch - 1.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |MaxSwitch|

                .. |MaxSwitch| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.MaxSwitch" target="_blank">
                    Switch.MaxSwitch</a> (external)

            .. only:: rinoh

                `Switch.MaxSwitch <https://ascom-standards.org/newdocs/switch.html#Switch.MaxSwitch>`_
        """
        return self.__get("maxswitch")

    def CanAsync(self, Id: int) -> bool:
        """The specified switch can operate asynchronously.
        See :meth:`SetAsync` and :meth:`SetAsyncValue`.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * Examples of switches that cannot be written to include a
              limit switch or a sensor.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |CanAsync|

                .. |CanAsync| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.CanAsync" target="_blank">
                    Switch.CanAsync()</a> (external)

            .. only:: rinoh

                `Switch.CanAsync() <https://ascom-standards.org/newdocs/switch.html#Switch.CanAsync>`_
        """
        return self.__get("canasync", Id=Id)

    def CancelAsync(self, Id: int) -> None:
        """Cancels an in-progress asynchronous state change operation. See :meth:`SetAsync` and
        :meth:`SetAsyncValue` for details of asynchronous switch operations.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * On return, the next call to :meth:`StateChangeComplete` for this switch
              will raise an :py:class:`OperationCancelledException`; thereafter calls
              to :meth:`StateChangeComplete` for the switch will return ``False``.
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |CancelAsync|

                .. |CancelAsync| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.CancelAsync" target="_blank">
                    Switch.CancelAsync()</a> (external)

            .. only:: rinoh

                `Switch.CancelAsync() <https://ascom-standards.org/newdocs/switch.html#Switch.CancelAsync>`_
        """
        return self.__put("cancelasync", Id=Id)

    def CanWrite(self, Id: int) -> bool:
        """The specified switch can be written to.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * Examples of witches that cannot be written to include a
              limit switch or a sensor.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |CanWrite|

                .. |CanWrite| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.CanWrite" target="_blank">
                    Switch.CanWrite()</a> (external)

            .. only:: rinoh

                `Switch.CanWrite() <https://ascom-standards.org/newdocs/switch.html#Switch.CanWrite>`_
        """
        return self.__get("canwrite", Id=Id)

    def GetSwitch(self, Id: int) -> bool:
        """The state of the specified switch.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * On is True, Off is False.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |GetSwitch|

                .. |GetSwitch| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.GetSwitch" target="_blank">
                    Switch.GetSwitch()</a> (external)

            .. only:: rinoh

                `Switch.GetSwitch() <https://ascom-standards.org/newdocs/switch.html#Switch.GetSwitch>`_
        """
        return self.__get("getswitch", Id=Id)

    def GetSwitchDescription(self, Id: int) -> str:
        """The textual description of the specified switch.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |GetSwitchDescription|

                .. |GetSwitchDescription| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.GetSwitchDescription" target="_blank">
                    Switch.GetSwitchDescription()</a> (external)

            .. only:: rinoh

                `Switch.GetSwitchDescription() <https://ascom-standards.org/newdocs/switch.html#GetSwitchDescription.MaxSwitch>`_
        """
        return self.__get("getswitchdescription", Id=Id)

    def GetSwitchName(self, Id: int) -> str:
        """The textual name of the specified switch.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |GetSwitchName|

                .. |GetSwitchName| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.GetSwitchName" target="_blank">
                    Switch.GetSwitchName()</a> (external)

            .. only:: rinoh

                `Switch.GetSwitchName() <https://ascom-standards.org/newdocs/switch.html#Switch.GetSwitchName>`_
        """
        return self.__get("getswitchname", Id=Id)

    def GetSwitchValue(self, Id: int) -> float:
        """The value of the specified switch as a float.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.


        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |GetSwitchValue|

                .. |GetSwitchValue| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.GetSwitchValue" target="_blank">
                    Switch.GetSwitchValue()</a> (external)

            .. only:: rinoh

                `Switch.GetSwitchValue() <https://ascom-standards.org/newdocs/switch.html#Switch.GetSwitchValue>`_
        """
        return self.__get("getswitchvalue", Id=Id)

    def MaxSwitchValue(self, Id: int) -> float:
        """The maximum value of the specified switch as a double.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |MaxSwitchValue|

                .. |MaxSwitchValue| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.MaxSwitchValue" target="_blank">
                    Switch.MaxSwitchValue()</a> (external)

            .. only:: rinoh

                `Switch.MaxSwitchValue() <https://ascom-standards.org/newdocs/switch.html#Switch.MaxSwitchValue>`_
        """
        return self.__get("maxswitchvalue", Id=Id)

    def MinSwitchValue(self, Id: int) -> float:
        """The minimum value of the specified switch as a double.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |MinSwitchValue|

                .. |MinSwitchValue| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.MinSwitchValue" target="_blank">
                    Switch.MinSwitchValue()</a> (external)

            .. only:: rinoh

                `Switch.MinSwitchValue() <https://ascom-standards.org/newdocs/switch.html#Switch.MinSwitchValue>`_
        """
        return self.__get("minswitchvalue", Id=Id)

    def SetAsync(self, Id: int, State: bool) -> None:
        """Asynchronouly Set a switch to the specified boolean on/off state.

        Args:
            Id: the specified switch number (see Notes)
            State: The required control state

        Raises:
            NotImplementedException: If :meth:`CanAsync` ``= False`` for switch ``Id``
            InvalidValueException: The ``Id`` is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * **Asynchronous** (non-blocking): The method returns as soon as the state change
              operation has been successfully started, with :meth:`StateChangeComplete` for
              switch ``Id = False``. After the state change has completed
              :meth:`StateChangeComplete` becomes True.
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * On is True, Off is False.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |SetAsync|

                .. |SetAsync| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.SetAsync" target="_blank">
                    Switch.SetAsync()</a> (external)

            .. only:: rinoh

                `Switch.SetAsync() <https://ascom-standards.org/newdocs/switch.html#Switch.SetAsync>`_
        """
        self.__put("setasync", Id=Id, State=State)

    def SetAsyncValue(self, Id: int, Value: float) -> None:
        """Asynchronouly Set a switch to the specified value

        Args:
            Id: the specified switch number (see Notes)
            Value: The value to be set, between :meth:`MinSwitchValue`` and :meth:`MaxSwitchValue`
                for switch ``Id``

        Raises:
            NotImplementedException: If :meth:`CanAsync` ``= False`` for switch ``Id``
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`), or if the
                given value is not between :meth:`MinSwitchValue` and :meth:`MaxSwitchValue`
                for the given switch ``Id``.
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * **Asynchronous** (non-blocking): The method returns as soon as the state change
              operation has been successfully started, with :meth:`StateChangeComplete` for
              switch ``Id = False``. After the state change has completed
              :meth:`StateChangeComplete` becomes True.
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * On is True, Off is False.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |SetAsyncValue|

                .. |SetAsyncValue| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.SetAsyncValue" target="_blank">
                    Switch.SetAsyncValue()</a> (external)

            .. only:: rinoh

                `Switch.SetAsyncValue() <https://ascom-standards.org/newdocs/switch.html#Switch.SetAsyncValue>`_
        """
        self.__put("setasyncvalue", Id=Id, Value=Value)

    def SetSwitch(self, Id: int, State: bool) -> None:
        """Set a switch to the specified state

        Args:
            Id: the specified switch number (see Notes)
            State: The required control state

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * On is True, Off is False.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |SetSwitch|

                .. |SetSwitch| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.SetSwitch" target="_blank">
                    Switch.SetSwitch()</a> (external)

            .. only:: rinoh

                `Switch.SetSwitch() <https://ascom-standards.org/newdocs/switch.html#Switch.SetSwitch>`_
        """
        self.__put("setswitch", Id=Id, State=State, tmo=60.0)

    def SetSwitchName(self, Id: int, Name: str) -> None:
        """Set a switch name to the specified value.

        Args:
            Id: the specified switch number (see Notes)
            Name: The desired (new) name for the switch

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * On is True, Off is False.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |SetSwitchName|

                .. |SetSwitchName| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.SetSwitchName" target="_blank">
                    Switch.SetSwitchName()</a> (external)

            .. only:: rinoh

                `Switch.SetSwitchName() <https://ascom-standards.org/newdocs/switch.html#Switch.SetSwitchName>`_
        """
        self.__put("setswitchname", Id=Id, Name=Name)

    def SetSwitchValue(self, Id: int, Value: float) -> None:
        """Set a switch value to the specified value.

        Args:
            Id: the specified switch number (see Notes)
            Value: Value to be set, between :attr:`MinSwitchValue` and
                :attr:`MinSwitchValue`.

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`), or
                the Value is out of range, not between :attr:`MinSwitchValue` and
                :attr:`MinSwitchValue`.
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.
            * On is True, Off is False.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |SetSwitchValue|

                .. |SetSwitchValue| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.SetSwitchValue" target="_blank">
                    Switch.SetSwitchValue()</a> (external)

            .. only:: rinoh

                `Switch.SetSwitchValue() <https://ascom-standards.org/newdocs/switch.html#Switch.SetSwitchValue>`_
        """
        self.__put("setswitchvalue", Id=Id, Value=Value, tmo=60.0)

    def StateChangeComplete(self, Id: int) -> bool:
        """True if the last :meth:`SetAsync` or :meth:`SetAsyncValue`
        has completed and the switch is in the requested state.

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            NotImplementedException: If :meth:`CanAsync` is ``False`` for switch ``Id``
            OperationCancelledException: If an in-progress state change is cancelled by a call to
                :meth:`CancelAsync` call for switch ``Id``
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |StateChangeComplete|

                .. |StateChangeComplete| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.StateChangeComplete" target="_blank">
                    Switch.StateChangeComplete()</a> (external)

            .. only:: rinoh

                `Switch.StateChangeComplete() <https://ascom-standards.org/newdocs/switch.html#Switch.StateChangeComplete>`_
        """
        return self.__get("statechangecomplete", Id=Id)

    def SwitchStep(self, Id: int) -> float:
        """The step size of the specified switch (see Notes).

        Args:
            Id: the specified switch number (see Notes)

        Raises:
            InvalidValueException: The Id is out of range (see :attr:`MaxSwitch`)
            NotConnectedException: If the device is not connected
            DriverException: An error occurred that is not described by one of the more specific ASCOM exceptions. The device did not *successfully* complete the request.

        Note:
            * Step size is the difference between successive values of the device.
            * Switches are numbered from 0 to :attr:`MaxSwitch` ``- 1``.

        .. admonition:: Master Interfaces Reference
            :class: green

            .. only:: html

                |SwitchStep|

                .. |SwitchStep| raw:: html

                    <a href="https://ascom-standards.org/newdocs/switch.html#Switch.SwitchStep" target="_blank">
                    Switch.SwitchStep()</a> (external)

            .. only:: rinoh

                `Switch.SwitchStep() <https://ascom-standards.org/newdocs/switch.html#Switch.SwitchStep>`_
        """
        return self.__get("switchstep", Id=Id)
