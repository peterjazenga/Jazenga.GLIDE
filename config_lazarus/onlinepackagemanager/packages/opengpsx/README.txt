Introduction
==========
  This component has been developed by Lazarus, intended to be opensource.

Overview
=======
  This program is a Lazarus component to demonstrate the real-time tracking and monitoring NMEA-183 compliant from GPS receiver and decode NMEA sentences likes :
- $GPGGA (Global Positioning System Fix Data)
- $GPGSA (DOP and Active Satellites)
- $GPGSV (Satellites in View)
- $GPRMC (Recommended Minimum Specific GNSS Data)

  Decoded parameters include: UTC time, date, position (lattitude, longitude), altitude, speed, course, heading and information about visible satellites.

  I also write the new components TNMEADecode, TGPSSkyplot & TGPSTarget which you can port to another application easily.
  1. TGPSNMEADecode, this non-visaul component encapsulated the codes for decoding NMEA sentence and create events which are fired when the GPS reading is enable.
    -OnGGA event for GGA data.
    -OnGSV event for GSV data.
    -OnGSA event for GSA data.
    -OnRMC event for RMC data.
  plus
    -OnGSVInfos event needed for TGPSSkyplot.

  2. TGPSSkyPlot use to display sky plot for GPS geometry with the flickerless moving of GPS legend.

  3. TGPSTarget use to draw arrow to the assigned target point by user and also display azimuth and distance.

  -The sample provides an interface over serial ports COM1 - COM4.
----------------------------------------------------------------------------------------------
