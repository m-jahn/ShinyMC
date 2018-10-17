# author: Michael Jahn
# date: 2017-10-11
# coding=utf-8
"""
Connect to all gas sensors and calibrate
"""
# loading libraries
import sys
import serial
import re
import os
import time
import numpy


# function to define and connect serial device
def connectSens(serialID):
    return serial.Serial(port=serialID, timeout=1)


# calibrate zero using nitrogen and take 5 test measurements
def calibrateToN2(sensor):
    # take series of 5 measurements BEFORE calibration
    print('CO2 concentration before calibration... ')
    for i in range(1,6):
        sensor.write('Q\r\n')
        # read measurement from sensor cache and print
        read = sensor.readline()
        # re-format reads
        read = re.sub('\r\n|.Z.|.z', '', read)[0:5]
        print(read)
        time.sleep(1)
    #
    # ask sensor to calibrate to N2
    sensor.write('U\r\n')
    print('Calibration done, message: ' + sensor.readline())
    time.sleep(1)
    #
    # take series of 5 measurements AFTER calibration and print mean CO2 conc
    print('Performing test measurement... ')
    reads = list()
    for i in range(1,6):
        sensor.write('Q\r\n')
        # read measurement from sensor cache and print
        read = sensor.readline()
        # re-format reads
        read = re.sub('\r\n|.Z.|.z', '', read)[0:5]
        print(read); reads.append(read)
        time.sleep(1)
    reads = [int(x) for x in reads]
    return numpy.mean(reads)


# LOOP OVER ALL CO2 SENSORS AND CALIBRATE
# function to find attached CO2 sensors in /dev
co2sensors = filter(lambda x: 'ttyCO2' in x, os.listdir('/dev'))
# throw error when no sensor is connected
if len(co2sensors) == 0:
    raise ValueError('No sensors named "ttyCO2" connected')

# calibration loop
for co2sensor in co2sensors:
    sensor = connectSens('/dev/' + co2sensor)
    # check if connection is open
    print('Connecting to sensor ' + co2sensor)
    if not sensor.is_open: sensor.open()
    else: print("Connection already open")
    #
    # zero calibrate to molecular nitrogen N2
    meanConc = calibrateToN2(sensor) 
    print('Mean CO2 after blanking: ' + str(meanConc*10) + ' ppm')
    #
    # close connection
    sensor.close()

exit()
