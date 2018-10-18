# author: Michael Jahn
# date: 2018-10-18
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


# GLOBAL PARAMETERS
# attached sensors in /dev
gassensors = filter(lambda x: re.match('ttyC?O2?_[0-9]', x), os.listdir('/dev'))
# throw error when no sensor is connected
if len(gassensors) == 0:
    raise ValueError('No sensors connected. Check \'tty\' devices in /dev')


# function to define and connect serial device
def connectSens(serialID):
    return serial.Serial(port=serialID, timeout=1)


# calibrate to zero using nitrogen and take 5 test measurements
def calibrateToN2(sensor, read_command):
    # take series of 5 measurements BEFORE calibration
    print('gas concentration before calibration... ')
    for i in range(1,6):
        sensor.write(read_command)
        # read measurement from sensor cache and print
        read = sensor.readline()
        # re-format reads
        read = re.sub('\r\n|.?Z.|.z', '', read)[0:5]
        print(read)
        time.sleep(1)
    #
    # ask sensor to zero-calibrate with N2
    sensor.write('U\r\n')
    print('Calibration done, message: ' + sensor.readline())
    time.sleep(1)
    #
    # take series of 5 measurements AFTER calibration and print mean CO2 conc
    print('Performing test measurement... ')
    reads = list()
    for i in range(1,6):
        sensor.write(read_command)
        # read measurement from sensor cache and print
        read = sensor.readline()
        # re-format reads
        read = re.sub('\r\n|.?Z.|.z', '', read)[0:5]
        print(read); reads.append(read)
        time.sleep(1)
    reads = [int(x) for x in reads]
    return numpy.mean(reads)


# calibration loop
for gassensor in gassensors:
    sensor = connectSens('/dev/' + gassensor)
    # check if connection is open
    print('Connecting to sensor ' + gassensor)
    if not sensor.is_open: sensor.open()
    else: print("Connection already open")
    #
    # checking type of gassensor to use correct commands
    if re.match('ttyCO2_[0-9]', gassensor):
        read_command='Q\r\n'
    else:
        read_command='Z\r\n'
    #
    # zero calibrate to molecular nitrogen N2
    meanConc = calibrateToN2(sensor, read_command) 
    print('Mean CO2 after blanking: ' + str(meanConc*10) + ' ppm')
    #
    # close connection
    sensor.close()

exit()
