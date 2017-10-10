# author: Michael Jahn
# date: 2017-04-10
# coding=utf-8
"""
Connect to CO2 sensor and take one measurement
"""
# loading libraries
import sys
import serial
import re
import os
import datetime as time


# function to find attached CO2 sensors in /dev
def matchString(string):
    match = re.search('ttyCO2', string)
    if match: return string


# function to define and connect serial device
def connectSens(serialID):
    return serial.Serial(port=serialID, timeout=1)


# take one measurement and format ouput
def makeMeasure(sensor):
    # set mode of sensor according to COZIR manual
    # best mode is 2, constant measurement and response on request
    sensor.write('K 2\r\n')
    print('mode: ' + sensor.readline())
    # request one measurement
    sensor.write('Q\r\n')
    # read measurement from sensor cache
    read = sensor.readline()
    # first number is raw value, 2nd is digitally filtered
    # add time and date to CO2 reading
    read = read + ' ' + co2sensor + ' ' + time.datetime.now().strftime('%Y-%m-%d %H:%M:%S') + '\n'
    # format string as a tab separated text
    read = re.sub('\r\n|.Z.|.z', '', read)
    return read


# LOOP OVER ALL CO2 SENSORS AND MAKE MEASUREMENT
co2sensors = filter(None, map(matchString, os.listdir('/dev')))
for co2sensor in co2sensors:
    sensor = connectSens('/dev/' + co2sensor)
    # check if connection is open
    if not sensor.is_open: sensor.open()
    else: print("Connection already open")
    #
    # append measurement to standard file
    if len(sys.argv) == 2 and isinstance(sys.argv[1], basestring):
        fileName = "/home/multicultivator/multicultivator/data/" + str(sys.argv[1])
    else:
        fileName = "/home/multicultivator/multicultivator/data/" + time.datetime.now().strftime('%Y%m%d') + "_CO2.txt"
    with open(fileName, "a") as co2data:
        co2data.write(makeMeasure(sensor))
    #
    # close connection
    sensor.close()

exit()
