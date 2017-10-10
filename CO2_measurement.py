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
import datetime as time

def connectSens(serialID):
    # define and connect serial device, name must be known
    return serial.Serial(port=serialID, timeout=1)

co2sensor = connectSens('/dev/ttyCO2_1')
# check if connection is open
if not co2sensor.is_open: co2sensor.open()
else:
    print("Connection already open")


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
    read = read + ' ' + time.datetime.now().strftime('%Y-%m-%d %H:%M:%S') + '\n'
    # format string as a tab separated text
    read = re.sub('\r\n|.Z.|.z', '', read)
    return read

# append measurement to standard file
if len(sys.argv) == 2 and isinstance(sys.argv[1], basestring):
    fileName = "/home/multicultivator/multicultivator/data/" + str(sys.argv[1])
else:
    fileName = "/home/multicultivator/multicultivator/data/" + time.datetime.now().strftime('%Y%m%d') + "_CO2.txt"
with open(fileName, "a") as co2data:
    co2data.write(makeMeasure(co2sensor))

# close connection
co2sensor.close()
exit()
