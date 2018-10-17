# author: Michael Jahn
# date: 2017-10-10
# coding=utf-8
"""
Connect to all gas sensors and take one measurement
"""
# loading libraries
import sys
import serial
import re
import os
import datetime as time


# GLOBAL PARAMETERS
# define output file path
if len(sys.argv) == 2 and isinstance(sys.argv[1], basestring):
    fileName = "/home/multicultivator/multicultivator/data/" + str(sys.argv[1])
else:
    fileName = "/home/multicultivator/multicultivator/data/" + time.datetime.now().strftime('%Y%m%d') + "_gas.txt"
# attached sensors in /dev
gassensors = filter(lambda x: re.match('ttyC?O2?_[0-9]', x), os.listdir('/dev'))
# throw error when no sensor is connected
if len(gassensors) == 0:
    raise ValueError('No sensors connected. Check \'tty\' devices in /dev')


# function to define and connect serial device
def connectSens(serialID):
    return serial.Serial(port=serialID, timeout=1)


# take one CO2 measurement and format ouput
def MeasureCO2(sensor):
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
    read = read + ' ' + gassensor + ' ' + time.datetime.now().strftime('%Y-%m-%d %H:%M:%S') + '\n'
    # format string as space separated text
    read = re.sub('\r\n|.Z.|.z', '', read)
    return read


# take one O2 measurement and format ouput
def MeasureO2(sensor):
    # request one measurement
    # the O2 sensor takes the following additional commands
    # T, temperature. H, humidity. B, pressure.
    # first obtain raw measurement value
    sensor.write('V\r\n')
    read1 = sensor.readline()
    # second number is digitally filtered
    sensor.write('Z\r\n')
    read2 = sensor.readline()
    # add time and date to CO2 reading
    read = read1 + ' ' + read2 + ' ' + gassensor + ' ' + time.datetime.now().strftime('%Y-%m-%d %H:%M:%S') + '\n'
    # format string as a space separated text
    read = re.sub('\r\n|[ZV].', '', read)
    return read


# LOOP OVER ALL SENSORS AND MAKE MEASUREMENT
# measurement loop
for gassensor in gassensors:
    sensor = connectSens('/dev/' + gassensor)
    # check if connection is open
    if not sensor.is_open: sensor.open()
    else: print("Connection already open")
    #
    with open(fileName, "a") as gasdata:
        if re.match('ttyCO2_[0-9]', gassensor):
            gasdata.write(MeasureCO2(sensor))
        elif re.match('ttyO2_[0-9]', gassensor):
            gasdata.write(MeasureO2(sensor))
        else: 
            print('No CO sensor implemented')
    # close connection
    sensor.close()

exit()
