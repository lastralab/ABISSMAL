<h1>Infra Red Beam Breaker - IRBB</h1>
**Authors**: nmoltta, gsvidaurre
**Date**: 22 November 2021

**Description**: This directory contains code to run and collect data from infrared beam breakers on a Raspberry Pi computer. Each beam breaker is a receiver - emitter pair. The code contained in IRBB.py was modified from from https://simonprickett.dev/using-a-break-beam-sensor-with-python-and-raspberry-pi/. The nest container design holds 2 beam breaker pairs. The lead beam breaker is the first that will be broken when a bird enters the tunnel into the nest chamber (e.g. the furthest beam breaker from the nest chamber). The rear beam breaker is the second that will be broken when a bird enters the tunnel into the nest chamber (e.g. the beam breaker nearest to the nest chamber).

**Data structure**: The function in IRBB.py should return the date and timestamp (HH:MM:SS) when an infrared beam is broken. The master script should return the recording chamber number, the beam breaker identity (lead or rear), the event label (e.g. "beam broken"), year, month, day, and timestamp (HH:MM:SS) of the event. Each of these fields will be written out as a new line of a .csv file generated per day by a separate function.

The master script sets up a pull up resistor to detect a falling edge that represents a beam break. Edges are detected by adding events rather than polling to avoid missing edges, and a bouncetime parameter ensures that only a single edge will be detected even when multiple callbacks occur in a rapid fashion.

**Software**: To use the infrared beam breakers I ran the following code in a terminal window to install libraries:

sudo pip3 install signal

The RPi.GPIO, time, and datetime libraries were already installed. 
