

Need to add information about the hardware setup that these functions were written for

One beam breaker pair sits in front of an RFID antenna (outside of the container, the "outer" pair), and the other sits behind the RFID antenna (mounted for detections inside of the container, the "inner" pair). The camera records into the center of the nest container from above. 


Then a description of each function
Possibly add a function to see how much of the original detections were dropped / retained after each pre-processing and integration step?


This RFID and beam breaker integration is a separate function because the way in which the sensors are set up to detect movement determines how the lag calculations and integration should be performed. In other words, it is difficult to make a general function to integrate data collected across any two types of sensors used in the tracking system. This function was written to integrate data across 1 RFID antenna and the outer pair of beam breakers mounted around the entrance of a nest container that was designed for zebra finches. For nest container entrance events, the integration is done by finding RFID timestamps that occurred within the lower to upper thresholds after the outer beam breaker. For nest container exit events, the integration is done by finding RFID timestamps that occurred within the lower to upper thresholds before the outer beam breaker. This matching is less strict than trying to find sequences of events in which the outer beam breakers, RFID antenna, and inner beam breakers triggered in that exact order (given that the way in which birds arrive or perch in the entrance can lead to variation in this expected sequence). In other words, these events will not represent perfect sequences of outer beam breakers, then RFID, then inner beam breakers triggering, but rather, RFID detections that occurred within the expected thresholds and before or after an outer beam breaker event that was already matched to an inner beam breaker even

Applies to all functions: This function must be executed across experimental setups if the tracking system was used to collect data for serial or parallel experimental replicates.