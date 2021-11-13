**Authors**: nmoltta, gsvidaurre
**Date**: 13 November 2021

**Description**: This directory contains code to run and collect videos from a Raspberry Pi (G) fisheye lens camera. Videos will be strategically recorded in h264 format to capture parent-offspring interactions within several seconds of visits to the nest chamber. These videos are intended to be used in later projects, and complement automated tracking of parental visits to the nest chamber currently obtained by the RFID antenna and infrared beam breakers.

**Notes on usage**: Since video data is computationally intensive to record, and expensive both to store and score, videos will be recorded around parental visits to the nest chamber. Either RFID or the infrared beam breakers should trigger video recording. Ideally, video recording will capture several seconds before the given parent enters (perhaps 10 seconds), and will record for a set amount of time after the trigger (e.g. 30 or 60 seconds). This code still needs to be optimized for resolution and size given the height of the nest container. A set of manually scored videos can possibly be used for automated tracking of behavior, but since these videos will be short it may be possible to score behaviors manually with an animal behavior app.

**Data structure**: The function in Temp.py should return videos that contain the recording chamber number, and the year, month, day, and timestamp (HH:MM:SS) in the file name of each video.
