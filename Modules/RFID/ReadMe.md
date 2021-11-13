**Authors**: nmoltta, gsvidaurre
**Date**: 13 November 2021

**Description**: This directory contains code to run and collect data from a 125kHz CognIot radio frequency identification (RFID) reader on a Raspberry Pi computer. The original code to control the RFID reader was obtained from https://github.com/CognIot/RFID_125kHz. The RFID reader is connected to an external circular antenna that sits at the entrance of the tunnel of the nest container (before both infrared beam breakers). The reader has been set to read EM4102 passive integrated transponder (PIT) tags held in a plastic leg bands made in sizes appropriate for different songbird species. This RFID system allows for identification of the individuals that enter and leave the nest chamber, and together with the infrared beam breakers, provides automated tracking of parental care behavior.

**Data structure**: The function in RFID.py should return the recording chamber number, the PIT tag identity (unique 16-bit string), year, month, day, and timestamp (HH:MM:SS). Each of these fields will be written out as a new line of a .csv file generated per day by a separate function.
