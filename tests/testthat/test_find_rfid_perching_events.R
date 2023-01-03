# Use the raw radio frequency identification (RFID) data to identify perching events (e.g. periods of time when an individual was perched on the RFID antenna). This function is performed for each unique passive integrated transponder (PIT) tag in the dataset. The function identifies runs of RFID detections separated by the given temporal threshold or less, then takes the first and last detection of each run and return these timestamps with start and end labels. Note that unlike the RFID pre-processing function, this function groups the data frame not only by PIT tag ID but also by date to avoid artificially long perching periods

# test 1: test that the temporal threshold is a number and an integer

# test 2: test that the raw data is a data frame

# test 3: test that the input dataset has the PIT tag ID column that will be grouped

# test 4: test that the timestamps are in the right format

# test 5: test that