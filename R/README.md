# General documentation for data processing and integration functions

`ABISSMAL` includes a set of functions to process raw data and integrate pre-processed data across movement sensors in order to make behavioral inferences about movement events. 

**Hardware requirements**: These functions were written for a specific hardware setup in which 3 types of movement sensors are mounted around the entrance of a nest container in the following order:

- Outer pair of beam breakers (just outside of the nest container entrance and in front of the RFID antenna)
- Radio frequency identification (RFID) antenna (in the middle of the nest container entrance, between the two pairs of beam breakers)
- Inner pair of beam breakers (just inside of the nest container entrance and behind the RFID antenna)
- Infrared camera (mounted on top of the nest container for a bird's-eye view of the nest inside)

**Descriptions of each function**:

See more detailed information about how these functions work in the accompanying methods paper as well as in the `.R` scripts for each function.

1. `combine_raw_data.R`: Combine raw data collected over time into a single spreadsheet per sensor type (RFID, beam breakers, video recording events, as well as temperature) and experimental setup.

2. `detect_perching_events.R`: Detect bouts of perching events in the raw RFID or infrared beam breaker data.

3. `preprocess_detections.R`: Pre-process the raw RFID or beam breaker data using two possible filtering modes based on temporal rules, or pre-process video data by using the magnitude of movement (e.g. the number of pixels that triggered video recording) to filter detections.

4. `find_clusters.R`: Identify clusters of detections across one or more sensors that occurred close together in time.

5. `score_clusters.R`: Generate behavioral inferences about each clusters of detection, and integrate perching events from RFID and/or beam breaker data as needed.

The 5 functions above are numbered by the order in which they should be used for data processing and analysis. All of these functions must be implemented across experimental setups if `ABISSMAL` was used to collect data across serial or parallel experimental replicates. The majority of these functions were written to process data collected across movement sensors. The function `combine_raw_data_per_sensor` concatenates raw temperature data over time, but this temperature data should not be used as input for subsequent functions. Finally, the file `utilities.R` contains utility functions that are used to check formal arguments and data in each of the functions above. 

**Usage** See code provided in the GitHub repository `gsvidaurre/ABISSMAL-methods-paper` (https://github.com/gsvidaurre/ABISSMAL-methods-paper) for examples about how these functions can be used to process and integrate data to make behavioral inferences.

**Automated unit testing**: The directory `~/tests/testthat` holds automated tests of function behavior and error handling for each of the functions above. These automated tests should be run across functions using the script `run_all_testthat_tests.Rmd` whenever one or more functions are updated. This unit testing relies on simulated datasets in order to ensure that the functions are performing as expected.

**Assumptions about sensor triggering events and behavioral inferences**: In the hardware setup above, the outer pair of beam breakers and the camera should be the first and last sensors to trigger when an animal enters the nest container, respectively. When an animal leaves the nest container, this order should be reversed. However, these assumptions are complicated by the fact that individuals can sit inside the circular entrance for longer periods of time (triggering not only the RFID antenna but also the beam breakers and camera with fine-scale movements). In addition, individuals can also enter and exit the nest container with individual variation in the speed and angle at which they enter, which can lead to failure to detect movement by one or more sensors. Finally, since multiple individuals can be inside the container at any point in time, video recording events may be triggered by movements from more than one individual.

**Using sensor triggering events to score direction**: The `score_detectionClusters` function scores the direction of movement by detecting edges or transitions in sequences of sensor labels in each cluster. For the first edge in each detection cluster, the function uses rules about which sensor triggered first in order to label direction. For instance, if a detection cluster contains the sequence of events "RFID", "RFID", "RFID", "Outer Beam Breaker", then the edge "RFID-Outer Beam Breaker" would be scored as an exit, since the RFID antenna triggered first. This directional scoring by edge detection imposes minimal assumptions about how sensor triggering events map onto the direction of movements, but still requires validation with datasets of known movement direction. 
