## Grace Smith-Vidaurre
## 19 June 2023

# General documentation for data processing and integration functions"

<style type="text/css">

h1.title { /* Document title */
  font-size: 32px;
  color: black;
  font-weight: normal;
  text-align: center;
}

h1 {
   color: #0E0E7D;
   font-size: 26px;
   font-weight: normal;
}

h2 {
   color: #0E0E7D;
   font-size: 24px;
   font-weight: bold;
}

h3 { /* Document subtitle */
   color: #0E0E7D;
   font-size: 28px;
   font-weight: normal;
   text-align: center;
}

body{ /* Normal */
      font-size: 20px;
  }
  
code.r{ /* Code block */
    font-size: 20px;
}
</style>

This set of functions were written for a hardware setup in which 3 types of movement sensors are mounted around the entrance of a nest container in the following order:

- Outer pair of beam breakers (captures movement just outside of the nest container entrance)
- RFID antenna (captures movement that occurs right at the nest container entrance)
- Inner pair of beam breakers (captures movement inside the nest container, just after the nest container entrance)
- Camera (mounted on top of the nest container, captures movement inside the nest container from inside the entrance itself to further inside of the nest container)

Given the order in which these sensors are mounted, the outer pair of beam breakers and the camera should be the first and last sensors to trigger when an animal enters the nest container, respectively. When an animal leaves the nest container, this order should be inverted. However, these expectations are complicated by the fact that individuals can sit inside the circular entrance for longer periods of time (triggering not only the RFID antenna but also the beam breakers and camera with fine-scale movements), and individuals can also come in and out of the nest container with individual variation in the speed and angle at which they enter. Multiple individuals can also be inside the container, such that video recording events may be triggered by movements from more than one individual.

The functions inside this directory were written to pre-process the raw data from each sensor and integrate data among different sensors while imposing minimal assumptions about the order in which sensors triggered. There are 4 separate data integration functions (between each pair of sensor types, and among all 3 types of sensors) because the way in which the sensors were mounted determines how temporal differences between sensors are calculated for integration. Each function was written to process data for a single experimental setup (e.g. data collected over time for 1 pair of adult birds housed in 1 recording chamber). Therefore, these functions must be implemented across experimental setups if the tracking system was used to collect data for serial or parallel experimental replicates. Note that the majority of the functions are focused on processing and integrating datasets collected by movement sensors. The raw data collected by temperature probes is combined into a single spreadsheet but is not pre-processed nor integrated with other datasets.

Descriptions of each function, roughly in the order in which they should be used:

1. `combine_raw_data_per_sensor`: Combine raw data collected over time into a single spreadsheet per sensor type (RFID, beam breakers, video recording events, temperature).

2. `find_rfid_perching_events`: Identify bouts of perching events in the raw RFID data.

3. `preprocess_detections`: Pre-process the raw data by thinning the detections collected by polling (RFID) or edge detection (beam breakers), or by using the magnitude of movement to filter detections (video).

4. `label_beamBreaker_events`: Use temporal differences between pre-processed detections for the outer and inner pair of beam breakers to infer unique movement events with directionality.

5. `integrate_rfid_breamBreakers`: Integrate RFID and beam breaker datasets by matching pre-processed RFID detections to outer beam breaker timestamps in the pre-processed and labeled beam breaker dataset.

6. `integrate_rfid_video`: Integrate RFID and video datasets by matching pre-processed RFID detections to pre-processed video detections.

7. `integrate_beamBreakers_video`: Integrate beam breaker and video detection datasets by matching inner beam breaker timestamps in the pre-processed, labeled beam breaker dataset to pre-processed video timestamps.

8. `integrate_rfid_beamBreakers_video`: Integrate datasets across the three different types of movement sensors (RFID, beam breakers, video recordings). The RFID and beam breaker integration is done first (by matching the RFID and outer beam breaker timestamps). Then the video integration is performed by matching video timestamps to either the RFID timestamps or to the inner beam breaker beam breaker timestamps.

9. `detect_rfid_activityBouts`: Detect bouts of RFID activity in an integrated dataset with RFID timestamps.

See the documentation of each function for more details. The file `validators.R` contains utility functions that are used to check formal arguments and data in each of the functions above.
