<h1>ABISSMAL: Automated Behavioral tracking by Integrating Sensors that Survey Movements Around a target Location
</h1>
<b>Developers:</b><br>
Tania Molina: <span style="pointer-events:none;">tanismo&#64;l<span style="display:none"></span>astralab.com</span><br>
Grace Smith-Vidaurre, PhD: <span style="pointer-events:none">gsvidaurre[at]<span style="display:none"></span>gmail.com</span>
<br>
<br>

<b>Project Wiki:</b><br>
https://github.com/lastralab/Abissmal/wiki

<h2>Tracking System Overview</h2>
ABISSMAL is a hardware and software platform for automated behavioral tracking with built-in system monitoring and error logging. ABISSMAL also provides the capacity to make behavioral inferences by integrating data across multiple types of movement sensors. We tested the tracking system by tracking parental care behaviors with captive zebra finches that raised young birds over 50 days. See the accompanying methods manuscript for more detailed information about ABISSMAL (TKTK add the preprint citation). Our Wiki page has detailed information about how to set up and build hardware, as well as software installation and troubleshooting.


<h2>Pre-requisites</h2>
Please refer to our Wiki page [Set up Raspberry Pi and tracking system](https://github.com/lastralab/Abissmal/wiki/2.-Set-up-Raspberry-Pi-and-tracking-system-software) before proceeding. There are a few pre-requisites to consider before running the installation script.


<h2>Installing ABISSMAL</h2>
<br>

<pre>
           ____ _____  _____ _____ __  __          _      
     /\   |  _ \_   _|/ ____/ ____|  \/  |   /\   | |     
    /  \  | |_) || | | (___| (___ | \  / |  /  \  | |     
   / /\ \ |  _ < | |  \___ \\___ \| |\/| | / /\ \ | |     
  / ____ \| |_) || |_ ____) |___) | |  | |/ ____ \| |____ 
 /_/    \_\____/_____|_____/_____/|_|  |_/_/    \_\_____\_|

</pre>

<ol>
<li>Clone this repository to the Pi by running <pre>git clone https://github.com/lastralab/Abissmal.git</pre></li>
<li>From the root directory `/path/to/Abissmal/` run the following commands to install and run the ABISSMAL tracking system:</li>

   <ol>

   <li>Initiate the installation script, which will prompt you to enter information <pre>sudo bash run_install.sh</pre></li>
   <li>Enter your Pi password (we strongly recommend setting a password for your Raspberry Pi)</li>
   <li>When prompted, insert the additional ABISSMAL setup information accordingly</li>
   <li>The Pi will restart automatically to apply changes</li>
   <li>If you are using an ssh connection to connect to the Pi, then you will need to <a href="https://github.com/lastralab/Abissmal/wiki/5.-Troubleshooting">mount the external hard drive</a></li>
   <li>Run command to initiate ABISSMAL and start collecting data (wihout sudo) <pre>bash Main.sh</pre></li>

   </ol>

</ol>



<h2>File Structure Overview</h2>
This repository holds the following directories and files. See the README files in each directory below for more detailed information:

<h3>3D</h3> A directory that holds 3D-printing designs for customized hardware in .stl format. Some of these files were used in earlier versions of ABISSMAL (RFID antenna holder) and are not currently used.

<h3>Modules</h3> A directory that holds software modules in Python 3 and the bash shell for automated data collection and system monitoring, as well as software for error logging. 

<h3>R</h3> A directory that contains a series of computational analyses as R functions, as well as code for unit-testing these functions.

<h3>Video Concatenation</h3> A directory that holds scripts for video concatenation with ffmpeg. This code was originally written to merge pre- and post-motion detection videos but is currently not used in ABISSMAL.

<h3>Main.sh</h3> The main script that initiates the ABISSMAL tracking system. See the Wiki page for more information.

<h3>cron.sh</h3> A script for task scheduling through cron jobs. This script is automatically used by ABISSMAL for data collection, system monitoring, and error logging to run continuously over time.

<h3>run_install.sh</h3> A script for automated installation of software dependencies, setting up cron jobs, and setting up automated text alerts through Twilio (optional, and users will need to create their own Twilio account). See the Wiki page for more information. 

<h3>Issue templates and other files</h3> The directory .github/ISSUE_TEMPLATE contains templates for different types of issues. The file .gitignore contains information about which files should be ignored (not tracked) by Git. The LICENSE file holds information about the open-access license for this repository. The README.md file holds this documentation for the main repository.

<br>

<h2>Additional Documentation</h2> The Issues tab holds open issues (bugs, new features, documentation update requests) that are in progress. You can also check out issues that were closed over time. The Projects tab has a Kanban-board style timeline that we are using for project management. The Wiki tab has 5 major sections with detailed documentation: <br />
<ol>
<li>Project Management</li>
<li>Setting up Raspberry Pi computers and ABISSMAL software</li> 
<li>Building a custom nest container</li> 
<li>Setting up sensors and other hardware</li> 
<li>Troubleshooting software and hardware issues</li>
</ol>
<h2>Citing ABISSMAL</h2>
If you are using or modifying one or more components of the ABISSMAL tracking system, then please cite the associated methods paper (the citation will be updated when the manuscript is accepted for publication) as well as this repository:

<br>
Paper citation: TKTK

<br>
Repository citation: TKTK


<h2>Reporting issues while using ABISSMAL</h2>
Anyone using ABISSMAL is welcome to post issues about bugs. We will do our best to respond and help with bugs, but our top priority will be to fix bugs that hinder data collection, system monitoring, error logging, data cleaning, and processing for the current version of ABISSMAL. 

<br>
This tracking system can be modified for different questions that require behavioral tracking, as well as for different species and/or research settings. Those interested in modifying the system for other purposes are welcome to do so independently, as long as you cite both the associated methods paper (currently a preprint), and this GitHub repository. Unless we have come to an agreement with collaborators who are adapting ABISSMAL, we will not have time to help individuals or research teams modify the tracking system. 


<h2>Contributing to ABISSMAL as an open-access tool</h2>
We have laid out collaborative roles for contributing to ABISSMAL code development in our [Wiki Project Management page](https://github.com/lastralab/Abissmal/wiki/1.-Project-Management). Contributing to ABISSMAL code development requires a local version of all components of the tracking system, including sensors and other hardware. As such, we are limiting ABISSMAL code contributions to the current developers (Tania Molina, Grace Smith-Vidaurre) and future members of the Smith-Vidaurre lab who will have access to the full set of hardware needed to test code. The lab may add additional layers of data collection and processing in future releases of ABISSMAL. We recommend that others who interested in modifying ABISSMAL fork this repository as a foundation for independent customization.

<h2>Ethics Statement</h2>
We developed and tested this tracking system with an institutional animal care and use (IACUC) protocol that was approved for captive zebra finches by Rockefeller University. If you plan to use this tracking system for research with live animals (captive or wild), then you must have your own approved institutional protocols and permits for ethical and responsible use of animals in research.

