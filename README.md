<h1>(A)utomated (b)ehavioral tracking by (i)ntegrating (s)ensors that (s)urvey (m)ovements (a)round a target (l)ocation
</h1>
<b>Authors:</b><br>
Tania Molina- <span style="pointer-events:none;">tanismo&#64;l<span style="display:none">&nbsp;</span>astralab.com</span><br>
Grace Smith-Vidaurre, PhD - <span style="pointer-events:none">grace&#64;smith<span style="display:none">&nbsp;</span>-vidaurre.com</span>
<br>

Project wiki: https://github.com/lastralab/Abissmal/wiki

<h2>Project Overview</h2>
<i>Automated behavioral tracking by integrating sensors that survey movements around a target location</i> is an open electronics system we developed to track parental care behavior in captive zebra finches. This tracking system collects data on movement around and temperature inside a customized nest container. If you plan to use this system for research with live animals (captive or wild), you need to have the appropriate institutional protocols and permits for ethical and responsible use of animals in research. Our system can be used for parental care or other behavioral tracking in different species. You may need to adapt the software and hardware we use here for different species and research settings. Anyone is welcome to post bugs. We will do our best to respond and help with issues as they arise, with the condition that our time for helping folks to adapt this tracking system will be limited. You can also reach us by email with questions.


# Pre-requisites
Please refer to our Wiki [Set up Raspberry Pi and tracking system](https://github.com/lastralab/Abissmal/wiki/2.-Set-up-Raspberry-Pi-and-tracking-system-software) before proceeding. There are a few pre-requisites to consider before running the installation script.


# Installing Abissmal
<pre>
           ____ _____  _____ _____ __  __          _      
     /\   |  _ \_   _|/ ____/ ____|  \/  |   /\   | |     
    /  \  | |_) || | | (___| (___ | \  / |  /  \  | |     
   / /\ \ |  _ < | |  \___ \\___ \| |\/| | / /\ \ | |     
  / ____ \| |_) || |_ ____) |___) | |  | |/ ____ \| |____ 
 /_/    \_\____/_____|_____/_____/|_|  |_/_/    \_\______|
                                                  
</pre>
1. Go to desired directory to download this project (Desktop recommended)
2. Clone this repository `git clone https://github.com/lastralab/Abissmal.git`
3. From root directory `~/Desktop/Abissmal/` run:

   1. `sudo bash run_install.sh`
   2. Enter your password (we recommend to set a password for your raspberry pi)
   3. Insert setup information accordingly
   4. Raspberry pi will be restarted automatically to apply changes
   5. If you are using ssh connection, you might need to <a href="https://github.com/lastralab/Abissmal/wiki/Installation#external-drive-not-found">mount the external drive</a> 
   6. From .../Abissmal/ run `bash Main.sh` to start collecting data

## Troubleshooting

Please refer to our [Wiki](https://github.com/lastralab/Abbismal/wiki/Installation#troubleshooting)


