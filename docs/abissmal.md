# Usage

Automated Behavioral tracking by Integrating Sensors that Survey Movements Around a target Location

For full documentation visit our [wiki](https://github.com/lastralab/ABISSMAL/wiki)

## Running ABISSMAL

1. `./Main.sh` - Initiate ABISSMAL to start collecting data
2. Check the [logs](logs.md) to monitor the system

## Logs

Search log files to help with troubleshooting

* `grep -nir "ERROR" /home/pi/log/` - Find errors
* `tail -f /home/pi/log/abissmal_Box_01.log` - See logs in real time using tail
* `tail -f /home/pi/log/abissmal_Box_01.log | grep "Videos recorded"` - Use specific strings to tail
* `du -s -h /media/pi/<YourDriveName>/<DataPath>/*` - Monitor external drive space:
    * `df -h /media/pi/Box_01/` - Example output:
    * <pre class="dark">
      Filesystem      Size  Used Avail Use% Mounted on
      /dev/sda1       932G   35G  897G   4% /media/pi/Box_01</pre>
* `grep -nir "INFO" /home/pi/log/` - See useful information
