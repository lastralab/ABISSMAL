<h1>ParentalCareTracking</h1>
<b>Authors:</b><br>
Tania Molina<br>
Grace Smith-Vidaurre

Project board: https://github.com/lastralab/ParentalCareTracking/projects/1 <br>
Project wiki: https://github.com/lastralab/ParentalCareTracking/wiki

<h2>Project Overview</h2>
Software to track parental care behavior in captive birds. This project requires a nest container and collects temperature data, parental visits, and video.
<br><br>
Temperature data is collected every minute all day and night. Parental visits are tracked using radio frequency identification (RFID) and infrared beam breakers all day and night. The RFID system requires an antenna at the entrance of the nest container and leg bands on birds with PIT tags. Two beam breakers are placed behind the RFID antenna to detect direction of movement (did a bird enter or leave?) and also to provide backup data in case the RFID antenna fails. Videos are recorded for short periods of time around parental visits during the day only.
This parental care tracking system was developed and implemented for captive zebra finches (*Taeniopygia guttata*). A main script controls data collection, error/warnings documentation in logs, data transfer to a USB, and e-mail alerts.

---

Before proceeding with the installation, you should configure your email and the raspberry pi for external access.

### SSH Enable - Raspberry Pi 

Raspberry Pi OS has the SSH server disabled by default. It can be enabled manually from the desktop: (extracted from https://www.raspberrypi.com/documentation/computers/remote-access.html)

1. Launch `Raspberry Pi Configuration` from the `Preferences` menu
2. Navigate to the `Interfaces` tab
3. Select `Enabled` next to `SSH`
4. Click `OK`

### SSMTP configuration

Authorize access to your Google account (extracted from https://kb.synology.com/en-global/SRM/tutorial/How_to_use_Gmail_SMTP_server_to_send_emails_for_SRM)

1. Sign in to your [Google Account](https://account.google.com/).
2. Go to **Security** >  **Less secure app access** . Click  **Turn on access** .[^1^](https://kb.synology.com/en-global/SRM/tutorial/How_to_use_Gmail_SMTP_server_to_send_emails_for_SRM#x_anchor_id5)
   ![](https://kb.synology.com/_images/autogen/How_to_use_Gmail_SMTP_server_to_send_emails_for_SRM/1.png)![](https://kb.synology.com/_images/autogen/How_to_use_Gmail_SMTP_server_to_send_emails_for_SRM/2.png)

   ### Set up Gmail SMTP server


   1. Sign in to your SRM and go to **Control Panel** > **Notification** >  **Email** .
   2. Tick the **Enable email notifications** checkbox.
   3. Change **Service Provider** from **Gmail** to  **Custom SMTP server** .
   4. To connect to Gmail SMTP Server, please fill in the required fields according to the details below:

      * **SMTP Server** : smtp.gmail.com.
      * **SMTP Port** : 587
      * **Authentication required** : Tick the checkbox.
        * **Username** : Enter your Gmail address.
        * **Password** : Enter your Google account password.
          If you have enabled two-step verification (a.k.a., two-factor
          authentication) for your Google account, refer to [this article](https://support.google.com/mail/answer/185833?hl=) to generate an application password.
      * **Security connection (SSL/TLS) is required** : Tick the checkbox.
        * **Sender name** : Enter a desired name.
        * **Sender email** : Enter your Gmail address. Please note that if the **Sender email** differs from the **Username** above, your emails may be marked as spam.
   5. Click **Apply** to let SRM log in to your Google account.[^2^](https://kb.synology.com/en-global/SRM/tutorial/How_to_use_Gmail_SMTP_server_to_send_emails_for_SRM#x_anchor_id6)
   6. You can click **Send a test email** to check if your settings are correct.

      Notes:

      1. Less secure app access is not available for accounts with 2-step verification
         enabled. Such accounts need an application-specific password to grant
         SRM access.
      2. If SRM cannot sign in to your Google account, please make sure that you have enabled **Less secure app access** in advance. If not, please go to your [Google account](https://account.google.com/) to enable it. Then, go back to **Control Panel** to fill the **Password** field under **Authentication required** before clicking  **Apply** .

<h2>Installation</h2>

1. Run `sudo apt-get install git` in your RaspberryPi.
2. Go to desired directory to download this project.
3. Clone this repository `git clone https://github.com/lastralab/ParentalCareTracking.git`
4. From root directory `/path/to/ParentalCareTracking/` run:

   1. `sudo bash run_install.sh`
   2. Enter your password (we recommend to set a password for your raspberry pi)
   3. Insert information accordingly

### Installation Overview

The file run_install.sh will install all the packages you need to run our modules and will ask you for some information to setup the email service and ssh access:

1. ntfs-3g
2. python3
3. gparted
4. screen
5. ssmtp
6. nmap

*"Press 'Enter' to skip configuration."*

This means that if you already set up what it's asking, you can press Enter to skip.

SSMTP Configuration

1. Enter the email address to send emails from (gmail)

   * The email you will use to send email alerts to the same and/or other email addresses.
   * Example: miemail@gmail.com
2. Enter the email password

* It won't be shown in the screen, this will provide the RaspberryPi access to your email for the only purpose to use it to send email alerts.

3. Enter email(s) to send error alerts.

* You can add as many emails as you want, but they have to be sorrounded by single quotes and separated by a comma.
* Example: 'email1@email.com', 'email2@email.com'
* The provider doesn't matter, could be any valid email address.

4. Enter the new hostname

* For security reasons, we recommend to change the hostname to add a unique identity and be able to connect to it via SSH.
* Example: rasbperrypi01
* If you want to keep the default one, you should enter it, instead of skipping it.

After entering these items, the RaspberryPi will auto-restart in 5 seconds.

---


Remote Access Setup - SSH

- Change Raspberry Pi default password to avoid unwanted users
- Write all passwords in a safe place
- Follow steps from <a href="https://www.raspberrypi.com/documentation/computers/remote-access.html">Raspberry Pi Documentation - Remote Access</a>:

  - Find IP address
  - Enable SSH
- > hostname -I
  >
- > nmap -sn {hostname}/24
  >

  - > Nmap scan report for {machine} (hostname)
    >
- > ping {machine}
  >
- From remote computer

  - > ssh pi@{hostname}
    >

Troubleshooting</h2>

LED still ON after detaching all screens</h3>

- Open terminal in root folder, run:
  - > screen -r irbb
    >
  - Ctrl + C
