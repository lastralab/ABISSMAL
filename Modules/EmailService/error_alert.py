# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/7/21

import smtplib
from email.message import EmailMessage


def email_alert(toemail, module, text):
    msg = EmailMessage()
    msg.set_content(text)
    msg['Subject'] = f'PyAlert: {subject}'
    msg['From'] = ''  # smtp setup email from
    msg['To'] = toemail

    s = smtplib.SMTP('localhost')
    s.send_message(msg)
    s.quit()
