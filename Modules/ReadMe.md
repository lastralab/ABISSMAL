<h1>Main Script</h1>
<p>Running all modules asynchronously and perform activities based on the values returned.</p>
<h3>Pre-requisites</h3>

- Threading: <a href="https://docs.python.org/3/library/threading.html">Thread-based parallelism</a>
  - Runs functions asynchronously
    - Source: <a href="https://github.com/python/cpython/blob/3.10/Lib/threading.py">threading.py</a>
- SMTP configuration to send email alerts from localhost

<h3>Main functions</h3>

- init_irbb()
  - starts IRBB activity
  - logs info/errors
  - contains box identification
- email_alert() _TODO: move to main_
  - sends email 
  - contains box, module and error
- csv_writer()
  - searches for filename, appends row if exists
  - creates new file and writes first csv row
  - saves file in Data folder

# GSV notes