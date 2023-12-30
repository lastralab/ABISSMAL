<h1>ABISSMAL R vignettes
</h1>
<b>Developers:</b><br>
Grace Smith-Vidaurre, PhD: <span style="pointer-events:none">gsvidaurre[at]<span style="display:none"></span>gmail.com</span>
<br>
<br>

<h2>Overview</h2>

The goal of these tutorials is to disseminate basic R coding skills in a biological context through the ABISSMAL behavioral tracking system. I want to have 5 short tutorials in English and Spanish. I want an Rmd file per tutorial in each language, plus an accompanying video of the screen as I work through each one (ideally I'll have a script of what I'll say per video too).

1. Introduction to RStudio, download ABISSMAL GitHub repository, introduction to the ABISSMAL data processing / analysis workflow

- Need to add to the Intro how to report bugs in vignettes as GitHub issues
- Also add how to troubleshoot when an expression isn't complete in the console
- Tab completion, keyboard shortcuts

2. Setup a virtual workspace (global environment, package installation, working directory, RMarkdown files)

3. Create simulated data, including entrances, exits, and perching events

4. Combine the raw data and pre-process the raw data, detect perching events

5. Make barcode style visualizations of the raw and per-processed data

- For vignette 05, I want to make 3 barcode style visuals: 1) the raw combined RFID data, perching events and RFID pre-processed,2) IRBB raw combined and pre-processed, then 3) pre-processed RFID and IRBB together. All of these will be faceted plots with ggplot 

6. Detect clusters, score clusters, generate summary statistics

Things that would be important to add:
- Loops
- Errors and troubleshooting online
- Color code chunks that are for testing code versus chunks that are for creating necessary objects (https://bookdown.org/yihui/rmarkdown-cookbook/chunk-styling.html)

I want to make a pre- and post-vignette Google form to asses the content and style of the vignettes for disseminating basic R coding skills in a biological context

Note that code for processing and analyzing biological data collected from zebra finches, and code to make figures, is published online as part of a submitted manuscript for the ABISSMAL tracking system.

Also note that the knitted output for these tutorials must be visible from the ABISSMAL website. This is especially important for accessibility of the Introduction for people who haven't downloaded the ABISSMAL repo yet