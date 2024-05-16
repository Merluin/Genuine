 Experiment:     	GENUINE
============================================================
 Programmer:     	Thomas Quettier
============================================================
 Date:           	16/01/2024
============================================================
 PsychoPy Version:    	v2022.1.4
============================================================
 Description:    	Discrimination task using PEDFE video database
============================================================
 Affiliation:    	UNIBO
============================================================

NOTE
------------------------
- We are using 48 clips lasting 2 sec for a total of 96 secs + 96 secs ITI + Rts reps
- Emotions are Fear,Anger,Happiness
- We measure Genuine emotion, emotion recognition, intensity evaluation


Version updating:
------------------------
16/01/2024 1.0  behavioural tasks implementation.
16/01/2024 1.1  add genuine descrimination si/no using kb
		add black screen
		modified scale for intensity from 1-5 to 0-9
16/01/2024 1.2  add new video list
		change button Felice for Gioia
		rt check for ecxel
29/01/2024 2.0  Selected clips 8 per conditions	


Folder Structure:
------------------------
- data: Directory containing raw data files from participants (.csv, .log, .psydat). The .csv files are used for data analysis.
- genuine_lastRun.py: A Python script that was executed last. Contains settings and parameters used during the last run of the experiment.
- genuine.psyexp: The PsychoPy experiment file which can be opened and edited with PsychoPy Builder.
- interface: Folder that may contain resources related to the user interface of the experiment.
- PEDFE: Directory where PEDFE video database files are stored.Includes an Excel file that lists the experiment stimuli, providing a reference for the videos used in the tasks.
- README.txt: This file, which includes descriptions and important information about the experiment and its files 


References
------------------------

Miolla, A., Cardaioli, M., & Scarpazza, C. (2023). Padova Emotional Dataset of Facial Expressions (PEDFE): A unique dataset of genuine and posed emotional facial expressions. Behavior Research Methods, 55(5), 2559-2574.
