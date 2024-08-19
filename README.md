This readme file was generated on 2024-07-16 by Rebecca Trapp

GENERAL INFORMATION

Title of Dataset: Visual exploration and problem solving in European starlings 

First Author Information
Name: Rebecca Trapp
ORCID: 0000-0002-7052-0058
Institution: Purdue University
Address: 915 Mitch Daniels Blvd, Room G369, West Lafayette, IN 47907
Email: rtrapp@purdue.edu

Principal Investigator Information
Name: Jeffrey Lucas
ORCID:
Institution: Purdue University
Address: 915 Mitch Daniels Blvd, West Lafayette, IN 47907
Email: jlucas@purdue.edu


Date of data collection: 2022-04-29 through 2022-09-20 

Geographic location of data collection: 451 S 875 W, West Lafayette, IN 47906

Information about funding sources that supported the collection of the data: A. Lindsey Graduate Fellowship in Ecology (awarded by Biological Sciences Department, Purdue University)


SHARING/ACCESS INFORMATION

Licenses/restrictions placed on the data: none

Links to publications that cite or use the data: [link]

Links to other publicly accessible locations of the data: none

Links/relationships to ancillary data sets: none

Was data derived from another source?  no
If yes, list source(s): 

Recommended citation for this dataset: Trapp, R., Fernández-Juricic, E., and Lucas, J. [in prep] "Variation in use of visual strategies during exploration of a novel stimulus"


DATA & FILE OVERVIEW

File List: 
allboris.csv  (contains all behavioral data output by BORIS)
Condition_Data.csv  (contains trial information, video file name, and body measurements for each individual)
Folder: DLC_Output  (contains a .csv file for each video (three videos per trial, one trial per bird) -- named by video file)

Relationship between files, if important: Each file contains ID column to allow merging

Additional related data collected that was not included in the current data package: 

Are there multiple versions of the dataset? no
If yes, name of file(s) that was updated: 
Why was the file updated? 
When was the file updated? 


METHODOLOGICAL INFORMATION

Description of methods used for collection/generation of data: DeepLabCut (v. 2.3.10; Mathis et al. 2018), Behavioral Observation Research Interactive Software (BORIS; v. 8.17.1; Friard & Gamba 2016).  See [Trapp et al.] for full details.

Methods for processing the data: R code written by Rebecca Trapp and Samuel Trapp.  See [Trapp et al.] for full details.

Instrument- or software-specific information needed to interpret the data: R (v. 4.4.0), RStudio (v. 2024.04.1). Packages: tidyverse, dplyr, emmeans, factoextra, lme4, psych, afex, moments, survival, survminer, TraMineR, TraMineRextras, reshape2, nlme, rstatix, ggh4x, GGally, VGAM, tables, flextable, devtools, maditr, DescTools, ggplot2, ggsignif, ggfortify, ggbreak, ggalluvial

Standards and calibration information, if appropriate: p-cutoff of 0.6 in DeepLabCut

Environmental/experimental conditions: Included in Condition_Data.csv

Describe any quality-assurance procedures performed on the data: Spot-checked head angle accuracy with protractor on screen.

People involved with sample collection, processing, analysis and/or submission: Rebecca Trapp


DATA-SPECIFIC INFORMATION FOR: allboris.csv

Number of variables: 21

Number of cases/rows: 1114

Variable List: <list variable name(s), description(s), unit(s) and value labels as appropriate for each>
"Observation id" > name created for observation file; [initials of observer]_[metal id #]
"Observation date" > date and time observation was conducted in BORIS
"Description" > contains notes about the observation (e.g., ended early, bird is never on screen)
"Observation type" > method of conducting observation (always via media file)
"Source" > location of media file used
"Total duration" > total length of media files (in seconds)
"Media duration(s)" > length of each file separately (three videos make up one trial) (in seconds)
"FPS (frame/s)" > frames per second for each media file (always 59.940 fps)
"Box Type" > treatment puzzle box type ("Control" [renamed Blocked Control in R code], "None" [renamed Open Control in R code], "Lift", "Push", "Slide")
"Subject" > individual ID (metal band)
"Behavior" > specific behavior seen, not used in final analysis ("Start", "Probe", "Closed Peck", "Open Peck", "Pecks Box", "Foot Touch", "Lid") see supplementary materials (Trapp et al.) for ethogram
"Behavioral category" > broader definition of behavior ("Start" (start of trial), "Top Beak Touch" ("Probe", "Closed Peck", "Open Peck"), "Bottom Beak Touch" ("Pecks Box"), "Top Foot Touch" ("Foot Touch"), "Box Solve" ("Lid"))
"Modifier #1" > added detail to behavior, not used in final analysis ("Vertical", "Horizontal", and "Angled" applied to "Top Beak Touch" based on peck angle; "Obstructed" and "Unobstructed" applied to "Bottom Beak Touch" based on whether the contact was visible or inferred; "Moves" and "Opens" applied to "Box Solve" to differentiate moving the lid and fully opening it)
"Behavior type" > either Point event or State event (always point)
"Start (s)" > time that behavior started (in seconds)
"Stop (s)" > time that behavior ended (always the same as start, since there are no state events)
"Duration (s)" > duration of behavior (always NA, since there are no state events)
"Media file name" > full location name of media files used
"Image index start" > frame that behavior started
"Image index end" > frame that behavior ended (always the same as start, since there are no state events)
"Comment start" > contains any comments added regarding a specific behavior

Missing data codes: " "

Specialized formats or other abbreviations used: 


DATA-SPECIFIC INFORMATION FOR: Condition_Data.csv

Number of variables: 19

Number of cases/rows: 66

Variable List: <list variable name(s), description(s), unit(s) and value labels as appropriate for each>
"Color.ID" > the color band code for the individual
"Video" > the top camera video file name for the trial (e.g., "614" = Gh010614.mp4, Gh020614.mp4, and Gh030614.mp4 [GoPro naming scheme])
"Metal.ID" > the metal band number for the individual (used as ID in R code)
"Population" > capture date/location of individual ("9-20-I" is September 2020, Indianapolis; "7-21-I" is July 2021, Indianapolis; "12-20-L" is December 2020, West Lafayette; "4-21-L" is April 2021, West Lafayette)
"Sex" > sex of the individual, as determined by color at the base of the bill (M = male, F = female, UNK = unknown)
Lid.Type > treatment puzzle box type ("Control" [renamed Blocked Control in R code], "None" [renamed Open Control in R code], "Lift", "Push", "Slide")
"Date" > date trial occurred
"Deprivation.Time" > time food was removed from the aviary the night before the trial (24hr clock)
"Trial.Time" > time the trial started for each individual
"Bag.Weight1" > weight of the bird bag used for weighing/transport on trial day
"Trial.Weight" > weight of the bird and the bag immediately prior to its trial
"R.H." > relative humidity within the experimental area
"Temp" > temperature (Celsius) within the experimental area
"Light" > lux measure within the experimental area
"Bag.Weight2" > weight of the bird bag used for weighing prior to experiments
"Start.Weight" > baseline weight (bird and bag) before start of experiments
"Tarsus" > measurement in mm of tarsus length
"Wing" > measurement in mm of wing cord
"Bill" > measurement in mm of bill length


Missing data codes: " ", "NA"

Specialized formats or other abbreviations used: 


DATA-SPECIFIC INFORMATION FOR: [Folder: DLC_Output] 

Number of variables: 49

Number of cases/rows: between 24217 - 42421

Variable List: <list variable name(s), description(s), unit(s) and value labels as appropriate for each>
"ID" > metal ID number of individual
"frame" > frame in video (60 fps)
"[bodypart]_x" > the x-coordinate of the identified body part in the video
"[bodypart]_y" > the y-coordinate of the identified body part in the video
"[bodypart]_likelihood" > the confidence measurement (p) produced by DeepLabCut that the body part is correctly identified (values below 0.6 are removed in R code)
Body Parts in dataset: "BeakTip" (not used), "BeakMid" (mid-point of beak), "BeakBridge" (where the beak meets the forehead), "LeftNare" (not used), "RightNare" (not used), "LeftGapeline" (not used), "RightGapeline" (not used), "Chin" (not used), "LeftEye" (not used), "RightEye" (not used), "LeftShoulder" (wing joint on left), "RightShoulder" (wing joint on right), "TailTip" (not used), "Front" (box dot towards bird), "Back" (box dot towards camera), "Left" (box dot left of bird), "Right" (box dot right of bird)

Missing data codes: " "

Specialized formats or other abbreviations used: 
