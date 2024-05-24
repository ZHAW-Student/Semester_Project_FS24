# Proposal for Semester Project


<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github

quarto render Readme.md --to pdf
-->

**Patterns & Trends in Environmental Data / Computational Movement
Analysis Geo 880**

| Semester:      | FS24                                     |
|:---------------|:---------------------------------------- |
| **Data:**      | What type of data are you focussing on?  |
| **Title:**     | The title of your project                |
| **Student 1:** | Saskia Gianola                           |
| **Student 2:** | Sarah Wirth                              |

## Abstract 
Based on movement data (walking) we want to derive the purpose of the movement. Using the attributes sinuosity, speed and number of stops we want to find out whether an activity belongs to commuting path, is recreational or belongs to running an errand for example grocery shopping. 

## Research Questions
Is it possible to derive the type of activity from movement data considering the attributes sinuosity, speed and visited locations? Which type of analytical concept performs best in differentiating between the different activities? Do different concepts perform differentially for different people.

## Results / products
We expect to be able to distinguish between different types of activity (commuting path, recreational, running errands). The trajectories are expected to differ regarding sinuosity, speed and number of stops. 

As a result we aim to have an estimation of the performance of different analytical concepts at the task of differentiating. This could be presented as  misclassification tables for each analytical concept in which we assess which concept succeeded best at discerning the different walking-activities.


## Data
We use the activity data from Saskia and maybe another person. If we would be able to start a second data-collection with Saskia or get additional walking data from other students that would be ideal.

## Analytical concepts
We 
Similarity measures
Raw characteristics such as sinuosity, speed and number of stops.
Visual comparison with timecubes

## R concepts
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->



## Risk analysis
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->
Our analysis depends on whether our data contains enough trajectories. Therefore we might need other Strava- Data to add to our. Our plan B is going for travel-mode detection using ca. 2 months of Google-Timeline data, which has rather big temporal intervals, which would be another risk for our plan B. Adding data from another person might also add a challenge as we would need to discern between "normal" and "fast" speeds for each person.

## Questions? 
<!-- Which questions would you like to discuss at the coaching session? -->
How many trajectories do we need to have a sufficient amount of data for our project?
As the task of differentiating 