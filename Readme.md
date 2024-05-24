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
Is it possible to derive the type of activity from movement data considering the attributes sinuosity, speed and visited locations? Which type of analytical concept performs best in differentiating between the different activities? 
Optional: Do the concepts perform differently for different people.

## Results / products
We expect to be able to distinguish between different types of walking-activity (commuting path, recreational, running errands) as we expect the trajectories to differ regarding sinuosity, speed and number of stops. As a result we aim to have an estimation of the performance of different analytical concepts at the task of differentiating between walking-activities. This could be presented as  misclassification tables for each analytical concept, in which we assess which concept succeeded best at discerning the different walking-activities.

## Data
We use the activity data, which Saskia documented with Strava, but require more trajectories. The Strava- data is suited for our project because of its high spatial and temporal resolution.
For getting more trajectories the following options are possible:
Option A: Saskia documents more trajectories. -> suitable if we only want to use data from one person
Option B: Sarah documents more trajectories. -> suitable if we  want to use data from several people and want to see whether the same analytical concepts differ in their capability of differing the type of walking- activities. 
Option C: use data from other member of our course -> same considerations as for Option B

## Analytical concepts
We 
Similarity measures
Raw characteristics such as sinuosity, speed and number of stops.
Visual comparison with timecubes

## R concepts
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->


## Risk analysis
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->
Our analysis depends on whether our data contains enough trajectories (see Data). Our plan B is going for travel-mode detection using ca. 2 months of Google-Timeline data, which has rather big temporal intervals. This would represent a further risk for our plan B. Adding data from another person might also add a challenge as we would need to discern between "normal" and "fast" speeds for each person.

## Questions? 
<!-- Which questions would you like to discuss at the coaching session? -->
How many trajectories do we need, to have a sufficient amount of data for our project?
Which option to get more trajectories (Data) should we use?
As the task of differentiating between different walking activities seems difficult, we are not sure about the workload of comparing the capability of the different analytical concepts for the movement- data of different people.