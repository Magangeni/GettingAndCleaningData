---
title: "Getting and Cleaning Data Course Project"
author: "Magangeni"
date: "31 December 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Run Analysis
##Introduction
The document describes how to clean and analyse the data for the project

There is one R script called run_analysis.R that contains the following functions
    a) run_analysis() - the main function
    b) get_data_set(diectory="./test", data_set="test") - the worker function that reads in     the test or training data into R. 
    c) get_complete_cases(d) - a helper function that extracts complete measurements in the        data frame d

Source Data
The user activity data are contained in identically named files in two directories: ./test and ./train. The files are  
    a) x_test.txt, x_train.txt   
    b) y_test.txt, y_train.txt
    b) subject_test.txt and subject_train.txt
The contents of the files are described in the original README.txt file

The file features.txt contains the variables (features) that were measured and recorded in the x_{test|train}.txt files. Line n in the features file corresponds to column n the X_*.txt files.

The activity_labels.txt contains descriptions of the 6 activities performed by the subjects.

##Processing
To simplify the code, the features.txt and activity_labels.txt were copied into the ~/test and ~/train directories.

To start the process, source the file "run_analysis.R" and enter the command 
    run_analysis()
This function calls the worker function calls the worker function get_data_set() to compose
  a) the test data set  (no parameters in call to get_data_set())
  b) the training data set - called as (get_data_set("./train","train"))

The two data sets are constructed so that they have identical variable names and are compbined into one data set. 

The main function then writes a CSV file (subject_activity_mean_std.csv) that contains the subject and activity ids as well as the mean and standard deviation masurements.

###Data Cleaning
The worker function attempts to remove any incomplete observations (there were none in the data set given). In addition it alters the variable names in the features.txt file so that they are valid R variable names. In particular it performs the following
    a) Removes adjacent pairs of parentheses e.g. abc-()-123 becomes abc--123
    b) Replaces left or right parentheses with an underscore e.g. abc(xyz)123 becomes  
       abc_xyz_123
    c) Removes any parentheses at the end of a variable e.g. abc(123) becomes abc_123
    d) replaces hyphens (-) and commas (,) with underscores(_)
These changes were made to ensure that variable names are not confused with function calls or arethmetic expressions.

###Merging
Each row of the data set contains the measurements for one subject performing one activity.
First the y_* data (activity id's) was merged with the activity labels data so that each activity can be easily identified as WALKING or SITTING. Then three data sets (subjects, labeled activities and measurements) are merged.

Data for the mean and standard deviations only are extracted and returned.
The main function then produces a summary(grouped by subject id and activity) of the merged data.

###Output
The fucntion writes out two CSV files, one for the unsummarised activity data set (subject_activity_mean_std.csv) and another for the summary (subject_activity_summary_mean_std.csv). It also outputs a list of variable names (varibale_names.csv) which will be described in the code book.

