# Machine Learning Final Project
Nick Milikich
PSY 60122 Machine Learning for Social & Behavioral Research
December 2019

This project is an analysis of the factors that affect the performance of dialysis clinics in the US. There has been much study into factors at the patient level that lead to frequent hospitalizations, but little at the facility level. This study aims to do so by analyzing a previously compiled dataset of data from various reports, by variable selection, influence, and interactions. It was completed to fulfill the requirements for PSY 60122 Machine Learning for Social & Behavioral Research at the University of Notre Dame.

The project is contained in DialysisCare.R, and can be run by simply executing this one file. Also included in this repository are MilikichPresentation.pptx and MilikichProject.docx, which are fuller reports on the methodology and conclusions of this analysis.

Running this code requires that the R packages glmnet, partykit, caret, grf, and dismo be installed.

To run the source code, the following should be included in the same directory (included in this repository):
   - A file named DialysisCareQualityData.csv that contains the raw dialysis clinic data in the form of a csv file, obtained from Kaggle (does not appear to be available anymore).

This project was run using R version 4.0.3.
