# Model-based Social Determinants of Health (SDoH)

This repository contains the code for implementing the Model-based Estimation of Individual-level SDoH as presented in our paper, "Model-based Estimation of Individual-level Social Determinants of Health and its Applications in All of Us".


## Overview

The code performs the following five steps:

1. Data preprocessing
2. Exploring associations between an SDoH and demographic covariates with a weighted mixed-effect model
3. Variable selection
4. The final SDoH model
5. (Calibrated) Model-based SDoH Prediction

This code leverages an aggregated SDoH dataset, downloaded from the American Community Survey (ACS), to estimate individual-level SDoH. The examples provided focus on two SDoH variables: no high school diploma (NOHSDP) and no health insurance (UNINSUR). However, the approach can be applied to a wide range of SDoH factors, enabling comprehensive and in-depth analyses of SDoHs and their impact on health outcomes. Beyond the ACS, diverse sources of aggregated SDoH data can also be employed to estimate any target SDoH variable using this methodology. If you have any questions, please feel free to contact one of the authors of the paper.


## Preprocessing

Please download the 2016-2020 American Community Survey (ACS) 5-Year Data (https://www.census.gov/programs-surveys/acs). Once you have the data, preprocessing can be implemented using the following R script:

  - Preprocessing/Preprocessing.R


## Steps 2-4 & Reproducing figures and tables of the paper

Steps 2-4 can be implemented using R scripts located in the 'NOHSDP' and 'UNINSUR' directories, corresponding to the 'no high school diploma' and 'no health insurance' SDoH variables, respectively. Figures and tables will be directly reproduced.

1. 'No high school diploma' (in the 'NOHSDP' directory)
   - Figure 2: EstimateAssociation.R &rarr; AssociationHeatmap.R
   - Supplementary Figure 2(a): VariableSelection.R &rarr; EstimatedCorrelationPlot.R
   - Supplementary Table 2: VariableSelection.R &rarr; EstimatedCorrelationPlot.R &rarr; FitModel.R

2. 'No health insurance' (in the 'UNINSUR' directory)
   - Supplementary Figure 1: EstimateAssociation.R &rarr; AssociationHeatmap.R
   - Supplementary Figure 2(b): VariableSelection.R &rarr; EstimatedCorrelationPlot.R
   - Supplementary Table 3: VariableSelection.R &rarr; EstimatedCorrelationPlot.R &rarr; FitModel.R


## (Calibrated) Model-based SDoH Prediction

For individual-level prediction, we provide a R script 'SDoHPrediction.R' located in 'Prediction' directory. You will need your own individual-level data with demographic information, as 'All of Us' data used in our paper is not publicly available.