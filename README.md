# Model-based Social Determinants of Health (SDoH)

This code implements the SDoH model (obtain the estimated associations between SDoH variables and demographic covariates) presented in our paper, Model-based Estimation of Individual-level Social Determinants of Health and its Applications in All of Us, utilizing aggregated SDoH data.


## Overview

This code performs the following four steps:

1. Date preprocessing
2. Exploring associations between an SDoH and demographic covariates with a weighted mixed-effect model
3. Variable selection
4. The final SDoH model

Please note that this code leverages an aggregated SDoH dataset, downloaded from the American Community Survey (ACS), to estimate individual-level SDoH. The examples provided focus on two SDoH variables: no high school diploma (NOHSDP) and no health insurance (UNINSUR). However, the approach can be applied to a wide range of SDoH factors, enabling more comprehensive and in-depth analyses of SDoHs and their impact on health outcomes. Beyond the ACS, diverse sources of aggregated SDoH data can also be employed to estimate any target SDoH variable using this methodology. If you have any questions, please feel free to contact one of the authors of the paper.


## Preprocessing

Before reproducing Figures and tables (below), please download the 2016-2020 American Community Survey (ACS) 5-Year Data (https://www.census.gov/programs-surveys/acs) and CDC/ATSDR SVI data (https://www.atsdr.cdc.gov/placeandhealth/svi/). Please contact the authors if you have trouble downloading the data. Once you have the data, the preprocessing can be implemented using the following R script:

  - Preprocessing/Preprocessing.R


## Reproducing figures and tables of the paper

Figures and tables can be directly reproduced using R scripts located in 'NOHSDP' and 'UNINSUR' directories, corresponding to the 'no high school diploma' and 'no health insurance' SDoH variables, respectively.

1. 'No high school diploma' (in the 'NOHSDP' directory)
   - Figure 2: Estimate-association.R
   - Supplementary Figure 2: VariableSelection.R -> EstimatedCorrelationPlot.R
   - Supplementary Figure 3: VariableSelection.R -> EstimatedCorrelationPlot.R -> FitModel.R

2. 'No health insurance' (in the 'UNINSUR' directory)
   - Supplementary Figure 1: XXX.R
   - Supplementary Figure 2: XXX.R -> XXX.R
   - Supplementary Figure 3: XXX.R -> XXX.R
