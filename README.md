# bordeaux_survey_crosswalk
This repo contains all the code necessary to replicate data construction and analyses in the paper: 


Statistical harmonization of measures across studies using external data: Self-rated health and self-rated memory from a clinic study and a nationally-representative sample

We present one approach using external data to harmonize measures for two constructs: self-rated health and self-rated memory, measured differently in a memory clinic based study (using a Likert scale) and a nationally representative study (using a continuous scale).

* Dataset:  Survey from a memory clinic in Bordeaux, France from March to September 2023.    
* Measures: Self-rated health and self-rated memory measured on continuous and Likert scale.
* Methods: For each construct, we predicted the Likert measures with multinomial and ordinal logistic models using varying specifications of the continuous measures (linear or spline) and varying covariate sets, including question order, age, sex/gender, and interaction terms between the continuous measure and question order, age, and sex/gender. We compared performance, fit, and Cohen’s weighted kappa values for these models with different sets of predictors to identify the best models to harmonize the Likert measure and continuous measure for each construct. Analyses were run using `R 4.1.3` within The Dementia Platform UK (DPUK) Data Portal. The scripts were reviewed by the DPUK team and exported from the portal.

