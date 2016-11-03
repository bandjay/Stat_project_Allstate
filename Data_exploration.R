#####  Script for Data exploration  ##############
## Explore numeric features , find out principal components and correlation analysis, 
     # check for feasible transformations w.r.to response.
## Explore categorical variables , check for chi-square independence, 
     # anova model for vars with few levels, vars with many levels may require special attention.
     # try merging several vars and see if it makes sense.
##  outlier and influence diagnostics .
## try different feature intercations such as x1+x2  ,x1/x2, x1*x2 w.r.to response.
## reserve 10% of data for validation, dont fit any model for this data.
## Try both bootstrap and CV for regularization.
##  Training dataset has 580 k rows approx...better use 'readr' package for loading data it is faster
## for exploration 'dplyr' package is faster with large data.
##  Apart from CARET we can use H20 package which has nice models that can do distributed modelling(faster)
