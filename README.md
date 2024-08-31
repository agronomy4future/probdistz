
<!-- README.md is generated from README.Rmd. Please edit that file -->

# probdistz

<!-- badges: start -->
<!-- badges: end -->

The goal of probdistz package is to calculate the probability density function (PDF), cumulative density function (CDF), and Z-scores for specified yield columns in a dataset, optionally generating smoothed data.

□ Code summary: 

□ Code explained: https://agronomy4future.org/archives/23060

## Installation

You can install the development version of fwrmodel like so:

Before installing, please download Rtools (https://cran.r-project.org/bin/windows/Rtools)

``` r
if(!require(remotes)) install.packages("remotes")
if (!requireNamespace("probdistz", quietly = TRUE)) {
  remotes::install_github("agronomy4future/probdistz")
}
library(remotes)
library(probdistz)
```

## Example

This is a basic code to calculate environmental index:

``` r
probdistz(data, env_cols, yield_cols, smooth= TRUE)
*smooth= TRUE or FALSE, Default is TRUE

■ smooth= TRUE: the probdistz() package predicts additional data to compensate for missing values based on 6σ,
and uses this to calculate the PDF, CDF, and Z-score, resulting in a continuous probability curve.
■ smooth= FALSE: the probdistz() package calculates the PDF, CDF, and Z-score based on the actual dataset.
Therefore, if the datapoints are limited, the probability curve may not be connected.
```

## Let’s practice with actual dataset

``` r
# to uplaod data
if(!require(readr)) install.packages("readr")
library(readr)
github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/main/grains.csv"
df = data.frame(read_csv(url(github), show_col_types=FALSE))

head(df,5)
     field genotype block        stage   treatment shoot      grain_number grain_weight
1 up_state     cv_1     I pre_anthesis treatment_1 main stem  513          49.26
2 up_state     cv_1     I pre_anthesis treatment_1 tillers    268          44.68
3 up_state     cv_2     I pre_anthesis treatment_1 main stem  616          45.19
4 up_state     cv_2     I pre_anthesis treatment_1 tillers    83           44.34
5 up_state     cv_1     I pre_anthesis     control main stem  516          48.25
.
.
.
# to calculate PDF, CDF and Z-score per genotype across all environments
dataA= probdistz(df, env_cols= c("genotype"), yield_cols= c("grain_weight"), smooth= TRUE)

set.seed(100)
dataA[sample(nrow(dataA),5),]
     grain_weight   smooth_pdf smooth_cdf    smooth_z      field
1738     54.51908 1.520489e-03 0.99783357  2.85285285 down_state
503      43.71542 6.508248e-02 0.51197845  0.03003003   up_state
1382     35.34647 3.230913e-02 0.07730672 -1.42342342 down_state
1648     49.67207 1.851959e-02 0.96178377  1.77177177 down_state
985      79.18965 2.876187e-09 1.00000000  5.81981982   up_state

# to calculate PDF, CDF and Z-score per genotype, categorized by field
dataB= probdistz(df, env_cols= c("field","genotype"), yield_cols= c("grain_weight"), smooth= TRUE)

set.seed(100)
dataB[sample(nrow(dataB),5),]
     grain_weight   smooth_pdf   smooth_cdf    smooth_z      field genotype
3786     53.69313 3.036760e-04 9.996976e-01  3.42942943 down_state     cv_1
503      45.21273 6.573905e-02 5.119784e-01  0.03003003   up_state     cv_1
3430     38.00036 7.595374e-02 1.985402e-01 -0.84684685 down_state     cv_1
3696     49.72585 6.898674e-03 9.905716e-01  2.34834835 down_state     cv_1
4090     21.05160 4.985506e-07 4.091933e-07 -4.93093093 down_state     cv_3
.
.
.
```
## How is the probdistz() package efficient in creating PDF and CDF curves?

To create the PDF curve,

□ When using stat_function()

``` r
ggplot(data=subset(df, field=="up_state"), aes(x=grain_weight)) +
  
  stat_function(data=subset(subset(df, field=="up_state"), genotype=="cv_1"), 
                aes(color="cv_1"), linewidth=1, fun=dnorm, 
                args=list(mean=mean(subset(subset(df, field=="up_state"), genotype=="cv_1")$grain_weight, na.rm=TRUE),
                          sd=sd(subset(subset(df, field=="up_state"), genotype=="cv_1")$grain_weight, na.rm=TRUE))) +
  
  stat_function(data=subset(subset(df, field=="up_state"), genotype=="cv_2"), 
                aes(color="cv_2"), linewidth=1, fun=dnorm, 
                args=list(mean=mean(subset(subset(df, field=="up_state"), genotype=="cv_2")$grain_weight, na.rm=TRUE),
                          sd=sd(subset(subset(df, field=="up_state"), genotype=="cv_2")$grain_weight, na.rm=TRUE))) +
  
  stat_function(data=subset(subset(df, field=="up_state"), genotype=="cv_3"), 
                aes(color="cv_3"), linewidth=1, fun=dnorm, 
                args=list(mean=mean(subset(subset(df, field=="up_state"), genotype=="cv_3")$grain_weight, na.rm=TRUE),
                          sd=sd(subset(subset(df, field=="up_state"), genotype=="cv_3")$grain_weight, na.rm=TRUE))) +
```

□ When using probdistz()

``` r
ggplot(data=subset(dataA, field=="up_state"), aes(x=grain_weight, y=smooth_pdf, color=genotype)) +
  geom_line ()+
```
