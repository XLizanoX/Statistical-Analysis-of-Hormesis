/*
   SAS Code for Brain-Cousens Model Fitting Using PROC NLMIXED
   Author: Your Name
   Description: This script fits the Brain-Cousens non-linear model using PROC NLMIXED.
                It includes data preparation, model fitting, and weighting by the inverse
                of the variance (1/y^2). The script fits four variations of the model.
*/

/* Step 1: Load data from a CSV file */
proc import datafile='/home/mydata/data.csv'
            out=mydata
            dbms=csv
            replace;
run;

/* Step 2: Calculate the weight (gew) as the inverse of the variance (1/y^2) */
data mydata;
    set mydata;
    gew = 1 / (y_sd**2); /* gew weights observations based on their precision */
run;

/* Step 3: Fit the first Brain-Cousens model using PROC NLMIXED */
proc nlmixed data=mydata tech=newrap maxiter=1000 maxfunc=5000;
    /* Initial parameter values */
    parms c = 9.2915 b = 3.4118 d = 16.4477 e = 898.65 f = 0.01471 s2=1.0;

    /* Define the model formula */
    eta = c + (((d - c) + (f * x)) / (1 + exp(b * log(x / e))));

    /* Model y with a normal distribution, weighted by gew */
    model y ~ normal(eta, s2 / gew);
run;

/* Step 4: Recalculate gew as the inverse of the variance (1/y^2) */
data mydata;
    set mydata;
    gew = 1 / (y_sd**2); /* gew weights observations based on their precision */
run;

/* Step 5: Fit the second Brain-Cousens model using PROC NLMIXED */
proc nlmixed data=mydata tech=newrap maxiter=1000 maxfunc=8000;
    /* Initial parameter values */
    parms 
        c = 9.2915
        b = 3.4118
        d = 16.4477
        f = 0.01471
        s2 = 1.0
        ED = 1630;

    /* Define the model formula */
    eta = c + (((d - c) + (f * x)) / (1 + (((K / (100 - K)) + ((100 / (100 - K)) * (f * ED / (d - c)))) * exp(b * log(x / ED))));

    /* Model y with a normal distribution, weighted by gew */
    model y ~ normal(eta, s2 / gew);
run;

/* Step 6: Fit the third Brain-Cousens model using PROC NLMIXED */
proc nlmixed data=mydata tech=newrap maxiter=1000 maxfunc=5000;
    /* Initial parameter values */
    parms 
        c = 9.2915
        b = 3.4118
        d = 16.4477
        f = 0.01471
        s2 = 1.0
        LDS = 1000;

    /* Define the model formula */
    eta = c + (((d - c) + (f * x)) / (1 + ((f * LDS / (d - c)) * exp(b * log(x / LDS)))));

    /* Model y with a normal distribution, weighted by gew */
    model y ~ normal(eta, s2 / gew);
run;

/* Step 7: Fit the fourth Brain-Cousens model using PROC NLMIXED */
proc nlmixed data=mydata tech=newrap maxiter=1000 maxfunc=5000;
    /* Initial parameter values */
    parms 
        c = 9.2915
        b = 3.4118
        d = 16.4477
        f = 0.01471
        s2 = 1.0
        M = 600;

    /* Define the model formula */
    eta = c + (((d - c) + (f * x)) / (1 + ((f * M / (((d - c) * b) - (f * M * (1 - b)))) * exp(b * log(x / M)))));

    /* Model y with a normal distribution, weighted by gew */
    model y ~ normal(eta, s2 / gew);
run;