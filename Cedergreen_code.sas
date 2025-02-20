/* 
   SAS Code for Non-Linear Mixed Effects Modeling with Weighted Least Squares
   Author: Your Name
   Description: This script performs non-linear mixed effects modeling using PROC NLMIXED.
                It includes data preparation, model fitting, and estimation of parameters.
                The model is weighted by the inverse of the variance (1/SD^2).
*/

/* Step 1: Verify the structure of the variables */
proc contents data=mydata;
run;

/* Step 2: Import the dataset */
proc import datafile='/home/mydata/data.csv'
            out=mydata
            dbms=csv
            replace;
run;

/* Step 3: Calculate the weight (gew) as the inverse of the variance (1/SD^2) */
data mydata;
    set mydata;
    gew = 1 / (y_sd**2); /* gew weights observations based on their precision */
run;

/* Step 4: Fit the model using PROC NLMIXED */
proc nlmixed data=mydata tech=newrap maxiter=1000 maxfunc=5000;
    /* Initial values for the parameters */
    parms a=0.001 b=1.7 d=8 e=1800 f=20 s2=1.0;

    /* Constraints on the parameters */
    bounds a > 0, e > 0;

    /* Prevent logarithm of zero in x */
    if x = 0 then x = 0.01; /* Adjust x to avoid log(0) */

    /* Define the model formula */
    eta = (0 + (d - 0 + f * (exp(-1 / (x**a)))) / (1 + exp(b * log(x / e))));

    /* Model y with a normal distribution, weighted by gew */
    model y ~ normal(eta, s2 / gew);
run;

/* Step 5: Fit another model with additional parameters */
proc nlmixed data=mydata;
    parms a=0.001 b=1.7 d=8 ED110=800 LDS=1000;
    
    /* Define additional parameters and constraints */
    phi = ((10*exp(b*log(LDS))/(exp(-1/(LDS**a))/exp(-1/(ED110**a))))-(11*exp(b*log(ED110)));
    f = (1.1*(d-0)*(1+exp(b*log(ED110))/phi)-(d-0))/exp(-1/(ED110**a));
    
    bounds LDS > 0;
    bounds ED110 > 0;
    
    /* Define the model formula */
    eta = (0+(d-0+f*exp(-1/(x**a)))/(1+(exp(b*log(x))/phi)));
    
    /* Calculate hormetic dose zone (LDS/ED110) */
    dist = LDS/ED110;
    
    /* Model y with a normal distribution, weighted by gew */
    model y ~ normal(eta, s2/gew);
    
    /* Estimate the hormetic dose zone */
    estimate 'hormetic dose zone' dist;
run;

/* Step 6: Fit a third model with more parameters */
proc nlmixed data=mydata;
    parms a=0.001 b=1.7 d=8 M=200 LDS=1000; /* Arbitrary initial values */
    
    /* Define additional parameters and constraints */
    phi = ((exp(b*log(LDS/M))*exp(-1/(M**a))*((a*(M**(-a-1)))-(b/M))*exp(b*log(M)))/(((b/M)*exp(-1/(LDS**a)))-(exp(b*log(LDS/M))*exp(-1/(M**a))*(a*(M**(-a-1)))));
    f = (((d-0)*(exp(b*log(M))/phi)*(b/M))/((exp(-1/(M**a))*(a*(M**(-a-1)))*(1+(exp(b*log(M))/phi)))-(exp(-1/(M**a))*(exp(b*log(M))/phi)*(b/M)));
    
    bounds M > 0;
    bounds LDS > 0;
    
    /* Define the model formula */
    eta = (0+(((d-0)+(f*exp(-1/(x**a))))/(1+(exp(b*log(x))/phi))));
    
    /* Calculate dist2 (LDS/M) and ymax at x=M */
    dist = LDS/M;
    ymax = (0+(((d-0)+(f*exp(-1/(M**a))))/(1+(exp(b*log(M))/phi)));
    
    /* Model y with a normal distribution, weighted by gew */
    model y ~ normal(eta, s2/gew);
    
    /* Estimate ymax and dist2 */
    estimate 'ymax' ymax;
    estimate 'ymax%' (ymax*100/d);
    estimate 'dist2' dist;
run;