# Statistical analysis of hormesis
The goal of this repository is to provide tools for analyzing hormetic dose-response relationships using both polynomial and specialized hormesis models. These models are applicable to a wide range of biological and toxicological studies where hormesis is observed. Key objectives include: 
Identifying the optimal dose range for growth stimulation. 
Quantifying Ymax (maximum stimulation as a percentage of the control), 
Mmax (dose at which the maximum response occurs), and LDS (limited dose for stimulation). 
Comparing the performance of different statistical models for analyzing hormesis.

Repository Contents
1. Cedergreen Model
File: cedergreen_model.sas

y = c + (((d - c) + (f * exp(-1/xa)) / (1 + exp(b * ln(x/e))))

2. Brain-Cousens Model
File: brain_cousens_model.sas

y = c + (((d - c) + f * x) / (1 + exp(b * ln(x/e))))

3. Quadratic Polynomial Model
File: quadratic_polynomial.R

4. Cubic Polynomial Model
File: cubic_polynomial.R

y = β0 + β1x + β2x^2 + β3x^3 + ... + βnx^n + ε

Bibliography

Belz, R. G., & Piepho, H. P. (2014). Statistical modeling of the hormetic dose zone and the toxic potency completes the quantitative description of hormetic dose responses. Environmental Toxicology and Chemistry, 34(5), 1169-1177.

Belz, R. G., & Piepho, H. P. (2012). Modeling effective dosages in hormetic dose-response studies. PloS one, 7(3), e33432.


For questions or collaborations, please contact:

Lizandro Ramírez Trejo
Email: rlizano2000@gmail.com


