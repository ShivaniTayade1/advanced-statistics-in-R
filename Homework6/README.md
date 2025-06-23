The goal of HW6 is to learn how to determine the number of segments to retain.

As in class 5b, you will modify the idea proposed and developed in Naik et al. (JASA, 2007) paper. You will include a "clustering penalty" as in Naik et al's Equation 5 on page 246 in addition to the parameter penalty in the AIC/BIC information criteria. To this end, code an algorithm as follows:

1. Use Kmeans algorithm (studied in class5a) to classify customers in segments k = 1, 2, ..., K.max = 10.

2. Run regression of log(price) on log(xvars) as in class 5a.

3. Then compute AIC and BIC for each regression model in clusters k = 1, ..., K.max = 10

4. Compute the modified_AIC = AIC - 2* [ Sum [n_k * log(alpha_k) ] over all k ]. Here alpha_k is the proportion of customers in segment k. Plot the modified_AIC(k) as a function of k. Pick k that minimizes the modified_AIC(k). 

5. Compute the modified_BIC = BIC - log(N)* [ Sum [n_k * log(alpha_k) ] over all k ]. Here N is the total sample size. Likewise try to find the best k* that minimizes the modified_BIC(k). 

6. Interpret elasticities from the retained model and the implied managerial insights based on these coefficients.

 
