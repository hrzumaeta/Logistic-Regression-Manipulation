# logistic-regression-manipulation

The purpose of this analysis is to show how one can manipulate a logistic regression product in order to find the values for a variable which can give the regression a user defined probability of an outcome (the response variable).

The data used is from the mtcars dataset of R.

The response variable is "Efficiency", which is a classification of whether a vehicle is deemed fuel efficient or not.
A logistic regression is run on this response variable and select other predictor variables (mpg, qsec, disp, vs, drat, carb, type). 

The regression is then given a probability of efficiency, set by the user, and then the values (in this case weight) can be changed to be able to return that probability, given all other predictor variables remain constant.
