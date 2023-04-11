# Telemarketing

We built a logistic regression model to predict whether a customer would subscribe to a 
bank term deposit, using the telemarketing dataset. We applied three different approaches to build the 
model:

1. Using only numerical variables and applying Principal Component Analysis (PCA) to reduce the 
dimensionality of the data before applying the logistic regression model.
2. Using dummy variables for all categorical variables and including all variables in the model.
3. Using both dummy variables for categorical variables and numerical variables, and applying PCA 
to reduce the dimensionality of the data before applying the logistic regression model.

Based on the results, we selected model 2 as the final model because it achieved the highest accuracy. 
However, we must acknowledge that this model has some limitations, as the estimated regression 
coefficients are not unique, which could be misleading. To overcome this issue, we suggest exploring 
regularization techniques such as ridge regression and lasso regression, which are beyond the scope of 
this study

Dataset - https://www.kaggle.com/datasets/aguado/telemarketing-jyb-dataset
