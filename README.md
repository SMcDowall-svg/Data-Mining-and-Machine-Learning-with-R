# Data-Mining-and-Machine-Learning-with-R

A group project completed with my classmates. We use billing data to predict patient expenses.  We took a data set of medical documents from Singapore for the period of
January 1, 2011 to December 28, 2015. 

Our first step was to create an appropriate dataset to begin our analysis. We combined four tables to create a table with 13600 rows and 38 columns. The dataset was processed to replace missing values using the k-nearest algorithm, while some entries were corrected for coding errors or typos.
typing errors.

Next, we began our exploratory analysis with a correlation matrix and simplistic but powerful simplistic but powerful graphs. These graphs told us that a nonlinear model
would work better than a linear model. However, we still tested both techniques.

A set of supervised and unsupervised learning algorithms were tested on the dataset and the best performing models are presented in this report. In the end, we kept the multiple linear regression models, and the decision tree. The variables for the supervised learning were chosen by a principal component analysis (PCA) for the reduction of (PCA) for dimension reduction. This was important because we had a large number of variables. Without the dimensionality reduction, we would have an artificially inflated R², which was proven by the which was proven by experimentation. 

We also tried the logarithmic transformation transformation in all models and got worse results. Finally, we settled for a contented ourselves with a normalization of the numerical data.

In the end, the nonlinear decision tree algorithm outperformed the multiple linear regression with 62% accuracy. This is due to the country tested, namely Singapore. Having universal health care implies highly regulated cost control. Therefore, a predictive model using patient data would not be sufficient to predict costs.
