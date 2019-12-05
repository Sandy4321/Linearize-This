Linearize This!
==============

A meta-dataset and logistic meta-model for classifying categorical variables
and interaction terms.
Concept and code by Devin Kwok.
In fulfillment of Stat 429 individual project, fall 2019, University of Calgary.

Requirements
------------
* r-base-dev for `packrat`
* CRAN package `packrat` for managing package dependencies
* CRAN package `car` for analysis using VIF and power transform

How to use
----------
1. Open an interactive R prompt in the project root directory. `packrat` should
boostrap install itself. Run `packrat::restore()` to automatically install
the other required libraries.
2. Add datasets into `datasets/`. Each dataset should be a `.csv` file readable
by the R function `read.table()`.
3. From the project root directory, run `Rscript src/main.R`. This produces the
training data and does a hard-coded prediction based on the fitted logistic model
found in `analysis/analyze-logistic-model.R`.

Overview
--------
The goal of this project is to detect linear and non-linear correlations that
indicate the types of variables present in a dataset.

Suppose a candidate variable which is strongly correlated with a response variable
is fitted as a categorical (factor) variable. Then its slopes (parameters) under linear
regression should exhibit a strong trend. On the other hand, if the predictor variable 
shows little or no correlation, then fitting it as a factor will not result in
slopes with a significant trend.

If a variable has a non-linear interaction with another predictor, there should
be a similar trend in the slopes of the variable and predictor combined as
a series of interaction terms. On the other hand if there is no interaction,
then the fitted slopes of each predictor should remain consistent due to the 
independence of the predictors.

This theory is supported by the plots in `analysis/analyze-fake-factors.R`,
where uncorrelated, linearly correlated, and interacting variables are artifically
generated and plotted as factors with other variables.

By examining statistics such as the distribution of the slopes, number of 
factor levels (or degrees of freedom), and ANOVA comparison between models
with and without the factor, it may be possible to classify the type of
correlation a variable has with others in the dataset. For grading purposes,
this project uses logistic regression to determine a decision boundary from
empirically acquired data.

A possible extension is to apply principal component analysis (PCA) to the
slope distribution across multiple predictors in order to find a vector which
maximizes the variation of the factor of interest. This vector can then be
used to rank the factor, potentially allowing the recovery of ordinal information
from categorical data.

Methodology
-----------
* Find a large number of datasets with multiple columns of both
numerical and categorical data
* Identify candidate variables as those which can be factorized
(variables can also be binned to allow factorization)
* Train pairs of linear models on a response variable and predictor (which may be null),
where the larger model either includes the candidate factor as an interaction term with
the predictor, or just includes the candidate factor when the predictor is null.
* To detect trends in the slopes unique to the larger model, calculate statistics
on the model pair, such as difference in degrees of freedom, R squared, ANOVA,
mean and median of the slope distribution, etc.
* Evaluate model pairs on test data. Use statistical tests such as Wilcoxon signed rank
test or test of proportions to determine whether there is a significant improvement in
mean squared error when the candidate variable is added as a factor/interaction.
* Fit logistic model onto this meta-dataset to predict the above.


Abstract and Presentation
-------------------------

Presentation slides: [https://docs.google.com/presentation/d/1FMwBReobji46LQtjJOflOeicIVjpNTO_eg7IEI97cFI/edit?usp=sharing](https://docs.google.com/presentation/d/1FMwBReobji46LQtjJOflOeicIVjpNTO_eg7IEI97cFI/edit?usp=sharing)

Abstract: [https://docs.google.com/document/d/1F9nop7opMhOW7LG0wTyPhvvauWH0npHwXUJJCkP9weo/edit?usp=sharing](https://docs.google.com/document/d/1F9nop7opMhOW7LG0wTyPhvvauWH0npHwXUJJCkP9weo/edit?usp=sharing)