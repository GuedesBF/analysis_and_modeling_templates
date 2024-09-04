# This script creates the function that performs best subsets selection
# with exaustive grid search using parallel computing
# Given the name of the outcome variable as a character scalar and a vector
# of characters with the names of the predictors,
# this function:
# -creates a list of formulas
# -fits a glm for each formula
# -extracts metrics for the selection (AIC, BIC)

