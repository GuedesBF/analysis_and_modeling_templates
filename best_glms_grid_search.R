# This script creates the function that performs best subsets selection
# with exaustive grid search using parallel computing
# Given the name of the outcome variable as a character scalar and a vector
# of characters with the names of the predictors,
# this function:
# -creates a list of formulas
# -fits a glm for each formula
# -extracts metrics for the selection (AIC, BIC)

best_subsets_grid_selection <- function(dataset,
                                        num_workers = parallel::detectCores() - 1,
                                        outcome,
                                        predictors,
                                        by = "AIC",
                                        future_plan = c("sequential",
                                                        "multisession",
                                                        "multicore",
                                                        "mclapply"
                                                        )
                                        ) {
    
    library(parallel)
    library(furrr)
    library(purrr)
    library(tidyr)
    library(dplyr)
    
    # Ensure future_plan is one of the four options
    future_plan <- match.arg(future_plan)
    
    # Check arguments
    if (!is.data.frame(dataset)) {
        stop("dataset must be a data frame or coercible to a data.frame")
    }
    if (!is.character(outcome) | !is.character(predictors)) {
        stop("outcome and predictors must be character")
    }
    if (!outcome %in% colnames(dataset)) {
        stop("outcome must be the name of a potential outcome variable")
    }
    if (!all(predictors %in% colnames(dataset))) {
        stop("predictors must be a character vector with names of predictor variables")
    }
    if (!by %in% c("AIC", "BIC")) {
        stop("by must be Akaike information criterion (AIC) or Bayes information criterion (BIC)")
    }
    
    # Set up the appropriate plan for parallelization
    if (future_plan == "multisession") {
        plan(multisession, workers = num_workers)
    } else if (future_plan == "multicore") {
        plan(multicore, workers = num_workers)
    }
    
    # Generate combinations of predictors
    predictors_combinations <- map(seq_along(predictors),
                                   \(x) combn(predictors, x, simplify = FALSE)) |>
        flatten()
    
    # Create formulas for each combination
    formulas <- map(predictors_combinations,
                    \(x) reformulate(response = outcome, termlabels = x))
    
    # Function to fit the GLM and capture warnings
    model_fit <- function(formula, dataset) {
        warning_msg <- NULL
        
        fit <- tryCatch(
            {
                glm(formula = formula, data = dataset, family = "binomial")
            },
            warning = function(w) {
                warning_msg <<- w$message
                suppressWarnings(glm(formula = formula, data = dataset, family = "binomial"))
            },
            error = function(e) {
                return(NULL)
            }
        )
        
        if (!is.null(fit)) {
            output <- tibble(
                formula = list(formula),
                AIC = AIC(fit),
                BIC = BIC(fit),
                warnings = warning_msg
            )
        } else {
            output <- tibble(
                formula = list(formula),
                AIC = NA,
                BIC = NA,
                warnings = "Error in model fit"
            )
        }
        
        return(output)
    }
    
    # Define how to handle different future plans
    if (future_plan == "sequential") {
        # Use vanilla map (sequential execution)
        list_of_models <- map(formulas,
                              \(x) model_fit(x, dataset))
    } else if (future_plan == "mclapply") {
        # Use mclapply for parallel execution (Unix systems only)
        list_of_models <- mclapply(formulas,
                                   \(x) model_fit(x, dataset),
                                   mc.cores = num_workers)
    } else {
        # Use future_map for multisession/multicore parallelization
        list_of_models <- future_map(formulas,
                                     \(x) model_fit(x, dataset),
                                     .options = furrr_options(seed = TRUE))
    }
    
    # Combine results and arrange by the chosen criterion (AIC or BIC)
    list_of_models <- list_rbind(list_of_models)
    
    # Arrange by the chosen criterion (AIC or BIC)
    list_of_models |> arrange(!!sym(by))
}
