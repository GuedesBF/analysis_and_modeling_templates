# so we can import the dataset
library(rio)
library(tidyverse)
library(bestglm)

# for benchmarking
library(tictoc)

# import the dataset
df_main <- rio::import("WA_Fn-UseC_-Telco-Customer-Churn.csv") |> 
    tibble()

# convert all characters to factors, and have Yes as the reference value
df_main <- 
    df_main |>
    mutate(across(where(is.character), as.factor)) |> 
    mutate(across(where(\(x) "Yes" %in% levels(x)), \(y) 
                  relevel(y, ref = "No")))

# fill nas in Total Charges variable
df_main <- 
    df_main |> 
    mutate(TotalCharges = replace_na(TotalCharges,
                                     replace = mean(TotalCharges,
                                                    na.rm = TRUE)))

# source the file that defines the model grid function
source("best_glms_grid_search.R")

# 10 predictors

# forced dimensionality reduction just for testing
# 10 variables
df_10 <- 
    df_main |> 
    select(11:21)

# sequential
tic("sequential, 10 predictors")
results_10_predictors_sequential <- 
best_subsets_grid_selection(dataset = df_10,
                            #num_workers = 7,
                            outcome = "Churn",
                            predictors = names(select(df_10, -Churn)),
                            future_plan = "sequential")
toc_sequential_10 <-
    toc()

# mclapply
tic("mclapply, 10 predictors")
results_10_predictors_mclapply <- 
    best_subsets_grid_selection(dataset = df_10,
                                #num_workers = 7,
                                outcome = "Churn",
                                predictors = names(select(df_10, -Churn)),
                                future_plan = "mclapply")
toc_mclapply_10 <-
    toc()

# multisession
tic("multisession, 10 predictors")
results_10_predictors_multisession <- 
    best_subsets_grid_selection(dataset = df_10,
                                #num_workers = 7,
                                outcome = "Churn",
                                predictors = names(select(df_10, -Churn)),
                                future_plan = "multisession")
toc_multisession_10 <-
    toc()

# best_glm for benchmarking
# data preprocessing
df_10_best <- 
    mutate(df_10, 
           Churn = ifelse(Churn == "Yes", 1, 0)) |> 
    relocate(Churn, .after = everything()) |> 
    as.data.frame()

tic("bestglm_10_predictors_exaustive")
best_10_output <- 
    bestglm(Xy = df_10_best, family = binomial, method = "exhaustive")
toc()


# forced dimensionality reduction just for testing
# 12 variables
df_12 <- 
    df_main |> 
    select(9:21)

# 12 predictors

# sequential
tic("sequential, 12 predictors")
results_12_predictors_sequential <- 
best_subsets_grid_selection(dataset = df_12,
                            #num_workers = 7,
                            outcome = "Churn",
                            predictors = names(select(df_12, -Churn)),
                            future_plan = "sequential")
toc_sequential_12 <-
    toc()

# mclapply
tic("mclapply, 12 predictors")
results_12_predictors_mclapply <- 
    best_subsets_grid_selection(dataset = df_12,
                                #num_workers = 7,
                                outcome = "Churn",
                                predictors = names(select(df_12, -Churn)),
                                future_plan = "mclapply")
toc_mclapply_12 <-
    toc()

# multisession
tic("multisession, 12 predictors")
results_12_predictors_multisession <- 
    best_subsets_grid_selection(dataset = df_12,
                                #num_workers = 7,
                                outcome = "Churn",
                                predictors = names(select(df_12, -Churn)),
                                future_plan = "multisession")
toc_multisession_12 <-
    toc()

# data preprocessing
df_12_best <- 
    mutate(df_12, 
           Churn = ifelse(Churn == "Yes", 1, 0)) |> 
    relocate(Churn, .after = everything()) |> 
    as.data.frame()

tic("bestglm_12_predictors_exaustive")
best_12_output <- 
    bestglm(Xy = df_12_best, family = binomial, method = "exhaustive")
toc()

# write results to disk
write_rds(x = mget(ls(pattern = "results")), file = "results.rds")

# write timings to disk
write_rds(x = mget(ls(pattern ="toc_")), file = "timings.rds")