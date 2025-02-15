library(tidyverse)
library(feather)
source("R/helpers.r")
source("R/solver.r")
source("R/get_data.r")

west_options_wide <- list(
  get_options_data_xl(sn = "options1"),
  get_options_data_xl(sn = "options2"),
  get_options_data_xl(sn = "options3")
)
# west_demand_wide is the demand that the opti sees. Can differ from the actual demand.
west_demand_wide <- get_demand_data_xl(sn="demand_that_opti_sees_v")
# west_actual_demand_wide is the actual demand.
west_actual_demand_wide <- get_demand_data_xl(sn="demand_v")
df_heads <- get_heads_data_xl()
df_avg_weight <- get_weight_data_xl()
df_mdf <- get_multiplier_data_xl(sn="mdf")
df_mde <- get_multiplier_data_xl(sn="mde")

### make sure columns match and are in same order
# get all unique groups
unique_groups <- get_all_colnames_sorted(
  west_demand_wide %>% select(-from_date, -to_date),
  west_options_wide[[1]],
  west_options_wide[[2]],
  west_options_wide[[3]]
)

# force all options have a column in relevant df's
west_demand_wide2 <- force_all_options(west_demand_wide, unique_groups)
west_actual_demand_wide2 <- force_all_options(west_actual_demand_wide, unique_groups)
west_options_wide2 <- lapply(west_options_wide, function(x){ force_all_options(x, unique_groups)})

############## solve
##############
all_l <- list() # a list to keep data for each solve

for (da_date in
     (west_demand_wide2 %>%
      select(from_date) %>%
      distinct() %>%
      drop_na() %>%
      pull())) {
  filter_from_date <- da_date #as.Date('2024-12-30')
  
  heads <- df_heads %>%
    filter(from_date == filter_from_date) %>%
    head(1) %>%
    pull(heads)
  
  cweight <- df_avg_weight %>%
    filter(from_date == filter_from_date) %>%
    head(1) %>%
    pull(avg_weight_per_head)
  
  # demand = demand that opti sees
  demand <- west_demand_wide2 %>%
    filter(from_date == filter_from_date) %>%
    select(all_of(unique_groups)) %>%
    head(1) %>%
    as.matrix %>%
    as.vector()
  
  # actual_demand = not demand that opti sees
  actual_demand <- west_actual_demand_wide2 %>%
    filter(from_date == filter_from_date) %>%
    select(all_of(unique_groups)) %>%
    head(1) %>%
    as.matrix %>%
    as.vector()
  
  options_prep <- function(df){
    df %>%
      select(all_of(unique_groups)) %>%
      as.matrix %>%
      (\(.) . * cweight)()
  }
  
  options <- lapply(west_options_wide2, function(x) {options_prep(x)})
  
  # remove any all zero option
  # which_all_zero_options <- which(0 == (options %>% rowSums()))
  # if (length(which_all_zero_options) > 0) {
  #   options <- options[-which_all_zero_options, ]
  # }
  
  mdf <- df_mdf %>%
    filter(from_date == filter_from_date) %>%
    select(all_of(unique_groups)) %>%
    as.numeric()
  
  mde <- df_mde %>%
    filter(from_date == filter_from_date) %>%
    select(all_of(unique_groups)) %>%
    as.numeric()
  
  solved <- matrix(0, heads, length(unique_groups))
  evald <- 0
  ps <- 0# keep track of how many passthroughs
  
  ## loops too slow!
  ## So instead work with matrixs
  
  # create originals
  original_demand <- demand
  
  
  ### plot demand v options for each group
  # to check if there are issues re possible non-options that do have demand
  #plot(apply(options, 2, max), original_demand)
  
  
  start.time <- Sys.time()
  
  for (ith_head in 1:heads) {
    
    new_solved <- xsolve_multiple(
      solved = solved,
      options = options,
      demand = demand,
      dfw = mdf,
      dew = mde
    )
    
    solved <- new_solved

  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken %>% print
  
  colSums(solved)
  original_demand
  
  #plot(original_demand, colSums(solved))
  #grid()
  
  # to avoid date showing as number convert to yyyymmdd format
  formatted_date <- format(da_date |> as.POSIXct(), "%Y%m%d")
  
  # output solve
  solved_df <- data.frame(solved)
  colnames(solved_df) <- unique_groups
  solved_fn <- glue::glue("outputs/solve_{formatted_date}.csv")
  readr::write_csv(solved_df, solved_fn)
  
  # output demand that was to get filled
  fitted_demand_df <- data.frame(original_demand %>% matrix(nrow = 1))
  colnames(fitted_demand_df) <- unique_groups
  demand_fn <- glue::glue("outputs/fitted_demand_{formatted_date}.csv")
  readr::write_csv(fitted_demand_df, demand_fn)
  
  # output actual demand
  actual_demand_df <- data.frame(actual_demand %>% matrix(nrow = 1))
  colnames(actual_demand_df) <- unique_groups
  demand_fn <- glue::glue("outputs/actual_demand_{formatted_date}.csv")
  readr::write_csv(actual_demand_df, demand_fn)
  
  # summary of demand and fill
  fitted_demand_total <- fitted_demand_df %>%
    mutate(category = 'fitted demand') %>%
    relocate(category, 1)
  
  actual_demand_total <- actual_demand_df %>%
    mutate(category = 'actual demand') %>%
    relocate(category, 1)
  
  solve_total <- colSums(solved) %>%
    matrix(nrow = 1) %>%
    data.frame()
  colnames(solve_total) <- unique_groups
  solve_total <- solve_total %>%
    mutate(category = 'solve') %>%
    relocate(category, 1)
  
  summary_df <- fitted_demand_total %>%
    bind_rows(actual_demand_total) %>%
    bind_rows(solve_total) %>%
    mutate_if(is.numeric , replace_na, replace = 0)
  
  summary_fn <- glue::glue("outputs/summary_{formatted_date}.csv")
  readr::write_csv(summary_df, summary_fn)
  
  this_list <- list(
    'date' = da_date,
    'formatted_date' = formatted_date,
    'fitted_demand_df' = fitted_demand_df,
    'actual_demand_df' = actual_demand_df,
    'summary_df' = summary_df
  )
  all_l[[length(all_l)+1]] <- this_list
  
}

