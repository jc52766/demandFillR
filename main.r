library(tidyverse)
library(feather)
source("R/solver.r")

west_options_wide <- readxl::read_excel(
  "manual_data_FREETEXT.xlsx",
  sheet = "options_v"
) %>%
  distinct() %>%
  mutate(i = row_number()) %>%
  relocate(i, 1)

# west_demand_wide is the demand that the opti sees. Can differ from the actual demand.
west_demand_wide <- readxl::read_excel(
  "manual_data_FREETEXT.xlsx",
  sheet = "demand_that_opti_sees_v"
) %>%
  relocate(demand_from_date, 1) %>%
  rename(from_date = demand_from_date, to_date = demand_to_date)

# west_actual_demand_wide is the actual demand.
west_actual_demand_wide <- readxl::read_excel(
  "manual_data_FREETEXT.xlsx",
  sheet = "demand_v"
) %>%
  relocate(demand_from_date, 1) %>%
  rename(from_date = demand_from_date, to_date = demand_to_date)

df_heads <- readxl::read_excel(
  "manual_data_FREETEXT.xlsx",
  sheet = "heads_v"
)

df_avg_weight <- readxl::read_excel(
  "manual_data_FREETEXT.xlsx",
  sheet = "weight_v"
)

df_mdf <- readxl::read_excel(
  "manual_data_FREETEXT.xlsx",
  sheet = "mdf"
)

df_mde <- readxl::read_excel(
  "manual_data_FREETEXT.xlsx",
  sheet = "mde"
)

west_options_wide %>% head
west_demand_wide %>% head
df_heads %>% head
df_avg_weight %>% head
df_mdf %>% head
df_mde %>% head

### make sure columns match and are in same order
# get all unique groups
unique_groups <- unique(c(
  colnames(west_demand_wide %>% select(-from_date, -to_date)),
  colnames(west_options_wide %>% select(-i))
)) %>%
  sort()

df_all_groups <- data.frame(matrix(0, ncol = unique_groups %>% length, nrow = 1))
colnames(df_all_groups) <- unique_groups

west_demand_wide2 <- west_demand_wide %>%
  dplyr::bind_rows(df_all_groups) %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  select(c(from_date, to_date, all_of(unique_groups)))

west_options_wide2 <- west_options_wide %>%
  dplyr::bind_rows(df_all_groups) %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  select(c(i, all_of(unique_groups)))

west_actual_demand_wide2 <- west_actual_demand_wide %>%
  dplyr::bind_rows(df_all_groups) %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  select(c(from_date, to_date, all_of(unique_groups)))


identical(
  west_options_wide2 %>%
    select(all_of(unique_groups)) %>%
    colnames,
  west_demand_wide2 %>%
    select(all_of(unique_groups)) %>%
    colnames,
  # west_actual_demand_wide2 %>%
  #   select(all_of(unique_groups)) %>%
  #   colnames
)


############## solve
##############
all_l <- list() # a list to keep data for each solve

for (da_date in
     (west_demand_wide2 %>%
      select(from_date) %>%
      distinct() %>%
      drop_na() %>%
      pull())) {
  filter_demand_from_date <- da_date #as.Date('2024-12-30')
  
  heads <- df_heads %>%
    filter(demand_from_date == filter_demand_from_date) %>%
    head(1) %>%
    pull(heads)
  
  cweight <- df_avg_weight %>%
    filter(demand_from_date == filter_demand_from_date) %>%
    head(1) %>%
    pull(avg_weight_per_head)
  
  # demand = demand that opti sees
  demand <- west_demand_wide2 %>%
    filter(from_date == filter_demand_from_date) %>%
    select(all_of(unique_groups)) %>%
    head(1) %>%
    as.matrix %>%
    as.vector()
  
  # actual_demand = not demand that opti sees
  actual_demand <- west_actual_demand_wide2 %>%
    filter(from_date == filter_demand_from_date) %>%
    select(all_of(unique_groups)) %>%
    head(1) %>%
    as.matrix %>%
    as.vector()
  
  options <- west_options_wide2 %>%
    select(all_of(unique_groups)) %>%
    as.matrix %>%
    (\(.) . * cweight)()
  
  # remove any all zero option
  # which_all_zero_options <- which(0 == (options %>% rowSums()))
  # if (length(which_all_zero_options) > 0) {
  #   options <- options[-which_all_zero_options, ]
  # }
  
  mdf <- df_mdf %>%
    filter(demand_from_date == filter_demand_from_date) %>%
    select(all_of(unique_groups)) %>%
    as.numeric()
  
  mde <- df_mde %>%
    filter(demand_from_date == filter_demand_from_date) %>%
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
  plot(apply(options, 2, max), original_demand)
  
  
  start.time <- Sys.time()
  
  for (ith_head in 1:heads) {
    
    new_solved <- xsolve(
      solved = solved,
      options = options,
      demand = demand,
      dfw = mdf,
      dew = mdf
    )
    
    solved <- new_solved

  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken %>% print
  
  colSums(solved)
  original_demand
  
  plot(original_demand, colSums(solved))
  grid()
  
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

