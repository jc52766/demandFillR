library(tidyverse)

get_xl_data <- function(fn, sn) {
  readxl::read_excel(fn, sheet = sn)
}

get_options_data_xl <- function(fn = "manual_data_FREETEXT.xlsx", sn = "options_v") {
  get_xl_data(fn = fn, sn = sn) %>%
    distinct()
}

get_demand_data_xl <- function(fn = "manual_data_FREETEXT.xlsx", sn = "demand_v") {
  get_xl_data(fn = fn, sn = sn)
}

get_heads_data_xl <- function(fn = "manual_data_FREETEXT.xlsx", sn = "heads_v") {
  get_xl_data(fn = fn, sn = sn)
}

get_weight_data_xl <- function(fn = "manual_data_FREETEXT.xlsx", sn = "weight_v") {
  get_xl_data(fn = fn, sn = sn)
}

get_multiplier_data_xl <- function(fn = "manual_data_FREETEXT.xlsx", sn = "mdf") {
  get_xl_data(fn = fn, sn = sn)
}

