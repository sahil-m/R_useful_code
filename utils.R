# finalized ---------------------------------------------------------------
options(readr.show_progress = F)

skim_with_unique_percent = skim_with(character = sfl(unique_to_valid_proportion = ~round(n_distinct(., na.rm = TRUE)/sum(!is.na(.)), 4)),
                                     factor = sfl(unique_to_valid_proportion = ~round(n_distinct(., na.rm = TRUE)/sum(!is.na(.)), 4)),
                                     append = TRUE)

get_relationship_between_columns <- function(df, col1, col2, return_type="prop") {
  df_grouped_1 <-
    df %>% 
    group_by(.data[[col1]]) %>% 
    summarise("count_{col2}" := n_distinct(.data[[col2]], na.rm = T)) %>% 
    ungroup()
  
  df_grouped_2 <- 
    df %>%
    group_by(.data[[col2]]) %>%
    summarise("count_{col1}" := n_distinct(.data[[col1]], na.rm = T)) %>%
    ungroup()
  
  if (any(df_grouped_1[[glue("count_{col2}")]] > 1)) {
    if (any(df_grouped_2[[glue("count_{col1}")]] > 1)) {
      ##### many-to-many
      df_many_to_many <- 
        df %>% 
        group_by(.data[[col1]]) %>% 
        mutate("count_{col2}" := n_distinct(.data[[col2]], na.rm = T)) %>% 
        dplyr::filter(.data[[glue("count_{col2}")]] > 1) %>% 
        ungroup() %>% 
        group_by(.data[[col2]]) %>%
        mutate("count_{col1}" := n_distinct(.data[[col1]], na.rm = T)) %>%
        dplyr::filter(.data[[glue("count_{col1}")]] > 1) %>% 
        mutate(
          "count_{col1}_withManyRel" := n_distinct(.data[[col1]], na.rm = T)
        ) %>%
        ungroup()
      if (any(df_many_to_many[[glue("count_{col1}_withManyRel")]] > 0)) {
        message("many-to-many-overlapping")
      } else message("many-to-many-non-overlapping") 
    } else {
      message(glue("{col1}-to-{col2}: one-to-many"))
      if (return_type == "prop") {
        message("Proportion of 'many' variable having one-to-one: ", round(as.numeric(table(df_grouped_1[[glue("count_{col2}")]])[1])/n_distinct(df[[col2]])*100, 2))
        
        message("Proportion distribution of the 'one' variable as per its mapping-count:\n")
        round(prop.table(table(df_grouped_1[[glue("count_{col2}")]]))*100, 2)
      } else if (return_type == "count") {
        message("Count distribution of the 'one' variable as per its mapping-count:\n")
        table(df_grouped_1[[glue("count_{col2}")]])
      } else message("Output can either be in count or a proportion")
    }
  } else {
    if (any(df_grouped_2[[glue("count_{col1}")]] > 1)) {
      message(glue("{col1}-to-{col2}: many-to-one"))
      if (return_type == "prop") {
        message("Proportion of 'many' variable having one-to-one: ", round(as.numeric(table(df_grouped_2[[glue("count_{col1}")]])[1])/n_distinct(df[[col1]])*100, 2))
        
        message("Proportion distribution of the 'one' variable as per its mapping-count:\n")
        round(prop.table(table(df_grouped_2[[glue("count_{col1}")]]))*100, 2)
      } else if (return_type == "count") {
        message("Count distribution of the 'one' variable as per its mapping-count:\n")
        table(df_grouped_2[[glue("count_{col1}")]])
      } else message("Output can either be in count or a proportion")
    } else {
      message("one-to-one")
    }
  }
}


get_groups_withMultipleRows <- function(df, group_by_vars, rm_NA_in_groupVars = TRUE) {
  if (rm_NA_in_groupVars) {
    df <- drop_na(df, !!!group_by_vars)
  }
  
  groups_withMultipleRows <- df %>%
    group_by(!!!group_by_vars) %>%
    mutate(n_count = n()) %>%
    ungroup() %>%
    dplyr::filter(n_count > 1) %>%
    arrange(desc(n_count), !!!group_by_vars)
  
  if (nrow(groups_withMultipleRows) == 0)
  {
    message("No groups with multiple row") 
    return()
  } else {
    groups_withMultipleRows_collapsed <- df %>%
      group_by(!!!group_by_vars) %>%
      summarise(n_count = n()) %>%
      ungroup()
    print(table(groups_withMultipleRows_collapsed$n_count, useNA = "ifany"))
    return(groups_withMultipleRows)
  }
}
# df <- tibble(a = c(1,1,1,2,2), 
#              b = c(NA,NA,12,13,14),
#              c = c(11,12,13,14,15))
# 
# df %>% get_groups_withMultipleRows(vars(a, b), rm_NA_in_groupVars = F)

connect_to_oracle_db <- function(host, port, service_name, username, password) {
  drv <- dbDriver("Oracle")
  
  connect.string <- paste("(DESCRIPTION=",
                          "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
                          "(CONNECT_DATA=(SERVICE_NAME=", service_name, ")))", 
                          sep = "")
  
  con <-dbConnect(drv, 
                  username = username, 
                  password = password,
                  dbname=connect.string)
  
  return(con)
}


first_day_of_month_wday <- function(date) {
  lubridate::day(date) <- 1
  lubridate::wday(date)
}

week_of_month <- function(date) {
  ceiling((lubridate::day(date) + first_day_of_month_wday(date) - 1) / 7)
}

add_date_based_features <- function(df, date_field) {
  return(
    df %>% 
      mutate(date = lubridate::date({{date_field}}),
             day_of_week = wday({{date_field}}, label = TRUE, abbr = TRUE),
             day_of_month = mday({{date_field}}),
             day_of_year = yday({{date_field}}),
             week_of_month = week_of_month({{date_field}}),
             week_of_month_plain = ceiling(day_of_month/7),
             week_of_year = lubridate::week({{date_field}}),
             month = lubridate::month({{date_field}}, label = TRUE, abbr = TRUE),
             quarter = lubridate::quarter({{date_field}}),
             year = lubridate::year({{date_field}}),
             year_month = paste(year, month, sep = "_"),
             year_quarter = paste(year, quarter, sep = "_"),
             is_end_of_month = ({{date_field}} == (ceiling_date({{date_field}}, unit = "months") - days(1))),
             part_of_month = cut(day_of_month, c(0,2,29,31), c('start', 'mid', 'end')),
             is_weekend = day_of_week %in% c('Sat', 'Sun'),
             is_sunday = day_of_week == 'Sun',
             label = paste(day_of_month, month, 'week', week_of_month, day_of_week, sep = '-')
      )
  )
}

# in experiment -----------------------------------------------------------
# df <- tibble(a = c(1,1,1,2,2),
#              b = c(11,11,12,13,14),
#              c = c(11,12,13,14,15),
#              d = c(101,102,103,104,105))
# 
# ##### {{}}, ..., and vars #####
# #####
# fun_temp <- function(.data, group_vars, calc_var) {
#   .data %>% 
#     group_by(!!!group_vars) %>% 
#     summarise(sum = sum({{calc_var}}))
# }
# df %>% fun_temp(vars(a, b), c)
# 
# # vars() and !!! go together
# 
# #####
# fun_temp <- function(.data, calc_var, ...) {
#   .data %>% 
#     group_by(...) %>% 
#     summarise(sum = sum({{calc_var}}))
# }
# fun_temp(df, c, a, b)
# 
# #####
# fun_temp <- function(.data, calc_vars) {
#   .data %>% 
#     summarise(across({{calc_vars}}, sum, na.rm=T))
# }
# df %>% 
#   group_by(a, b) %>% 
#   fun_temp(c(c, d))
# 
# #####
# fun_temp <- function(.data, group_vars, calc_vars) {
#   .data %>% 
#     group_by(across({{group_vars}})) %>% 
#     summarise(across({{calc_vars}}, sum, na.rm=T, .names = "{.col}_sum"))
# }
# fun_temp(df, c(a, b), c(c, d))
# 
# #####
# fun_temp <- function(.data, group_vars, calc_vars, funs) {
#   .data %>% 
#     group_by(across({{group_vars}})) %>% 
#     summarise(across({{calc_vars}}, .fns = funs, .names = "{.col}_{fn}"))
# }
# fun_temp(df, 
#          c(a, b), 
#          c(c, d), 
#          list(mean = ~mean(.x, na.rm=T), sum = ~sum(.x, na.rm=T))
#          )
# 
# ##### writing get_relationship_between_columns in a different way #####
# get_relationship_between_columns <- function(df, col1, col2, return_type="prop") {
#   col1_str <- as_label(enquo(col1))
#   col2_str <- as_label(enquo(col2))
#   
#   df_grouped_1 <-
#     df %>% 
#     group_by({{col1}}) %>% 
#     summarise(across({{col2}}, n_distinct, na.rm=T, .names="count_{.col}")) %>% 
#     ungroup()
#   
#   df_grouped_2 <- 
#     df %>%
#     group_by({{col2}}) %>%
#     summarise(across({{col1}}, n_distinct, na.rm=T, .names="count_{.col}")) %>% 
#     ungroup()
#   
#   if (any(df_grouped_1[[paste("count", col2_str, sep="_")]] > 1)) {
#     if (any(df_grouped_2[[paste("count", col1_str, sep="_")]] > 1)) {
#       ##### many-to-many
#       df_many_to_many <- 
#         df %>% 
#         group_by({{col1}}) %>% 
#         mutate(across({{col2}}, n_distinct, na.rm=T, .names="count_{.col}")) %>% 
#         dplyr::filter(.data[[paste("count", col2_str, sep="_")]] > 1) %>% 
#         ungroup() %>% 
#         group_by({{col2}}) %>%
#         mutate(across({{col1}}, n_distinct, na.rm=T, .names="count_{.col}")) %>%
#         dplyr::filter(.data[[paste("count", col1_str, sep="_")]] > 1) %>% 
#         mutate(across({{col1}}, n_distinct, na.rm=T, .names="count_{.col}_withManyRel")) %>%
#         ungroup()
#       if (any(df_many_to_many[[paste("count", col1_str, "withManyRel", sep="_")]] > 0)) {
#         message("many-to-many-overlapping")
#       } else message("many-to-many-non-overlapping") 
#     } else {
#       message("col1_str", "-to-", "col2_str", ": one-to-many")
#       if (return_type == "prop") {
#         message("Proportion distribution of the 'one' variable as per its mapping-count:\n")
#         round(prop.table(table(df_grouped_1[[paste("count", col2_str, sep="_")]]))*100, 2)
#       } else if (return_type == "count") {
#         message("Count distribution of the 'one' variable as per its mapping-count:\n")
#         table(df_grouped_1[[paste("count", col2_str, sep="_")]])
#       } else message("Output can either be in count or a proportion")
#     }
#   } else {
#     if (any(df_grouped_2[[paste("count", col1_str, sep="_")]] > 1)) {
#       message("col1_str", "-to-", "col2_str", ": many-to-one")
#       if (return_type == "prop") {
#         message("Proportion distribution of the 'one' variable as per its mapping-count:\n")
#         round(prop.table(table(df_grouped_2[[paste("count", col1_str, sep="_")]]))*100, 2) 
#       } else if (return_type == "count") {
#         message("Count distribution of the 'one' variable as per its mapping-count:\n")
#         table(df_grouped_2[[paste("count", col1_str, sep="_")]])
#       } else message("Output can either be in count or a proportion")
#     } else {
#       message("one-to-one")
#     }
#   }
# }
# 
# get_relationship_between_columns(chargecodes_wo_capiq_mapping, CHARGECODE, SRC_PARTY_ID)
# 
# df = tibble(a_a = 1:5)
# fun_temp <- function(.data, var) {
#   .data %>% 
#     pull({{var}})
# }
# fun_temp(df, a_a)
# 
# 
# fun_temp <- function(data, var) {
#   var2 = enquo(var)
#   message(paste(as_label(enquo(var)), "sth", sep="_"))
#   message(glue("{var2}_sth"))
# }
# 
# 
# fun_temp(df, a)

# count_na <- function(df) {
#   count = sapply(df, function(x) sum(is.na(x)))
#   count_percentage = round(count/nrow(df)*100, 2)
#   
#   
#   data.frame(column_name = names(count),
#              nas = paste(count, paste0("(", count_percentage, "%)"), sep = " "))
# }

# count_na <- function(df) {
#   count = sapply(df, function(x) sum(is.na(x)))
#   count_percentage = round(count/nrow(df)*100, 2)
#   
#   
#   data.frame(column_name = names(count),
#              nas_count = count,
#              nas_percent = round(count/nrow(df)*100, 2),
#              row.names = NULL)
# }

