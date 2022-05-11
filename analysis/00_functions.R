# Functions ----
# read_column_type: read first row of csv to determine column data type by name
read_column_type = function(file){
  
  first_row = read_csv(
    file,
    n_max = 1,
    col_names = FALSE,
    col_types = cols(.default = "c")
  )
  
  type_string = tibble(
    column_names = c(first_row)) %>% 
    mutate(column_type = case_when(
      column_names == "patient_id" ~ "i",
      str_detect(column_names, "_date") ~ "D",
      str_detect(column_names, "imd_") ~ "i",
      str_detect(column_names, "age") ~ "d",
      str_detect(column_names, "_count") ~ "i",
      TRUE ~ "c"
    )) %>%
    pull(column_type) %>%
    paste(collapse = "")
  
  return(type_string)
}

# plot_hist: plot histogram given data and variable name
plot_hist = function(data, x, path = here::here("output"),
                     fill = NULL, bins = 50, my_theme = theme_bw()){
  if(is.null(fill)){
    ggplot(data = data, aes_string(x = x)) + 
      geom_histogram(bins = bins) +
      my_theme
  } else {
    ggplot(data = data, aes_string(x = x, fill = fill)) + 
      geom_histogram(bins = bins) +
      my_theme
  }
  ggsave(paste0("plot_hist_", x, ".jpeg"),
         plot = last_plot(),
         device = "jpeg",
         path = path)
}

# weekly_count_date: weekly count of date variables in data  
weekly_count_date = function(data, var, start_date = NULL, end_date = NULL, 
                        period = "week"){
  
  if(is.null(start_date)) {
    start_date = data %>% pull(all_of(var)) %>% min(na.rm = FALSE)
  } else if (is.character(start_date)){
    start_date = ymd(start_date)
  }
  
  if(is.null(end_date)) {
    end_date = data %>% pull(all_of(var)) %>% max(na.rm = FALSE)
  } else if (is.character(end_date)){
    end_date = ymd(end_date)
  }
  
  tibble(
    date = seq(start_date, end_date, by=period) %>%
      floor_date(period) %>% 
      rep(length(var)),
    name = rep(var, each = length(week)/length(var))) %>% 
    left_join(
      data_admissions %>%
        select(all_of(var)) %>% 
        rownames_to_column() %>% 
        pivot_longer(-rowname) %>% 
        mutate(date = floor_date(value, period)) %>% 
        group_by(name) %>% 
        count(date),
      by = c("date", "name")
    ) %>% 
    replace_na(list(n = 0))
} 