# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")



# Time Series Plot ----
# What if we wanted to approximate the 3 month rolling average with a line?
# We can use a smoother

rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))




# Data Preparation

sales_by_m_cross_country_tbl <- rolling_avg_3_tbl %>%
    
    filter(category_2 == "Cross Country Race") %>%
    
    select(month_end, total_price) %>%
    
    mutate(month_end_num = as.numeric(month_end))


sales_by_m_cross_country_tbl %>%
    
    ggplot(aes(month_end_num, total_price)) +
    geom_point() +
    geom_smooth(method = "loess", span = 0.2, se = FALSE)

# Making a loess model

fit_loess_cross_country <- sales_by_m_cross_country_tbl %>%
    
    loess(total_price ~ month_end_num, data = ., span = 0.2)

fit_loess_cross_country


# Working With Broom

fit_loess_cross_country %>%
    broom::augment() %>%
    
    
    # Visualizing results
    ggplot(aes(month_end_num, total_price)) +
    geom_point() +
    geom_line(aes(y = .fitted), color = "blue")




# Step 1: Function To Return Fitted Results ----

rolling_avg_3_tbl_nested <- rolling_avg_3_tbl %>%
    group_by(category_1, category_2) %>%
    nest()

rolling_avg_3_tbl_nested$data[[1]]


data <- rolling_avg_3_tbl_nested$data[[1]]

tidy_loess <- function(data, span = 0.2) {
    
    data_formatted <- data %>%
        select(month_end, total_price) %>%
        mutate(month_end_num = as.numeric(month_end))
    
    fit_loess <- loess(formula = total_price ~ month_end_num, 
                       data    = data_formatted, 
                       span    = span)
    
    output_tbl <- fit_loess %>%
        broom::augment() %>%
        select(.fitted)
    
    return(output_tbl)
    
}


# Step 2: Test Function on Single Element ----
rolling_avg_3_tbl_nested$data[[2]] %>%
    tidy_loess()

# Step 3: Map Function to All Categories ----

# Map Functions

loess_tbl_nested <- rolling_avg_3_tbl_nested %>%
    mutate(fitted = data %>% map(tidy_loess))

loess_tbl_nested$fitted[[1]]

loess_tbl_nested %>%
    unnest()

# Visualize Results

loess_tbl_nested %>%
    
    unnest() %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = .fitted), color = "blue", size = 2) +
    geom_smooth(method = "loess", span = 0.2) +
    facet_wrap(~ category_2, scales = "free_y")

