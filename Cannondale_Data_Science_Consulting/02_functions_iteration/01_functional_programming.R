# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# FUNCTIONAL PROGRAMMING ----


install.packages("ggrepel") # ggrepel needed for text and label repel in plots

library(tidyverse)
library(lubridate)
library(tidyquant)
library(ggrepel)
library(fs)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# Make bikes_tbl
bikes_tbl <- bike_orderlines_tbl %>%
    
    distinct(model, category_1, price)


# Visualize Box Plot
bikes_tbl %>%
    ggplot(aes(x = category_1, y = price)) +
    geom_boxplot()


# Create detect_outliers()

detect_outliers <- function(x) {
    
    if (missing(x)) stop("The argument x needs a vector.")
    
    if (!is.numeric(x)) stop("The argument x must be numeric.")
    
    data_tbl <- tibble(data = x)
    
   limits_tbl <- data_tbl %>%
        summarise(
            quantile_lo = quantile(data, probs = 0.25, na.rm = TRUE),
            quantile_hi = quantile(data, probs = 0.75, na.rm = TRUE),
            IQR         = IQR(data, na.rm = TRUE),
            limit_lo    = quantile_lo - 1.5*IQR,
            limit_hi    = quantile_hi + 1.5*IQR
        )
   
   output_tbl <- data_tbl %>%
       mutate(outlier = case_when(
           data < limits_tbl$limit_lo ~ TRUE,
           data > limits_tbl$limit_hi ~ TRUE,
           TRUE ~ FALSE
       ))
   
   return(output_tbl$outlier)
}

detect_outliers()
detect_outliers("a")
detect_outliers(x)

tibble(x = x) %>%
    mutate(outlier = detect_outliers(x))

# Apply detect_outliers() to bikes_tbl
bike_outliers_tbl <- bikes_tbl %>%
    
    group_by(category_1) %>%
    mutate(outlier = detect_outliers(price)) %>%
    ungroup()


# Visualize with remove_outlers()
bike_outliers_tbl %>%
    
    ggplot(aes(category_1, price)) +
    geom_boxplot() +
    
    ggrepel::geom_label_repel(aes(label = model),
                              color = "red",
                              size = 3,
                              data = . %>%
                                  filter(outlier))



# Separate bike features function

data <- bikes_tbl

separate_bike_model <- function(data, keep_model_column = TRUE, append = TRUE) {
    
    # Append
    if (!append) {
        data <- data %>% select(model)
    }
    
    # Pipeline
    output_tbl <- data %>%
        
        # select(model) %>%
        
        # Fix typo
        mutate(model = case_when(
            model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
            model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
            model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
            TRUE ~ model
        )) %>%
        
        # separate using spaces
        separate(col     = model, 
                 into    = str_c("model_", 1:7), 
                 sep     = " ", 
                 remove  = FALSE, 
                 fill    = "right") %>%
        
        # creating a "base" feature
        mutate(model_base = case_when(
            
            # Fix Supersix Evo
            str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Fat CAAD bikes
            str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Beast of the East
            str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
            
            # Fix Bad Habit
            str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Scalpel 29
            str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
            
            # catch all
            TRUE ~ model_1)
        ) %>%
        
        # Get "tier" feature
        mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
        
        # Remove unnecessary columns
        select(-matches("model_[0-9]")) %>%
        
        # Create Flags
        mutate(
            black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
            hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
            team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
            red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
            ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
            dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
            disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
        )
    
    if (!keep_model_column) output_tbl <- output_tbl %>% select(-model)
    
    return(output_tbl)
    
}

bikes_tbl %>%
    separate_bike_model(keep_model_column = TRUE, append = TRUE) 

# SAVING AND SOURCING FUNCTIONS ----

# Create folder and file ----
fs::dir_create("00_scripts/")

path <- "00_scripts/separate_bikes_and_outlier_detection.R"
fs::ciles_create(path)

# Build and add header ----
file_header_text <- str_glue(
"
# SEPARATE BIKE MODELS AND DETECT OUTLIERS ----
    
# seperate_bike_models(): A tidy function to separate the model column into engineered features
    
# detect_outliers(): A vectorized function that detects outliers using TRUE/FALSE output
    
# Libraries ----
library(tidyverse)
"
    
)

write_lines(file_header_text, path = path)


# Add functions with dump() ----

c("separate_bike_model", "detect_outliers") %>%
    dump(file = "00_scripts/separate_bikes_and_outlier_detection.R",
          append = TRUE)


# Source function ----

rm("seperate_bike_model")
rm("detect_outliers")

source("00_scripts/separate_bikes_and_outlier_detection.R")
