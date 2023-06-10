library(tidyverse)
library('dplyr')
library("readxl")
library('mFilter')
library(quantmod)
library(zoo)
## UPLOAD THE DATASET ##

data = read_xlsx("C:/Users/lucap/Documents/GitHub/dem_ps/dem_data.xlsx")

## SOME VARIABLES ARE MISTAKENLY CATEGORIZED AS CHARACTERS, BECAUSE THEY HAVE A COMMA##
#substitute the comma with a dot

columns_to_convert <- c("c_durables",
                        "c_nondurables",
                        "avg_hr_man",
                        "int_rate", 
                        "avg_rwage",
                        "avg_nwage",
                        "cpi")


for (col in columns_to_convert) {
  data[[col]] <- as.numeric(gsub(",", ".", data[[col]], fixed = TRUE))
}

#there are some other variables that are categorizes as "characters", even without commas

columns_to_convert <- c("empl","gdp_cap")

for (col in columns_to_convert) {
  data[[col]] <- as.numeric(data[[col]])
}

# Let's transform everything into logs
# Isolate the cols we want to transform

exclude_columns <- c("date", "int_rate", "cpi", "d_tfp")
subset_df <- data[, setdiff(names(data), exclude_columns)]

#apply a log transformation
log_df <- as.data.frame(lapply(subset_df, log))

#add back the columns we need, but we didn't need to transform

log_df$int_rate = data$int_rate
log_df$d_tfp = data$d_tfp
log_df$date = data$date

# move date back to the first column because it looks weird otherwise
# Move 'column_name' to the first position
log_df <- log_df[, c("date", setdiff(names(log_df), "date"))]


# Convert the character date variable to a date variable
log_df$date <- as.yearqtr(log_df$date, format = "%Y Q%q")

# Apply the Hodrick-Prescott filter
# Initialize two empty dataframes, one for each BC component
#cycle1 = data.frame(matrix(nrow = 237, ncol = 0))
#trend1 = data.frame(matrix(nrow = 237, ncol = 0))

# Filter through HP filter and extract both cyclical and trend components
#for (colname in colnames(log_df[2:15])) {
  hp_filter <- hpfilter(log_df[[colname]], freq = 1600, type = "lambda", drift = FALSE)
  #extract the two components
  cyclical <- hp_filter$cycle
  trend <- hp_filter$trend
  # fill the empty df with the obtained values
  cycle1[[colname]] <- cyclical
  trend1[[colname]] <- trend
}


# ISSUE: the cyclical and trend components of interest rate and tfp is NAs.
# 1) reasons: is it because they didn't underwent log transf, and are thus negative?
# 2) is it because they have NAs in their starting columns?
# (1) sounds weird so I'll try to fix (2) since there are only 6 NAs:

#DROP NA rows
log_df2 <- log_df[1:(nrow(log_df) - 7), ]

#APPLY GP to log_df1, we will use the number 2 to differentiate

cycle2 = data.frame(matrix(nrow = 230, ncol = 0))
trend2 = data.frame(matrix(nrow = 230, ncol = 0))

# Filter through HP filter and extract both cyclical and trend components
for (colname in colnames(log_df2[2:15])) {
  hp_filter <- hpfilter(log_df2[[colname]], freq = 1600, type = "lambda", drift = FALSE)
  #extract the two components
  cyclical <- hp_filter$cycle
  trend <- hp_filter$trend
  # fill the empty df with the obtained values
  cycle2[[colname]] <- cyclical
  trend2[[colname]] <- trend
}

# yay, issue fixed!the NAs were the problem

# Let's build the tables

# Calculate standard deviations
std_deviations <- apply(cycle2, 2, sd)

# Create a final df, the one that we will print at the end
df_final <- data.frame(Column = names(std_deviations),
                     SD = std_deviations,
                     row.names = names(cycle2))
colnames(df_final) = c("Variable", "SD")


# To produce the tables I will compute all cross correlations, then I'll drop the ones I'm not interested in

# CROSS CORRELATIONS

# Retrieve columns and rows
variables = colnames(cycle2)
lags = -4:4

# Initialize empty df
result_df <- data.frame(Variable = character(length(variables)),
                        t_4 = numeric(length(variables)),
                        t_3 = numeric(length(variables)),
                        t_2 = numeric(length(variables)),
                        t_1 = numeric(length(variables)),
                        t = numeric(length(variables)),
                        t_1 = numeric(length(variables)),
                        t_2 = numeric(length(variables)),
                        t_3 = numeric(length(variables)),
                        t_4 = numeric(length(variables)),
                        stringsAsFactors = FALSE)

# Compute actual cross-correlations
for (i in seq_along(variables)) {
  var = variables[i]
  correlations = ccf(cycle2$gnp, cycle2[[var]], lag.max = max(abs(lags)), plot = FALSE)$acf
  correlations = correlations[lags + 5]
  result_df[i, ] = c(var, correlations)
}


