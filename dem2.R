library(tidyverse)
library(dplyr)
library(mFilter)
library(quantmod)
library(zoo)
library(fredr)
library(readxl)

# Key to access FRED data
fredr_set_key("d0bf6a360e3cb655d38f17c115ccc52b")

# Retrieve data from FRED website

#GNP
GNP <- fredr(
  series_id = 'GNPC96',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
#this produces a 5 col tibble, we keep only the date and the values
GNP <- GNP[-c(2, 4, 5)]

# CD
CD <- fredr(
  series_id = 'PCEDG',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
CD <- CD[-c(1, 2, 4, 5)]

CND <- fredr(
  series_id = 'PCEND',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
CND <- CND[-c(1, 2, 4, 5)]

AveH <- fredr(
  series_id = 'AWHNONAG',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
AveH <- AveH[-c(1,2,4,5)] 

# Real per capita output
Y <- fredr(
  series_id = 'A939RX0Q048SBEA',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
Y <- Y[-c(1,2,4,5)] 

# Real per capita consumption
C <- fredr(
  series_id = 'A794RX0Q048SBEA',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
C <- C[-c(1,2,4,5)] 

# Real Gross Private Domestic Investments
I <- fredr(
  series_id = 'GPDIC1',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
I <- I[-c(2,4,5)] 

r <- fredr(
  series_id = 'INTDSRUSM193N',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
r <- r[-c(1,2,4,5)] 

# real wage is missing
# TFP (A) is missing
# total hours is missing
# labour force is missing

# We add this variables through an excel file and we fix the measurement unit

add_var = read_xlsx("C:/Users/lucap/Documents/GitHub/dem_ps/additional_variables.xlsx")
add_var$H = add_var$H*1000
add_var$L = add_var$L*1000
#we subset out the date and A, in order to carry out a log transformation
subset_add_var = add_var[,2:4]
log_df <- as.data.frame(lapply(subset_add_var, log))
# Re-upload A in order to join it to our variables
A = read_xlsx("C:/Users/lucap/Documents/GitHub/dem_ps/additional_variables.xlsx")
A = A[,c(1,5)]
add_var = cbind(A, log_df) 

# clean the environment a bit
remove(log_df)
remove(subset_add_var)
remove(A)

# make everything 230 observations
AveH <- AveH[1:(nrow(AveH) - 7), ]
C <- C[1:(nrow(C) - 7), ]
CD <- CD[1:(nrow(CD) - 7), ]
CND <- CND[1:(nrow(CND) - 7), ]
GNP <- GNP[1:(nrow(GNP) - 7), ]
I <- I[1:(nrow(I) - 7), ]
Y <- Y[1:(nrow(Y) - 7), ]
add_var <- add_var[1:(nrow(add_var) - 6), ]
r <- r[1:(nrow(r) - 1), ]

# join everything
df = cbind(add_var,GNP, CND, CD, AveH, C, I, r, Y)

# retrieve the standard deviations
for (colname in colnames(df[2:14])) {
  hp_filter <- hpfilter(log_df2[[colname]], freq = 1600, type = "lambda", drift = FALSE)
  #extract the two components
  cyclical <- hp_filter$cycle
  trend <- hp_filter$trend
  # fill the empty df with the obtained values
  cycle2[[colname]] <- cyclical
  trend2[[colname]] <- trend
}
