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
colnames(GNP) <- c("date","GNP")

# CD
CD <- fredr(
  series_id = 'PCEDG',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
CD <- CD[-c(2, 4, 5)]
colnames(CD) <- c("date","CD")

CND <- fredr(
  series_id = 'PCEND',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
CND <- CND[-c(2, 4, 5)]
colnames(CND) <- c("date","CND")

AveH <- fredr(
  series_id = 'AWHNONAG',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
AveH <- AveH[-c(2,4,5)] 
colnames(AveH) <- c("date","AveH")

# Real per capita output
Y <- fredr(
  series_id = 'A939RX0Q048SBEA',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
Y <- Y[-c(2,4,5)] 
colnames(Y) <- c("date","Y")

# Real per capita consumption
C <- fredr(
  series_id = 'A794RX0Q048SBEA',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
C <- C[-c(2,4,5)] 
colnames(C) <- c("date","C")

# Real Gross Private Domestic Investments
I <- fredr(
  series_id = 'GPDIC1',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
I <- I[-c(2,4,5)] 
colnames(I) <- c("date","I")

r <- fredr(
  series_id = 'INTDSRUSM193N',
  observation_start = as.Date("1964-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q', #quarterly data
  units = "log"
)
r <- r[-c(2,4,5)] 
colnames(r) <- c("date","r")

# real wage is missing
# TFP (A) is missing
# total hours is missing
# labour force is missing
# productivity per capita is missing

# We add this variables through an excel file and we fix the measurement unit

add_var <- read_xlsx("C:/Users/lucap/Documents/GitHub/dem_ps/additional_variables.xlsx")
add_var$H <- add_var$H*1000
add_var$L <- add_var$L*1000
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

#I want to make a join, based on the foreign key "date", I will have to uniform the variable
add_var$date <- as.yearqtr(add_var$date, format = "%Y Q%q") #typecast as date
add_var$date <- format(as.Date(as.yearmon(add_var$date), frac = 0), "%Y-%m-%d") #change date format
add_var$date <-as.Date(add_var$date, format = "%Y-%m-%d") #cast it into date type

# join everything
list_df = list(GNP, CND, CD, AveH, C, I, r, Y, add_var)
df <- list_df %>% reduce(inner_join, by='date')

# create the last variable GNP/L: output per worker
df$GNP_L <- df$GNP - df$L #using - because we're dealing with logs
#this creates a negative number since logGNP<logL.
#To deal with this I could transform everythin into single dollars and # people
# Or I could leave it as is, since GNP is in billion of dollars, while L is in thousands of people.

# I spotted a weird issue. real GNP in 1990 Q1 is 9.400.000. Its ln should be 16, while its log10 should be 7.
# My result is 9, which I don't really understand.

# last row has an NA value, I will drop it
df <- df[1:(nrow(df) - 1), ]

# isolate trend and cyclical component thorough an HP filter

cycle = data.frame(matrix(nrow = 230, ncol = 0))
trend = data.frame(matrix(nrow = 230, ncol = 0))

for (colname in colnames(df[2:14])) { #take everthing but the date
  hp_filter <- hpfilter(df[[colname]], freq = 1600, type = "lambda", drift = FALSE)
  #extract the two components
  cyclical_comp <- hp_filter$cycle
  trend_comp <- hp_filter$trend
  # fill the empty df with the obtained values
  cycle[[colname]] <- cyclical_comp
  trend[[colname]] <- trend_comp
}

# I still think there is a mistake because my GNP log is TOO CLOSE (goes from like 8 to 9)
# 1964 Q1 3.889.944 -> ln = 15.17, log10 = 6.58, mydf = 8.266
# 2021 Q2 17.671.563 -> ln = 16.76  log10= 7.29, mydf = 9.866
# ok evidently the shift is the same. Good since I have to measure the cross corr or sd

# To de-trend our variables we'll use the cyclical component ONLY

# Make a big table, then drop the relevant rows to achieve the two final tables

columns <- c("Variables", "sd%", "t-4", "t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4")
table_tot <- data.frame(matrix(nrow = 13, ncol = 11))
colnames(table_tot) <- columns

# Get the Standard Deviations

standard_dev <- apply(cycle, 2, sd) #get the standard deviation values
standard_dev = standard_dev*100 # get the percentage term?

# Fill the total table with the variable names and the standard deviations

table_tot$Variables <- colnames(cycle)
table_tot$`sd%` <- standard_dev

# Only the cross correlations are now missing

# I changed my mind -> I will create a second df with only the cc, than merge the two

# Provisional table for cross correlations
lags <- c("t-4", "t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4")
cross_correlations <- data.frame(matrix(nrow = 13, ncol = 9))
colnames(cross_correlations) <- lags
rownames(cross_correlations) <- colnames(cycle)

# Compute cross correlations
for (variable in colnames(cycle)) {
  cc <- ccf(cycle$GNP, cycle[, variable], lag.max = 4, plot = FALSE)
  
  # Convert non-numeric elements to NA and back to numeric -> Otherwise some cols remain of "character" type.
  cc$acf[!is.numeric(cc$acf)] <- NA #cc$acf access the value we want in the cc object
  cross_correlations[variable, ] <- as.numeric(cc$acf)
}

# Round to 2 decimal points
cross_correlations <- round(cross_correlations, digits = 2)

# Merge the two tables to get the final one
# Provisional table for cross correlations
lags <- c("t-4", "t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4")
cross_correlations <- data.frame(matrix(nrow = 13, ncol = 9))
colnames(cross_correlations) <- lags
rownames(cross_correlations) <- colnames(cycle)

# Basic function for cross correlations
for (variable in colnames(cycle)) {
  cc <- ccf(cycle$GNP, cycle[, variable], lag.max = 4, plot = FALSE)
  
  # Convert non-numeric elements to NA
  cc$acf[!is.numeric(cc$acf)] <- NA
  
  # Convert to numeric and assign to cross_correlations
  cross_correlations[variable, ] <- as.numeric(cc$acf)
}

# Round the elements to 2 decimal points
cross_correlations <- round(cross_correlations, digits = 2)

# Join cross_correlations with table tot to get the final table

# First keep only the first two cols of table_tot

table_tot <- subset(table_tot[,1:2])

# I will cbind and then check whether rownames of table tot and var column of cross_correlations match
table_final <- cbind(table_tot, cross_correlations,  row.names = rownames(cross_correlations))
# They check out, but I forgot to round up sd$
table_final$'sd%' <- round(table_final$'sd%', digits = 2)
