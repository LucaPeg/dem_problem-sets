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
list_df <- list(GNP, CND, CD, AveH, C, I, r, Y, add_var)
df <- list_df %>% reduce(inner_join, by='date')

# create the last variable GNP/L: output per worker
df$GNP_L <- df$GNP - df$L #using - because we're dealing with logs
#this creates a negative number since logGNP<logL.
# Or I could leave it as is, since GNP is in billion of dollars, while L is in thousands of people.

# Find Y/N (I forgot about this and now I have to go back)
# I could divide Y by AveH, but it feels weird. So I'm going to get per capita hours, then use it to divide Y
N <- df$L - df$H # hours per capita
df$Y_N <- df$Y- N # Y per capita / H per capita

# I create another variable, which is the same but obtained with Weeekly Hours
df$Y_N2 <- df$Y-df$AveH

#I add also N as a robustness check got from H/L
df$N <- N

# I spotted a weird issue. real GNP in 1990 Q1 is 9.400.000. Its ln should be 16, while its log10 should be 7.
# My result is 9, which I don't really understand.

# last row has an NA value, I will drop it
df <- df[1:(nrow(df) - 1), ]

# isolate trend and cyclical component thorough an HP filter

cycle = data.frame(matrix(nrow = 230, ncol = 0))
trend = data.frame(matrix(nrow = 230, ncol = 0))

for (colname in colnames(df[2:17])) { #take everthing but the date
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
# ok evidently the shift is the same. Good since I have to measure the cross corr/sd

# To de-trend our variables we'll use the cyclical component ONLY

# Make a big table, then drop the relevant rows to achieve the two final tables

columns <- c("Variables", "sd%", "t-4", "t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4")
table_tot <- data.frame(matrix(nrow = 16, ncol = 12))
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
cross_correlations <- data.frame(matrix(nrow = 16, ncol = 9))
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
cross_correlations <- data.frame(matrix(nrow = 16, ncol = 9))
colnames(cross_correlations) <- lags
rownames(cross_correlations) <- colnames(cycle)

# Get the cross correlations
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

#Got the final table! Now I have to drop some columns to get the two necessary ones.

# Variables we want in the first table
var_1st = c("GNP", "CND","CD","H","AveH",'L',"GNP_L","w")
var_2nd = c("Y", "C","I","AveH","Y_N", "Y_N2","w","r","d_tfp","N")

first_table <- subset(table_final[var_1st,])

# SECOND TABLE : I MUST DO THE AUTOCORRELATIONS ETC BUT WITH Y, NOT GNP
# Actually it's the same thing, stupid
# Still, let's subset "cycle" into the variables we need for table 2, then compute everything

cycle2 <- subset(cycle[,var_2nd])

# On cycle2 we have to carry out the following operations:
# 1) compute std dev
# 2) compute relative std dev
# 3) compute first order autocorrelation
# 4) compute contemporaneous correlation with output

# (1)
standard_dev2 <- apply(cycle2,2, sd )
standard_dev2 <- ifelse(names(standard_dev2) == "r", standard_dev2, standard_dev2 * 100)
standard_dev2 <-  round(standard_dev2, digits = 2)


# (2)
ssd2 <- standard_dev2 / standard_dev2[1]
ssd2 <-  round(ssd2, digits = 2)
# (3)

autocorrelation <- data.frame(    # initialize a data_frame to store autocorrelation results
  Column = character(), # where the variable name will go
  Autocorrelation = numeric(), #where the value will go, this way I will avoid the previous mess
  stringsAsFactors = FALSE 
)

for (variable in colnames(cycle2)) {
  acf_result <- acf(cycle2[[variable]], lag.max = 1, plot = FALSE) #computes autocorrelations
  autocorrelation <- rbind(autocorrelation,  # binds the different rows to the initialized df (that for each loop gains a row)
                           data.frame(Column = variable,
                                      Autocorrelation = acf_result$acf[2])) #2 because we are interested in lag 1, not the zero as well
}
autocorrelation$Autocorrelation <- round(autocorrelation$Autocorrelation, digits = 2)
# (4)

#contemporeneous correlations with output I guess it means to compute, for each period, the correlation of a given variable with Y
corr_Y <- data.frame(
  Column = character(),
  corr = numeric(),
  stringsAsFactors = FALSE
)

for (variable in colnames(cycle2)) {
  corr_result <- ccf(cycle2$Y, cycle2[[variable]], lag.max = 0, plot = FALSE) # 0 lag <- contemporaneuos correlation 
  corr_Y <- rbind(corr_Y, data.frame(Column = variable, corr = corr_result$acf))
}

# can I use just cor instead of the ccf? Let's see if the results differ
corr_Y2 <- data.frame(
  Column = character(),
  corr = numeric(),
  stringsAsFactors = FALSE
)

for (variable in colnames(cycle2)) {
  corr_result2 <- cor(cycle2$Y, cycle2[[variable]])
  corr_Y2 <- rbind(corr_Y2, data.frame(Column = variable, corr = corr_result2))
}

# The two results are perfectly the same. I'll round the first one up
corr_Y$corr <- round(corr_Y$corr, digits = 2)

# Only thing left is to merge everything!
# Elements that I have to merge: corr_Y, autocorrelation, ssd2, standard_dev2

#I will create a df with standard_dev2 and ssd2, since those are arrays, not df
table2 <- data.frame(
  "Variable" = colnames(cycle2),
  "SD" = standard_dev2,
  "Relative_SD" = ssd2
)

#fix names of the corr_Y and autocorrelation

colnames(corr_Y) <- c("Variable", "Contemporaneous Correlation with Y")
colnames(autocorrelation) <-  c("Variable", "First Order Auto-Correlation")

# Join everything on the col "Variable"
list_table2_vars <- list(table2, autocorrelation, corr_Y)
table2 <- list_table2_vars %>% reduce(inner_join, by = 'Variable')

table2

# There is a question that asks for relation between Y and G. Thus I get G

G <- fredr(
  series_id = 'A955RX1Q020SBEA',
  observation_start = as.Date("1971-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = 'q',
  units = "log"
)

# Keep only the date and value columns
G <- G[-c(2, 4, 5)]
colnames(G) <- c("date", "G")

# Filter G
hp_filter_G <- hpfilter(G$G, freq = 1600, type = "lambda", drift = FALSE)

# Extract the two components
cyclical_comp_G <- hp_filter_G$cycle
trend_comp_G <- hp_filter_G$trend

# Fill the empty data frames with the obtained values
cycle_G <- data.frame(date = G$date, G = cyclical_comp_G)
trend_G <- data.frame(date = G$date, G = trend_comp_G)

# find sd of G
sd_G <- sd(cycle_G$G)
table2
