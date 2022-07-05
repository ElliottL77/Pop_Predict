setwd("D:/Documents/WGU/C997/Final Project") #sets the working directory to this file path.

library(readxl) # Loads readxl so we can read the Excel file.
library(tidyverse) #loads the Tidyverse functions.

censusdata <- read_excel("elight5_Working_Data.xlsx", 
                         range = "A2:M53") # Imports the data...
# There are a lot of annotations on this file, so we'll use "range" to read only...
# The header row and the data we want.

head(censusdata) # Displays the first 6 rows of data. We can get a nice, easy view of how the data was imported.

mnrow <- filter(censusdata, State == ".Minnesota") #Selects the row for Minnesota.

# mndata <-select(mnrow, 4:13) # Selects just the years, ignoring the first three columns. The "4:13" is a nice way...
# # ... to capture the data without having to enter "'2010', '2011', ..."

Years <-  c(2010:2019)
MNpop <- c(5310828, 5346143, 5376643, 5413479, 5451079, 5482032, 5522744, 5566230, 5606249, 5639632) #The data entered by hand.

# mndata.col <- pivot_longer(mndata, c("2010":"2019"), names_to = "Year",  values_to = "Population")
MNState <-  cbind.data.frame(Years,MNpop) #binds the two columns together and converts it to a dataframe.

lmn <- lm(MNpop ~ Years, MNState) # Gets our linear model. y = x --> Population = Year.
summary(lmn) #Gets summary data of lmn.

plot(MNpop ~ Years , MNState) + #Plots the data as a scatterplot.
  abline(lmn, col='red') #inserts a red line that is the function of "lmn". The color is red.

FutureYears <- data.frame(Years = c(2020, 2021, 2022, 2023, 2024)) #Constructs the dataframe with the 5 years we seek to predict.
predict(lmn, newdata=FutureYears) # Takes our model, "lmn", and makes a prediction using the "FutureYears" values as the x variable. The output is...
# ... 5674395, 5711283, 5748172, 5785061, 5821950
