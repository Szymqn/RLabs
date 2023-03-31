library(tidyverse)
library(readr)

acsNY <- read_csv("./acsNew.csv", 
                  col_types = cols(ElectricBill = col_integer(), 
                                   FamilyIncome = col_integer(), 
                                   FamilyType = col_factor(levels = c("Married", "Female Head", "Male Head")), 
                                   HouseCosts = col_integer(), 
                                   Insurance = col_integer(), 
                                   NumBedrooms = col_integer(), 
                                   NumChildren = col_integer(), 
                                   NumPeople = col_integer(), 
                                   NumRooms = col_integer(), 
                                   NumVehicles = col_integer(), 
                                   NumWorkers = col_integer()))

summary(acsNew)

acsNY %>%
  filter(NumChildren == 0) %>%
  select(c(1:6)) %>%
  head()

save(acsNY, file = "acsNY.RData")

rm(list = ls())

load("acsNY.RData")

summary(acsNY)

acsNew <- read_csv("./acsNew.csv", 
                   col_types = cols(ElectricBill = col_integer(), 
                                    FamilyIncome = col_integer(),
                                    FamilyType = col_factor(levels = c("Married", "Female Head", "Male Head")),
                                    HeatingFuel = col_factor(levels = c("Wood", "Oil", "Electricity", "Gas", "Coal", "Other", "None")),
                                    HouseCosts = col_integer(),
                                    Income = col_factor(levels = c("Below", "Above")),
                                    Insurance = col_integer(), 
                                    NumBedrooms = col_integer(), 
                                    NumChildren = col_integer(), 
                                    NumPeople = col_integer(), 
                                    NumRooms = col_integer(), 
                                    NumVehicles = col_integer(), 
                                    NumWorkers = col_integer()))

summary(acsNew)

acsNY %>% summary
