#Deliverable 1

library(dplyr)

MechaCar_table <- read.csv("MechaCar_mpg.csv")

lin_reg <- lm(formula = MechaCar_table$"vehicle_length"~
     MechaCar_table$"vehicle_weight"+
     MechaCar_table$"spoiler_angle"+
     MechaCar_table$"ground_clearance"+
     MechaCar_table$"AWD"+
     MechaCar_table$"mpg", data = MechaCar_table)

summary(lin_reg)

#Deliverable 2

Suspension_table <- read.csv("Suspension_Coil.csv")

total_summary <- Suspension_table %>% summarize(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), Standard_Deviation = sd(PSI))

lot_summary <- Suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), Standard_Deviation = sd(PSI))

#Deliverable 3

t.test(Suspension_table$"PSI", mu = 1500)

t.test(subset(Suspension_table, Manufacturing_Lot == "Lot1",select=PSI), mu = 1500)

t.test(subset(Suspension_table, Manufacturing_Lot == "Lot2",select=PSI), mu = 1500)

t.test(subset(Suspension_table, Manufacturing_Lot == "Lot3",select=PSI), mu = 1500)



