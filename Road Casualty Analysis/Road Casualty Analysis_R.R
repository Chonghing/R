library(readr)
data1 <- read.csv("C:/Users/Hing/Desktop/Upskill/Week 4_Python&R/Road Casualty Analysis/Data/dft-road-casualty-statistics-casualty-2020.csv", header=TRUE, stringsAsFactors=FALSE)

summary(data1)

data1 <-subset(data1, select =c(accident_reference, casualty_class, sex_of_casualty, age_of_casualty, age_band_of_casualty, casualty_severity,pedestrian_location, pedestrian_movement, car_passenger, casualty_type, casualty_home_area_type, casualty_imd_decile))

data1 <-subset(data1, ((sex_of_casualty==1) | (sex_of_casualty==2))& pedestrian_location!=-1 & pedestrian_movement!=-1 & car_passenger!=-1 & casualty_home_area_type!=-1 & casualty_imd_decile!=-1 & age_band_of_casualty!=-1)

library(readxl)
data2 <- read_excel("C:/Users/Hing/Desktop/Upskill/Week 4_Python&R/Road Casualty Analysis/Data/Road-Safety-Open-Dataset-Data-Guide.xlsx", sheet = "Sheet1")

data2 <-subset(data2, table=="Casualty")
names(data2) <- make.names(names(data2), unique = TRUE)

data2sex = subset(data2, field.name=="sex_of_casualty")
data2sex <-subset(data2sex, select =c(code.format, label))
library(dplyr)
data2sex <- rename(data2sex, sex_of_casualty = code.format, Sex = label)
data2sex$sex_of_casualty <- as.integer(data2sex$sex_of_casualty)
data1 <- left_join(data1, data2sex, by='sex_of_casualty')

data2class = subset(data2, field.name=="casualty_class")
data2class <-subset(data2class, select =c(code.format, label))
data2class <- rename(data2class, casualty_class = code.format, Class = label)
data2class$casualty_class <- as.integer(data2class$casualty_class)
data1 <- left_join(data1, data2class, by='casualty_class')

data2ageband = subset(data2, field.name=="age_band_of_casualty")
data2ageband <-subset(data2ageband, select =c(code.format, label))
data2ageband <- rename(data2ageband, age_band_of_casualty = code.format, Age_Band = label)
data2ageband$age_band_of_casualty <- as.integer(data2ageband$age_band_of_casualty)
data2ageband["Age_Band"][data2ageband["Age_Band"]=="0 - 5"] <- "00 - 05"
data2ageband["Age_Band"][data2ageband["Age_Band"]=="6 - 10"] <- "06 - 10"
data1 <- left_join(data1, data2ageband, by='age_band_of_casualty')

data2severity = subset(data2, field.name=="casualty_severity")
data2severity <-subset(data2severity, select =c(code.format, label))
data2severity <- rename(data2severity, casualty_severity = code.format, Severity = label)
data2severity$casualty_severity <- as.integer(data2severity$casualty_severity)
data1 <- left_join(data1, data2severity, by='casualty_severity')

data2vehicle = subset(data2, field.name=="casualty_type")
data2vehicle <-subset(data2vehicle, select =c(code.format, label))
data2vehicle <- rename(data2vehicle, casualty_type = code.format, Vehicle = label)
data2vehicle$casualty_type <- as.integer(data2vehicle$casualty_type)
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Motorcycle 50cc and under rider or passenger"] <- "Motorcycle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Motorcycle 125cc and under rider or passenger"] <- "Motorcycle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Motorcycle over 125cc and up to 500cc rider or  passenger"] <- "Motorcycle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Motorcycle over 500cc rider or passenger"] <- "Motorcycle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Goods vehicle (over 3.5t. and under 7.5t.) occupant"] <- "Goods vehicle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Goods vehicle (7.5 tonnes mgw and over) occupant"] <- "Goods vehicle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Motorcycle - unknown cc rider or passenger"] <- "Motorcycle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Van / Goods vehicle (3.5 tonnes mgw or under) occupant"] <- "Goods vehicle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Tram occupant"] <- "Tram"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Taxi/Private hire car occupant"] <- "Taxi/Private hire"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Minibus (8 - 16 passenger seats) occupant"] <- "Minibus"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Bus or coach occupant (17 or more pass seats)"] <- "Bus or coach"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Agricultural vehicle occupant"] <- "Agricultural vehicle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Mobility scooter rider"] <- "Mobility scooter"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Electric motorcycle rider or passenger"] <- "Electric motorcycle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Other vehicle occupant"] <- "Other vehicle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Car occupant"] <- "Car"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Horse rider"] <- "Horse"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Cyclist"] <- "Bicycle"
data2vehicle["Vehicle"][data2vehicle["Vehicle"]=="Goods vehicle (unknown weight) occupant"] <- "Goods vehicle"
data1 <- left_join(data1, data2vehicle, by='casualty_type')

data1 <-subset(data1, select =c(accident_reference, Class, Sex, age_of_casualty, Age_Band, Severity, Vehicle))

data1 <- rename(data1, Age = age_of_casualty)

library(ggplot2)

data1_severity <- data1 %>% group_by(Severity) %>% count(accident_reference)
ggplot(data1_severity, aes(x=Severity, y=n, fill=Severity)) +geom_bar(stat="identity") + ggtitle("Road Casualty Severity") + ylab("Number of Cases")

data1_person <- data1 %>% group_by(Class) %>% count(accident_reference)
ggplot(data1_person, aes(x=Class, y=n, fill=Class)) +geom_bar(stat="identity") + ggtitle("Road Casualty Person") + ylab("Number of Cases")

data1_fatal_d <-subset(data1, Severity=="Fatal" & Class=="Driver or rider")

data1_fatal_d_gender <- data1_fatal_d %>% group_by(Sex) %>% count(accident_reference)
data1_fatal_d_gender <- data1_fatal_d_gender %>% select (Sex, n) %>% group_by(Sex) %>% summarise (n = sum(n))
data1_fatal_d_gender <-group_by(data1_fatal_d_gender) %>% mutate(percent = n/sum(n))
ggplot(data1_fatal_d_gender, aes(x=Sex, y=percent, fill=Sex)) +geom_bar(stat="identity") + ggtitle("Fatal case by Gender") + ylab("Number of Cases")
pie = ggplot(data1_fatal_d_gender,aes(x="",y=percent,fill =Sex)) +geom_bar(stat ="identity", width =1)
pie = pie + coord_polar("y", start=0)
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Fatal case by Gender")
pie

pie(data1_fatal_d_gender$percent, labels =paste(data1_fatal_d_gender$Sex, sep = " ",format(round(data1_fatal_d_gender$percent*100,2), nsmall=2), "%"), main = "Fatal case by Gender")

data1_fatal_d_age <- data1_fatal_d %>% group_by(Age_Band) %>% count(accident_reference)
data1_fatal_d_age <- data1_fatal_d_age %>% select (Age_Band, n) %>% group_by(Age_Band) %>% summarise (n = sum(n))
ggplot(data=data1_fatal_d_age, aes(x=Age_Band, y=n, group=1)) + geom_line() + ggtitle("Age of Fatal case") + labs(x="Age Band", y="Number of cases")

data1_fatal_d_vehicle_gender <-subset(data1_fatal_d, select =c(Sex, Vehicle))

data1_fatal_d_vehicle_f <-subset(data1_fatal_d_vehicle_gender, Sex=="Female")
data1_fatal_d_vehicle_f <- data1_fatal_d_vehicle_f %>% group_by(Vehicle) %>% count(Sex)
data1_fatal_d_vehicle_f <- data1_fatal_d_vehicle_f %>% select (Vehicle, n) %>% group_by(Vehicle) %>% summarise (n = sum(n))
data1_fatal_d_vehicle_f <-group_by(data1_fatal_d_vehicle_f) %>% mutate(percent = n/sum(n))
pie(data1_fatal_d_vehicle_f$percent, labels =paste(data1_fatal_d_vehicle_f$Vehicle, sep = " ",format(round(data1_fatal_d_vehicle_f$percent*100,2), nsmall=2), "%"), main = "Fatal case by Vehicle of Female")

data1_fatal_d_vehicle_m <-subset(data1_fatal_d_vehicle_gender, Sex=="Male")
data1_fatal_d_vehicle_m <- data1_fatal_d_vehicle_m %>% group_by(Vehicle) %>% count(Sex)
data1_fatal_d_vehicle_m <- data1_fatal_d_vehicle_m %>% select (Vehicle, n) %>% group_by(Vehicle) %>% summarise (n = sum(n))
data1_fatal_d_vehicle_m <-group_by(data1_fatal_d_vehicle_m) %>% mutate(percent = n/sum(n))
pie(data1_fatal_d_vehicle_m$percent, labels =paste(data1_fatal_d_vehicle_m$Vehicle, sep = " ",format(round(data1_fatal_d_vehicle_m$percent*100,2), nsmall=2), "%"), main = "Fatal case by Vehicle of Male")