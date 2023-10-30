library(dplyr)
library(reshape2)
library(ggplot2)

###Part 1:
## Importing and cleaning the data
#Import datasets of patient 31 to 35
patient31 <- read.table("data-31", header = FALSE)
patient32 <- read.table("data-32", header = FALSE)
patient33 <- read.table("data-33", header = FALSE)
patient34 <- read.table("data-34", header = FALSE)
patient35 <- read.table("data-35", header = FALSE)
#Change column name
colnames(patient31) <- c("Date", "Time", "Code", "Value")
colnames(patient32) <- c("Date", "Time", "Code", "Value")
colnames(patient33) <- c("Date", "Time", "Code", "Value")
colnames(patient34) <- c("Date", "Time", "Code", "Value")
colnames(patient35) <- c("Date", "Time", "Code", "Value")
#Combine date time to another column
patient31 <-  patient31 %>%
  mutate(patient31, datetime = paste(Date, Time))
patient32 <-  patient32 %>%
  mutate(patient32, datetime = paste(Date, Time))
patient33 <-  patient33 %>%
  mutate(patient33, datetime = paste(Date, Time))
patient34 <-  patient34 %>%
  mutate(patient34, datetime = paste(Date, Time))
patient35 <-  patient35 %>%
  mutate(patient35, datetime = paste(Date, Time))
#Change column $datetime into POSIXct type
patient31$datetime <-as.POSIXct(patient31$datetime,format="%m-%d-%Y %H:%M",tz=Sys.timezone())
patient32$datetime <-as.POSIXct(patient32$datetime, format="%m-%d-%Y %H:%M",tz=Sys.timezone())
patient33$datetime <-as.POSIXct(patient33$datetime, format="%m-%d-%Y %H:%M",tz=Sys.timezone())
patient34$datetime <-as.POSIXct(patient34$datetime, format="%m-%d-%Y %H:%M",tz=Sys.timezone())
patient35$datetime <-as.POSIXct(patient35$datetime, format="%m-%d-%Y %H:%M",tz=Sys.timezone())

##Derive individual summaries for patient data
#Patient 31:
summary31 <- summary(patient31$Value)
summary31
sd31 <- sd(patient31$Value)
sd31
#Patient 32:
summary32 <- summary(patient32$Value)
summary32
sd32 <- sd(patient32$Value)
sd32
#Patient 33:
summary33 <- summary(patient33$Value)
summary33
sd33 <- sd(patient33$Value)
sd33
#Patient 34:
summary34 <- summary(patient34$Value)
summary34
sd34 <- sd(patient34$Value)
sd34
#Patient 35:
summary35 <- summary(patient35$Value)
summary35
sd35 <- sd(patient35$Value)
sd35

##Derive collective summaries for the patient data
#create patient ID
patient31 <- patient31 %>%
  mutate(ID = as.factor(31))
patient31 <- patient31[, c(6, 1, 2, 3, 4, 5)]
patient32 <- patient32 %>%
  mutate(ID = as.factor(32))
patient32 <- patient32[, c(6, 1, 2, 3, 4, 5)]
patient33 <- patient33 %>%
  mutate(ID = as.factor(33))
patient33 <- patient33[, c(6, 1, 2, 3, 4, 5)]
patient34 <- patient34 %>%
  mutate(ID = as.factor(34))
patient34 <- patient34[, c(6, 1, 2, 3, 4, 5)]
patient35 <- patient35 %>%
  mutate(ID = as.factor(35))
patient35 <- patient35[, c(6, 1, 2, 3, 4, 5)]
#Merging data frame
patient_all <- rbind(patient31, patient32, patient33, patient34, patient35)
#Collective summary
summary_all <- summary(patient_all$Value)
summary_all
sd_all <- sd(patient_all$Value)
sd_all 

###Part2:
## Perform validation study
#Extract the Hours from the datetime and format it as numeric
patient31 <- mutate(patient31,Hours = format(as.POSIXct(patient31$datetime), format = "%H"))
patient31$Hours <- as.numeric(patient31$Hours)
patient32 <- mutate(patient32,Hours = format(as.POSIXct(patient32$datetime), format = "%H"))
patient32$Hours <- as.numeric(patient32$Hours)
patient33 <- mutate(patient33,Hours = format(as.POSIXct(patient33$datetime), format = "%H"))
patient33$Hours <- as.numeric(patient33$Hours)
patient34 <- mutate(patient34,Hours = format(as.POSIXct(patient34$datetime), format = "%H"))
patient34$Hours <- as.numeric(patient34$Hours)
patient35 <- mutate(patient35,Hours = format(as.POSIXct(patient35$datetime), format = "%H"))
patient35$Hours <- as.numeric(patient35$Hours)
patient_all <- rbind(patient31, patient32, patient33, patient34, patient35)
#Subset blood glucose measurements (48-64)
patient31_subset <- patient31[patient31$Code >= 48 & patient31$Code <= 64,]
patient32_subset <- patient32[patient32$Code >= 48 & patient32$Code <= 64,]
patient33_subset <- patient33[patient33$Code >= 48 & patient33$Code <= 64,]
patient34_subset <- patient34[patient34$Code >= 48 & patient34$Code <= 64,]
patient35_subset <- patient35[patient35$Code >= 48 & patient35$Code <= 64,]
#Get the average value of all measurements taken in a particular hour
patient31_subset <- patient31_subset %>%
  group_by(Hours) %>% mutate(MeanbyHour=mean(Value))
patient32_subset <- patient32_subset %>%
  group_by(Hours) %>% mutate(MeanbyHour=mean(Value))
patient33_subset <- patient33_subset %>%
  group_by(Hours) %>% mutate(MeanbyHour=mean(Value))
patient34_subset <- patient34_subset %>%
  group_by(Hours) %>% mutate(MeanbyHour=mean(Value))
patient35_subset <- patient35_subset %>%
  group_by(Hours) %>% mutate(MeanbyHour=mean(Value))

#Draw a 24-hour boold glucose change graph for each patient
patient31_plot24h <- ggplot(patient31_subset, aes(x=Hours, y=MeanbyHour)) + geom_line(colour = "black", size = 2) + geom_point(colour = "blue", size = 5) + theme_light()
patient31_plot24h
patient32_plot24h <- ggplot(patient32_subset, aes(x=Hours, y=MeanbyHour)) + geom_line(colour = "black", size = 2) + geom_point(colour = "blue", size = 5) + theme_light()
patient32_plot24h
patient33_plot24h <- ggplot(patient33_subset, aes(x=Hours, y=MeanbyHour)) + geom_line(colour = "black", size = 2) + geom_point(colour = "blue", size = 5) + theme_light()
patient33_plot24h
patient34_plot24h <- ggplot(patient34_subset, aes(x=Hours, y=MeanbyHour)) + geom_line(colour = "black", size = 2) + geom_point(colour = "blue", size = 5) + theme_light()
patient34_plot24h
patient35_plot24h <- ggplot(patient35_subset, aes(x=Hours, y=MeanbyHour)) + geom_line(colour = "black", size = 2) + geom_point(colour = "blue", size = 5) + theme_light()
patient35_plot24h

##Draw 24h changes graph
#Merging data frame
patient_all_subset <- rbind(patient31_subset, patient32_subset, patient33_subset, patient34_subset, patient35_subset)  
#Comparing individual plot
individual_all_plot24h <- ggplot(patient_all_subset, aes(x=Hours, y=MeanbyHour, color = ID)) + geom_line(size = 1.5) + geom_point(colour = "black", size = 3) + labs(x = "Timestamp in hour (daily)", y = "Blood glucose levels mmol/L")   + ggtitle("Individual blood glucose levels variation during a day of patient ID 31 to 35 ") + theme_light() 
individual_all_plot24h
#Collective plot but first recalculate the mean value (patient_all_subset contains mean valuebyhour of different patient ID, patient_all_subset_mean contains the aggregate meean of the whole dataset)
patient_all_subset_mean <- patient_all_subset %>%
  group_by(Hours) %>% mutate(MeanbyHour=mean(Value))
colective_plot24h <- ggplot(patient_all_subset_mean, aes(x=Hours, y=MeanbyHour)) + geom_line(colour = "black", size = 2) + geom_point(colour = "blue", size = 5) + labs(x = "Timestamp in hour (daily)", y = "Blood glucose levels mmol/L")   + ggtitle("Collective blood glucose levels variation during a day of patient ID 31 to 35") + theme_light()
colective_plot24h 


###Part3:
##Cleaning and preparing the data
#Replace all blood glucose measurement codes (Measurements = 1)
patient31$Code <- replace(patient31$Code,patient31$Code<=64 & patient31$Code>=48,1)
patient32$Code <- replace(patient32$Code,patient32$Code<=64 & patient32$Code>=48,1)
patient33$Code <- replace(patient33$Code,patient33$Code<=64 & patient33$Code>=48,1)
patient34$Code <- replace(patient34$Code,patient34$Code<=64 & patient34$Code>=48,1)
patient35$Code <- replace(patient35$Code,patient35$Code<=64 & patient35$Code>=48,1)

#Replace all insulin intake measurement codes (Medications = 0)
patient31$Code <- replace(patient31$Code,patient31$Code<=35 & patient31$Code>=33,0)
patient32$Code <- replace(patient32$Code,patient32$Code<=35 & patient32$Code>=33,0)
patient33$Code <- replace(patient33$Code,patient33$Code<=35 & patient33$Code>=33,0)
patient34$Code <- replace(patient34$Code,patient34$Code<=35 & patient34$Code>=33,0)
patient35$Code <- replace(patient35$Code,patient35$Code<=35 & patient35$Code>=33,0)

#Eliminate medical issues code
patient31_cleaned <- patient31[patient31$Code == 0 | patient31$Code == 1,]
patient32_cleaned <- patient32[patient32$Code == 0 | patient32$Code == 1,]
patient33_cleaned <- patient33[patient33$Code == 0 | patient33$Code == 1,]
patient34_cleaned <- patient34[patient34$Code == 0 | patient34$Code == 1,]
patient35_cleaned <- patient35[patient35$Code == 0 | patient35$Code == 1,]

#use the distinct function to eliminate possible duplicates of (datetime, Code)
patient31_cleaned <- patient31_cleaned %>% distinct(datetime, Code, .keep_all= TRUE)
patient32_cleaned <- patient32_cleaned %>% distinct(datetime, Code, .keep_all= TRUE)
patient33_cleaned <- patient33_cleaned %>% distinct(datetime, Code, .keep_all= TRUE)
patient34_cleaned <- patient34_cleaned %>% distinct(datetime, Code, .keep_all= TRUE)
patient35_cleaned <- patient35_cleaned %>% distinct(datetime, Code, .keep_all= TRUE)


#Divide/Cast Medications = 0 and Measurements = 1 into 2 columns
patient31_dcast <- dcast(patient31_cleaned,ID + Date + Time + datetime + Hours ~ Code, value.var = "Value")
patient32_dcast <- dcast(patient32_cleaned,ID + Date + Time + datetime + Hours ~ Code, value.var = "Value")
patient33_dcast <- dcast(patient33_cleaned,ID + Date + Time + datetime + Hours ~ Code, value.var = "Value")
patient34_dcast <- dcast(patient34_cleaned,ID + Date + Time + datetime + Hours ~ Code, value.var = "Value")
patient35_dcast <- dcast(patient35_cleaned,ID + Date + Time + datetime + Hours ~ Code, value.var = "Value")

#subset measurement (=1) data & remove NAs
patient31_BGM <- na.omit(patient31_dcast[,-6])
patient32_BGM <- na.omit(patient32_dcast[,-6])
patient33_BGM <- na.omit(patient33_dcast[,-6])
patient34_BGM <- na.omit(patient34_dcast[,-6])
patient35_BGM <- na.omit(patient35_dcast[,-6])

#Get the average value of measurements taken in a date
patient31_BGM <- patient31_BGM %>%
  group_by(Date) %>% mutate(MeanbyDate=mean(`1`))
patient32_BGM <- patient32_BGM %>%
  group_by(Date) %>% mutate(MeanbyDate=mean(`1`))
patient33_BGM <- patient33_BGM %>%
  group_by(Date) %>% mutate(MeanbyDate=mean(`1`))
patient34_BGM <- patient34_BGM %>%
  group_by(Date) %>% mutate(MeanbyDate=mean(`1`))
patient35_BGM <- patient35_BGM %>%
  group_by(Date) %>% mutate(MeanbyDate=mean(`1`))


#format the date column
patient31_BGM$Date <-as.Date(patient31_BGM$Date, "%m-%d-%Y")
patient32_BGM$Date <-as.Date(patient32_BGM$Date, "%m-%d-%Y")
patient33_BGM$Date <-as.Date(patient33_BGM$Date, "%m-%d-%Y")
patient34_BGM$Date <-as.Date(patient34_BGM$Date, "%m-%d-%Y")
patient35_BGM$Date <-as.Date(patient35_BGM$Date, "%m-%d-%Y")

#Draw blood glucose change graph for each patient data for Dates
patient31_ploteDate <- ggplot(patient31_BGM, aes(x = Date, y = MeanbyDate)) +           
  geom_line(size = 1.2, color = "orange") + labs(x = "Date", y = "Blood glucose levels mmol/L")   + ggtitle("Individual blood glucose levels variation over dates of Patient ID 31 ") + theme_light() + scale_x_date(date_labels = "%m-%d-%Y")
patient32_ploteDate <- ggplot(patient32_BGM, aes(x = Date, y = MeanbyDate)) +           
  geom_line(size = 1.2, color = "orange") + labs(x = "Date", y = "Blood glucose levels mmol/L")   + ggtitle("Individual blood glucose levels variation over dates of Patient ID 32 ") + theme_light() + scale_x_date(date_labels = "%m-%d-%Y")
patient33_ploteDate <-ggplot(patient33_BGM, aes(x = Date, y = MeanbyDate)) +           
  geom_line(size = 1.2, color = "orange") + labs(x = "Date", y = "Blood glucose levels mmol/L")   + ggtitle("Individual blood glucose levels variation over dates of Patient ID 33 ") + theme_light() + scale_x_date(date_labels = "%m-%d-%Y")
patient34_ploteDate <-ggplot(patient34_BGM, aes(x = Date, y = MeanbyDate)) +           
  geom_line(size = 1.2, color = "orange") + labs(x = "Date", y = "Blood glucose levels mmol/L")   + ggtitle("Individual blood glucose levels variation over dates of Patient ID 34 ") + theme_light() + scale_x_date(date_labels = "%m-%d-%Y")
patient35_ploteDate <-ggplot(patient35_BGM, aes(x = Date, y = MeanbyDate)) +           
  geom_line(size = 1.2, color = "orange") + labs(x = "Date", y = "Blood glucose levels mmol/L")   + ggtitle("Individual blood glucose levels variation over dates of Patient ID 35 ") + theme_light() + scale_x_date(date_labels = "%m-%d-%Y")

patient31_ploteDate
patient32_ploteDate
patient33_ploteDate
patient34_ploteDate
patient35_ploteDate
##Draw blood glucose changes over dates graph
#Merging data frame
patient_all_BGM <- rbind(patient31_BGM,patient32_BGM,patient33_BGM,patient34_BGM,patient35_BGM)
#Collective plot over Dates
patient_all_BGM_mean <- patient_all_BGM %>%
  group_by(Date) %>% mutate(MeanbyHour=mean(`1`))
colective_plotDate <-ggplot(patient_all_BGM_mean, aes(x = Date, y = MeanbyDate)) +           
  geom_line(size = 1.2, color = "blue") + labs(x = "Date", y = "Blood glucose levels mmol/L")   + ggtitle("Collective blood glucose levels variation over dates of Patient ID 31 to 35 ") + theme_light() + scale_x_date(date_labels = "%m-%d-%Y")
colective_plotDate 



  
