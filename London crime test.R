#1.Read the dataset into a data frame london_crime
london_crime <- read.csv("london-crime-data.csv")
View(london_crime)
#Show the structure of the dataset
str(london_crime)


#Create new variable called Date and combine the day, month, and year variables
#The format here is dd-mm-yyyy, but you can adjust the separator or order as needed
london_crime$Date <- paste(london_crime$day, london_crime$month, london_crime$year, sep="-")

#Show the structure again to verify the new Date variable
str(london_crime)

#2.Make adjustments to Variable names 
names(london_crime)[names(london_crime) == "borough"] <- "Borough"
names(london_crime)[names(london_crime) == "major_category"] <- "MajorCategory"
names(london_crime)[names(london_crime) == "minor_category"] <- "SubCategory"
names(london_crime)[names(london_crime) == "value"] <- "Value"
names(london_crime)[names(london_crime) == "Date"] <- "CrimeDate"
names(london_crime)
View(london_crime)

#3.Convert CrimeDate variable to Date type 
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, format="%d-%m-%Y")

#Show the structure of london_crime data frame to confirm the changes implemented
str(london_crime)

#Show the content of the CrimeDate variable to confirm the content is as expected
head(london_crime$CrimeDate)

#4. plot chart to show summary of borough information 
borough <- table(london_crime$Borough)
barplot(borough, 
        main="Summary of Crime by Borough", 
        xlab="Borough city", 
        ylab="sum total of crimes commited", 
        las=2, # Makes the borough names perpendicular to the axis
        cex.names=0.8)
 
#i.Croydon leads in the highest level of crime commited in Borough
#ii.City of London records the lowest level of crime commited. 

#5 
windows(16,10)
major_category <- table(london_crime$MajorCategory)
pie(major_category, 
    main="Major Crime ", 
    col=rainbow(length(major_category)))

#Theft and handling is the leading major category of crime in London
#London records Fraud or Forgery as the lowest level of crime

#6 categorise borough in london_crime into general areas within london
Region <- c("East", "North", "East", "West", "South", "North", "South", "West", "North", "East", "North", "West", "North", "West", 
            "East", "West", "West", "Central", "Central", "East", "Central", "Central", "South", "East", "East", "West", "Central",
            "South", "South", "Central", "Central", "East", "Central")


london_crime$Region <- Region[london_crime$Borough]
london_crime <- merge(london_crime, Region, by = "Borough", all.x = TRUE)
na_boroughs <- which(is.na(london_crime$Region))
london_crime$Region <- rep(Region, each = nrow(london_crime))

#7 region with highest recorded crime rate 
crimes <- table(london_crime$Region)
plot(crimes, 
     main="Region in London", 
     xlab="Region", 
     ylab=" Crimes reported", 
     col="blue",
     las=2)
# The east region is confirmed with the highest number of crime estimably 25500
# The south region records the least reported crime with a number of 15000 

#8 

#9 

#10 