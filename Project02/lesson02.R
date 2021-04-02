#### Course project 02 from Exploratory Data Analysis course

# Loading data
setwd("C:/Users/milen/Desktop/IC/exploratory data analysis/")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)
library(stringr)

# Question 01
total99 <- NEI %>% filter(year == 1999) %>% select(Emissions) %>% sum()
total02 <- NEI %>% filter(year == 2002) %>% select(Emissions) %>% sum()
total05 <- NEI %>% filter(year == 2005) %>% select(Emissions) %>% sum()
total08 <- NEI %>% filter(year == 2008) %>% select(Emissions) %>% sum()

pm25 <- data.frame(Year = c(1999, 2002, 2005, 2008),
                   Total.Emission = c(total99, total02, total05, total08))

png(filename = "question01.png", width = 480, height = 480, units = "px")
with(pm25, plot(Year, Total.Emission, type = "l",
                xlab = "Year", ylab = "PM2.5 emission (tons)", 
                main = "Total Emissions from PM2.5 in US throughout the years"))
dev.off()

# Question 02

balt99 <- NEI %>% filter(fips == "24510" & year == 1999) %>% select(Emissions) %>% sum()
balt02 <- NEI %>% filter(fips == "24510" & year == 2002) %>% select(Emissions) %>% sum()
balt05 <- NEI %>% filter(fips == "24510" & year == 2005) %>% select(Emissions) %>% sum()
balt08 <- NEI %>% filter(fips == "24510" & year == 2008) %>% select(Emissions) %>% sum()
balt <- data.frame(year = c(1999, 2002, 2005, 2008),
                   emission = c(balt99, balt02, balt05, balt08))

png(filename = "question02.png", width = 480, height = 480, units = "px")
with(balt, plot(year, emission, type = "l", lty = 2, xlab = "Year", 
                ylab = "PM2.5 emission (tons)", 
                main = "Total emissions in Baltimore City", pch = 20))
points(x = 1999, y = balt99, pch = 20)
points(x = 2002, y = balt02, pch = 20)
points(x = 2005, y = balt05, pch = 20)
points(x = 2008, y = balt08, pch = 20)
dev.off()

# Question 03
baltq3 <- NEI %>% filter(fips == "24510") %>% select(type, Emissions, year)

summary(baltq3$Emissions)
# Here we see that 75% of the data on emission is under the value 0.87,
# so I decided to move the scale on the y axis to avoid outliers and
# make the plot easier to visualize

png(filename = "question03.png", width = 800, height = 500, units = "px")
ggplot(baltq3, aes(factor(year), Emissions, fill = type)) +
    geom_bar(stat = "identity") +
    facet_grid(cols = vars(type)) +
    labs(title = "Emissions by type of source throughout the years",
         y = "PM2.5 emissions (tons)",
         x = "Year")
dev.off()

# Question 04
# Across the United States, how have emissions from coal
# combustion-related sources changed from 1999–2008?
View(SCC)
# The coal combustion sources are indicated on EI Sector "Fuel Comb - Electric Generation - Coal"
indexes <- SCC %>% filter(str_detect(EI.Sector, "Fuel Comb") & str_detect(EI.Sector, "Coal"))
indexes <- as.character(indexes$SCC)
coal <- NEI %>% select(fips, SCC, Emissions, year) %>% filter(SCC %in% indexes)

png(filename = "question04.png", width = 480, height = 480, units = "px")
with(coal, plot(as.factor(year), log10(Emissions), pch = 20,
                xlab = "Year", ylab = "PM2.5 emissions (tons)",
                main = "Emissions from coal combustion sources"))
dev.off()

# Question 05
index05 <- SCC %>% filter(str_detect(Short.Name, "Vehicle"))
index05 <- as.character(index05$SCC)
motor <- NEI %>% select(fips, SCC, Emissions, year) %>% filter(SCC %in% index05)
motor2 <- filter(motor, fips == "24510") 

png(filename = "question05.png", width = 480, height = 480, units = "px")
with(motor2, plot(as.factor(year), log10(Emissions),pch = 20,
                xlab = "Year", ylab = "PM2.5 emissions (tons)",
                main = "Emissions from vehicle sources in Baltimore"))
dev.off()

# Question 06
index05 <- SCC %>% filter(str_detect(Short.Name, "Vehicle"))
index05 <- as.character(index05$SCC)
motor <- NEI %>% select(fips, SCC, Emissions, year) %>% filter(SCC %in% index05)
motor2 <- filter(motor, fips == "24510") 
motor3 <- filter(motor, fips == "06037") 

png(filename = "question06.png", width = 800, height = 500, units = "px")
par(mfrow = c(1, 2))
with(motor2, plot(as.factor(year), log10(Emissions), pch = 20, ylim = c(-6, 3),
                  xlab = "Year", ylab = "PM2.5 emissions (tons)",
                  main = "Vehicle source emissions in Baltimore"))
with(motor3, plot(as.factor(year), log10(Emissions), pch = 20, ylim = c(-6, 3),
                  xlab = "Year", ylab = "PM2.5 emissions (tons)",
                  main = "Vehicle source emissions in Los Angeles"))
dev.off()
