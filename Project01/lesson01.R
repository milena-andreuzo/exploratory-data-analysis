#### Lesson 01 from Exploratory Data Analysis course

# Loading data
path <- "C:/Users/milen/Desktop/IC/exploratory data analysis/household_power_consumption.txt"
data <- as.data.frame(read.table(path, header = TRUE, 
                                 sep = ";", na.strings = "?"))
data2 <- data

# Convert date
data2$Date <- as.Date(data2$Date, "%d/%m/%Y")
head(data2)

# Filtering data because we will use 2007-02-01 and 2007-02-02 dates only
library(dplyr)
data2 <- data2 %>% filter(Date == "2007-02-02" | Date == "2007-02-01")
head(data2)

# Constructing graphics
library(ggplot2)

# Plot 1 - Frequency (y) ~ Global Active Power (x)
png(filename = "plot01.png", width = 480, height = 480)
ggplot(data2, aes(Global_active_power)) + 
    geom_histogram(bins = 18, na.rm = TRUE, color = "black", fill = "red") +
    labs(title = "Global Active Power",
         x = "Global Active Power (kilowatts)",
         y = "Frequency") +
    theme_classic()
dev.off()

# Plot 2 - Global Active Power (y) ~ Weekdays (x)
# Adding column datetime to make the time series possible
data2$datetime <- as.POSIXct(paste(data2$Date, data2$Time),
                             format="%Y-%m-%d %H:%M:%S")

png(filename = "plot02.png", width = 480, height = 480)
ggplot(data2, aes(datetime, Global_active_power)) +
    geom_line(na.rm = TRUE) +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
    labs(x = "", y = "Global Active Power (kilowatts)") +
    theme_classic()
dev.off()

# Plot 3 - Energy Sub metering
png(filename = "plot03.png", width = 480, height = 480)
ggplot(data2) + 
    geom_line(aes(datetime, Sub_metering_1, color = "black"), na.rm = TRUE) +
    geom_line(aes(datetime, Sub_metering_2, color = "red"), na.rm = TRUE) +
    geom_line(aes(datetime, Sub_metering_3, color = "blue"), na.rm = TRUE) +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
    labs(x = "", y = "Energy sub metering") +
    scale_color_manual(name = "",
                       values = c("black", "red", "blue"), 
                       labels = c("Sub_metering_1", 
                                  "Sub_metering_2", 
                                  "Sub_metering_3")) +
    theme(legend.position = c(.89, .94), panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA))
dev.off()


# Plot 4 - A panel of four graphics

# Graph 01 - plot 02
plot1 <- ggplot(data2, aes(datetime, Global_active_power)) +
    geom_line(na.rm = TRUE) +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
    labs(x = "", y = "Global Active Power (kilowatts)") +
    theme(panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA))

# Graph02 - voltage ~ datetime
plot2 <- ggplot(data2, aes(datetime, Voltage)) +
    geom_line(na.rm = TRUE) +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
    labs(x = "datetime", y = "Voltage") +
    theme(panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA))

# Graph 03 - plot 3
plot3 <- ggplot(data2) + 
    geom_line(aes(datetime, Sub_metering_1, color = "black"), na.rm = TRUE) +
    geom_line(aes(datetime, Sub_metering_2, color = "red"), na.rm = TRUE) +
    geom_line(aes(datetime, Sub_metering_3, color = "blue"), na.rm = TRUE) +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
    labs(x = "", y = "Energy sub metering") +
    scale_color_manual(name = "",
                       values = c("black", "red", "blue"), 
                       labels = c("Sub_metering_1", 
                                  "Sub_metering_2", 
                                  "Sub_metering_3")) +
    theme(legend.position = c(.75, .78),
          panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA))

# Graph 04 - Global reactive power ~ datetime
plot4 <- ggplot(data2, aes(datetime, Global_reactive_power)) +
    geom_line(na.rm = TRUE) +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
    labs(x = "datetime", y = "Global reactive power (kilowatts)") +
    theme(panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA))

# Gather the 04 plots and save the png file
png(filename = "plot04.png", width = 480, height = 480)
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
dev.off()