# -----------------------
# Graphing Sunrise/Sunset
# Data from http://www.timeanddate.com
# 40° 35' 7" N / 105° 5' 2" W
# ----------------------------
# 2010-12-17
# ------------
data <- read.csv("SunsetFoCo.csv", header = TRUE)
data <- apply(data, 2, gsub, pattern = " AM", replacement = ":00")
data <- apply(data, 2, gsub, pattern = " PM", replacement = ":00")
data <- apply(data, 2, gsub, pattern = "h ", replacement = ":")
data <- apply(data, 2, gsub, pattern = "m ", replacement = ":")
data <- apply(data, 2, gsub, pattern = "s", replacement = "")
data <- as.data.frame(data)
data$Date <- strptime(data$Date, format = "%d %b %y")
data <- as.data.frame(data)
head(data)
dim(data)


##############
# Plotting
##############
ynames <- levels(data$Sunset)
L      <- length(ynames); #136%%8
index  <- seq(1, L, by = 6)
early  <- median(which(as.numeric(data$Sunset) == min(as.numeric(data$Sunset))))
early_set <- as.numeric(data$Date[early])
late      <- median(which(as.numeric(data$Sunset) == max(as.numeric(data$Sunset))))
late_set  <- as.numeric(data$Date[late])

plot(data$Date, data$Sunset, ylab = "Sunset", xlab = "Day", type="s",
     col = "darkred", lwd = 1.5, yaxt = "n")

abline(v = late_set, col = "darkgreen", lwd = 1.5, lty = 2)
abline(v = early_set, col = "navy", lwd = 1.5, lty = 2)
legend("topleft", legend = c(format(late_set),format(early_set)), bg = "gray90",
       box.lty = 0, lty = 2, col = c("darkgreen", "navy"), lwd = 1.5)
title(main = "Sunset in Fort Collins, CO - Lat: 40° 35' 7'' N")
axis(2, at = index, labels = ynames[index], cex.axis = 0.66, tick = TRUE,
     col.ticks = "navy", lwd.ticks = 2, las = 1, tcl = -0.5, hadj = 0.9)

