setwd("C:/Users/glenr/Desktop/fun_prob_and_stats_projects/trump_tweets")
tweets <- read.csv("trump_in_office_tweets_1_October_2019.csv",
	stringsAsFactors = F)
tweets <- tweets[-dim(tweets)[1], ]
str(tweets)

# For time parsing.
library(lubridate)

# Pull out times and convert to time format vector.
times <- mdy_hms( tweets$created_at )
days <- date(times)

# Calculate a density curve.
dens_m <- density(table(days), from = 0, to = max(table(days)), bw = 3)

# Plot it.
png(file = "trump_tweets_per_day.png",
	unit = "in", width = 4.25, height = 3.5, res = 300,
	type = "cairo")
par(mgp = c(2, 0.6, 0), mar = c(4, 4, 2, 1))

plot(dens_m, 
	las = 1, xlab = "", ylab = "",
	main = "", type = "n", axes = T,
	ylim = c(0, 0.05), yaxt = "n")
axis(side = 2, at = seq(0, 0.05, 0.01))
mtext(side = 1, line = 1.7,
	"Number of tweets per day")
mtext(side = 2, line = 1.9,
	"Probability density")
X <- dens_m$x
Y <- dens_m$y
X2 <- c(min(X), X, max(X))
Y2 <- c(0, Y, 0)
polygon(X2, Y2, 
	col = "#ff7800",
	lwd = 2)
segments(median(table(days)), 0, 
	median(table(days)), 35, 
	lty = 3, lwd = 2, xpd = T,
	col = "white")
polygon(X2, Y2, 
	col = "transparent", border = "#e57c00",
	lwd = 2)

# Add median notation.
xpos <- median(table(days)); ypos <- max(Y)
text(xpos, ypos, pos = 3, offset = 0.2,
	paste("Median =", signif(median(table(days)), 3)),
	xpd = T, font = 3)

# Add title.
mtext(side = 3, line = 0.35,
	"President Trump's tweets", font = 2)

# Add data citation.
text(25, 0.035, pos = 4, cex = 0.75,
	"Data source: 
	trumptwitterarchive.com.
	Inauguration date to 
	1 October 2019.")

# Add signature.
text(-14, -0.017, xpd = T, font = 3, pos = 4, cex = 0.75,
	"Graphic by u/plantologist")

dev.off()


# Convert to days of week, and take count by day.
wdays <- as.numeric( table( wday(days) ) )

# Plot it.
png(file = "trump_tweets_per_day_of_week.png",
	unit = "in", width = 4, height = 3.5, res = 300,
	type = "cairo")
par(mgp = c(1.5, 0.6, 0), mar = c(4, 5, 3, 2))
plot(wdays, 7:1, type = "n",
	xlab = "Number of tweets since taking office",
	ylab = "", yaxt = "n",
	xlim = c(0, max(wdays)),
	ylim = c(0.5, 7.5),
	xaxt = "n")
axis(side = 1, at = seq(0, 300, 50), labels = c("0", "", "100", "", "200", "", "300"))
text(rep(-5, 7), 7:1, pos = 2, xpd = T,
	c("Sunday", "Monday", "Tuesday", 
	"Wednesday", "Thursday", "Friday", "Saturday"))
segments(rep(0, 7), 7:1, wdays, 7:1,
	lty = 3)
points(wdays, 7:1, pch = 19, cex = 0.66)

# In-plot number labels.
text(wdays + 10, 7:1 + 0.25, pos = 2,
	as.character(wdays), cex = 0.75,
	col = "gray33")

# Annnotation and title.
mtext(side = 3, line = 1.15,
	"President Trump's tweets by day of week", font = 2)

# Data citation.
mtext(side = 3, line = 0.25, cex = 0.75,
	"Data source: trumptwitterarchive.com, 1 October 2019.")


dev.off()


## Distributions by day of week, pulsar plot.
day_of_week <- wday(days)
sundays <- table(days[day_of_week == 1])
mondays <- table(days[day_of_week == 2])
tuesdays <- table(days[day_of_week == 3])
wednesdays <- table(days[day_of_week == 4])
thursdays <- table(days[day_of_week == 5])
fridays <- table(days[day_of_week == 6])
saturdays <- table(days[day_of_week == 7])

# Plot distibution of numbers of tweets for each day of the week.
# ! First, must read in other script: pulsar_plot.R
L <- list(sundays, mondays, tuesdays, wednesdays, thursdays, fridays, saturdays)

png(file = "trump_tweets_per_day_pulsar.png",
	unit = "in", width = 4.75, height = 4.75, res = 300,
	type = "cairo")
par(mar = c(4, 5, 1, 1))
pls <- pulsar(L, spacing = 0.66, method = "polygon", fill = "#e57c00", 
	return.ypos = T, median.lty = 3)
axis(side = 1)
mtext(side = 1, line = 2, "Number of tweets per day")

# Add labels.
text(rep(0, length(pls)), pls,
	c("Sundays", "Mondays", "Tuesdays", "Wednesdays", 
	"Thursdays", "Fridays", "Saturdays"),
	xpd = T, pos = 2)

# Add n values onto plot.
text(rep(55, 7), pls + 0.1, pos = 2, cex = 0.75,
	paste( "n =", table(day_of_week) ))

# Title and citation.
mtext(side = 3, line = -0.2, "President Trump's tweets by day of week")
mtext(side = 3, line = -1, cex = 0.75,
	"Data source: trumptwitterarchive.com, 1 October 2019.")

# Signature.
text(-12, -1.25, xpd = T, font = 3, pos = 4, cex = 0.66,
	"Graphic by u/plantologist")
dev.off()




