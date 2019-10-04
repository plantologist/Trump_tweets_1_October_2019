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
	c("Sundays", "Mondays", "Tuesdays", 
	"Wednesdays", "Thursdays", "Fridays", "Saturdays"))
segments(rep(0, 7), 7:1, wdays, 7:1,
	lty = 3)
points(wdays, 7:1, pch = 19, cex = 0.66)

# In-plot number labels.
text(wdays + 10, 7:1 + 0.25, pos = 2,
	as.character(wdays), cex = 0.75)

# Annnotation and title.
mtext(side = 3, line = 1.15,
	"President Trump's tweets by day-of-week", font = 2)

# Data citation.
mtext(side = 3, line = 0.25, cex = 0.75,
	"Data source: trumptwitterarchive.com, 1 October 2019.")


dev.off()


