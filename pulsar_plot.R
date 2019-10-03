pulsar <- function(x, spacing = 0.33, top = 0.66, 
     bw = NA, method = "line", 
     col = NA, fill = NA, lwd = 1, return.ypos = F,
	median.lty = NA)
{
     scl <- function(x){ ( x - min(x) ) / ( max(x) - min(x) ) }
     y_lim <- c(0, (length(x) * spacing) + top)
     x_min <- min( unlist( lapply(x, min) ) )
     x_max <- max( unlist( lapply(x, max) ) )
     x_lim <- c(x_min, x_max)
     plot(0, 0, 
          type = "n", axes = F,
          xlim = x_lim, ylim = y_lim,
          xlab = "", ylab = "")
     for(i in 1:length(x))
     {
          if(is.na(bw))
          {
               dens <- density(x[[i]], from = x_min, to = x_max)
          }else
          {
               dens <- density(x[[i]], 
                    from = x_min, to = x_max, bw = bw)
          }
          y_adj <- spacing * (length(x) - i)
          Y <- c( y_adj, scl( dens$y ) + y_adj, y_adj )
          X <- c( min(dens$x), dens$x, max(dens$x) )
          col <- ifelse(is.na(col), par("fg"), col)
          if(length(col) == 1){col <- rep(col, length(x))}
          if(length(fill) == 1){fill <- rep(fill, length(x))}
          if(method == "polygon")
          {
               polygon(X, Y, col = fill[i], border = col[i], lwd = lwd)
          }else
          {
               lines(X, Y, col = col[i], lwd = lwd)
          }
		if(!is.na(median.lty) & (i != 1))
		{
			ytop <- Y[ which.min( abs( X - median(x[[i]]) ) ) ]
			segments(median(x[[i]]), 0, median(x[[i]]), ytop,
				lty = median.lty)
		}
		if(!is.na(median.lty) & (i == 1))
		{
			segments(median(x[[i]]), y_adj, median(x[[i]]), y_adj + 1,
				lty = median.lty)
			text(median(x[[i]]), y_adj + 1, pos = 3, xpd = T,
				font = 3, offset = 0.25, cex = 0.75, "Median")
		}
          if(i == length(x))
          {
               yax <- pretty(dens$y)
               #axis(side = 2, at = scl(yax), labels = yax)
          }
     }
     if(return.ypos)
     {
          return( seq( from = 0, to = spacing * (length(x) - 1), 
               by = spacing ) + (spacing/2) )
     }
}