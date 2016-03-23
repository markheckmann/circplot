## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
  fig.width=5, 
  fig.height=5,
  fig.align = "center"
)

## ----setup, warning=FALSE, message=FALSE---------------------------------
library(circplot)

## ----echo=FALSE----------------------------------------------------------
set.seed(0)
n <- 50
m <- data.frame(a = rnorm(n, 200, 30),
                b = rnorm(n, 120, 40))
head(m)

## ------------------------------------------------------------------------
plot(NULL, xlim=c(1,365), ylim=c(0, 1), xlab="days", ylab="average mood")
abline(h=c(.2, .8), col="grey")
points(cbind(m$a, .2), pch=16, col="red")
points(cbind(m$b, .8), pch=16, col="green")

## ------------------------------------------------------------------------
circ_plot()
circ_par(x.from = c(1,365),
         y.from= c(0,1))
ring_set(1)
circ_points(m$a, .5 , col="red", pch=16)
ring_set(2)
circ_points(m$b, .5, col="green", pch=16)

## ------------------------------------------------------------------------
circ_par()

## ------------------------------------------------------------------------
circ_plot()
ring_show()

circ_par(rings = c(1, 1.1, 1.4, 1.5))
circ_plot()
ring_show()

circ_par(rings = c(1, .8, .6))
circ_plot()
ring_show()

## ----fig.width=7, fig.height=3.5-----------------------------------------
set.seed(0)
x <- rnorm(10, 0, .1)
y <- rep(.5, length(x)) 

par(mfrow=c(1,2))
circ_par_init()
circ_par(rings = c(1, 1.2, 1.4))

# Points
circ_plot()
title("Points")
ring_show()
ring_set(1)
circ_points(x, y, col="red", pch=16)

# Text
circ_plot()
title("Text")
ring_show()
ring_set(2)
circ_text("Some text", mean(x), .5)

## ----fig.width=7, fig.height=3.5-----------------------------------------
par(mfrow=c(1,2))

# Line
circ_plot()
title("Line")
ring_show()
ring_set(1)
circ_lines(min(x), .5, max(x), .5, lwd=2, col="blue")

# Rectangle
circ_plot()
title("Rectangle")
ring_show()
ring_set(2)
circ_rect(min(x), 0, max(x), 1, lwd=2, col=grey(.9))


## ----fig.width=7, fig.height=3.5-----------------------------------------
par(mfrow=c(1,2))

# Boxplot
circ_plot()
title("Boxplot")
ring_show()
ring_set(1)
circ_boxplot(x, col="#FF000030")

# Density
circ_plot()
title("Density plot")
ring_show()
ring_set(2)
circ_density(x, col="#FF000030")

## ----fig.width=7, fig.height=3.5-----------------------------------------
par(mfrow=c(1,2))

# Funnel plot
circ_plot()
title("Funnel")
ring_show()

x1 <- x/3
x2 <- x/3 + .1
x3 <- x/2 + .4

ring_set(2)
circ_points(x2, .5, col="green", pch=16)
circ_funnel(x2, col="#00FF0030")

ring_set(1)
circ_points(x1, .5, col="red", pch=16)
circ_funnel(x1, outer=FALSE, col="#FF000030")

# setting the radius explicitly
circ_points(x3, .5, col="blue", pch=16)
circ_funnel(x3, r=1.1, col="#0000FF30")

## ------------------------------------------------------------------------
circ_par_init()
circ_plot()
circ_par(x.from = c(1,365),
         y.from= c(0,1),
         rings = c(1, 1.1, 1.15, 1.3))
ring_set(1)
circ_points(m$a, .5 , col="red", pch=16)
circ_points(m$b, .5, col="green", pch=16)
ring_set(3)
circ_boxplot(m$a)
circ_boxplot(m$b)

## ------------------------------------------------------------------------
circ_plot()
circ_par(rings = c(1, 1.1, 1.3))
ring_set(1)
circ_points(m$a, .5 , col="red", pch=16)
circ_points(m$b, .5, col="green", pch=16)
ring_set(2)
circ_density(m$a, col="#FF000030")
circ_density(m$b, col="#00FF0030")

## ------------------------------------------------------------------------
set.seed(0)
n <- 50
m$c = rnorm(n, 90, 10)
m$d = rnorm(n, 95, 20)
m$e = rnorm(n, 40, 10)

circ_par_init()
circ_plot()
circ_par(x.from = c(1, 365),
         rings = seq(1.1, 1.6, .1))
ring_set(1)
circ_boxplot(m$a, "GROUP A", height=.8)
ring_set(2)
circ_boxplot(m$b, "GROUP B", height=.8)
ring_set(3)
circ_boxplot(m$c, "GROUP C", height=.8)
ring_set(4)
circ_boxplot(m$d, "GROUP D", height=.8)
ring_set(5)
circ_boxplot(m$e, "GROUP E", height=.8)

## ------------------------------------------------------------------------
circ_plot()

phis <- seq(0, 2*pi, by=.4)
for (phi in phis) {
  v <- c(cos(phi), sin(phi))
  text_rot(v * 1.1, label=phi)
}

