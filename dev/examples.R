#### example ####

# # cartesian coords
# plot(NULL, xlim=c(0, 10), ylim=c(0,1))
# x = 0:10
# y = runif(11, 0, 1)
# d <- cbind(x,y)
# lines(d)
# 
# xx <- seq(0, 1, length=100)
# xx <- c(xx, rev(xx))
# yy <- rep(c(0,1), each=100)
# polygon(xx, yy)


# polar coordinates

n <- 100
a <- rnorm(n, 2, .5)
b <- rnorm(n, 2.3, .6)
a.rad <- runif(n)
b.rad <- runif(n)

d <- data.frame(a = rnorm(n, 2, .5),
                a.r = runif(n),
                b = rnorm(n, 2.3, .6),
                b.r = runif(n))

#circ_par(y.to = c(0, 1))  
plot(NULL, xlim=c(-1.5, 1.5), ylim=c(-1.5,1.5), asp=1)
draw_circle(0, 0, 1)

aa <- circ_convert(d$a, d$a.r)
segments(0, 0, aa[, 1], aa[, 2], col="#000000FF")

circ_par(y.to= c(1., 1.05))
circ_points(a, 0, pch=16, cex=.4, col="#000000FF")
circ_par(y.to= c(1.07, 1.13))
circ_boxplot(a, "group a")

circ_par(y.to= c(1.15, 1.25))
circ_boxplot(b, "group b")


# polar coordinates - ring plots
plot(NULL, xlim=c(-1.5, 1.5), ylim=c(-1.5,1.5), asp=1)
draw_circle(0, 0, 1)
circ_par(y.to= c(0, 1))
aa <- circ_convert(a, a.rad)
segments(0, 0, aa[, 1], aa[, 2], col="#00FF00FF")
bb <- circ_convert(b, b.rad)
segments(0, 0, bb[, 1], bb[, 2], col="#ff0000FF")


circ_par(rings=1:4/10 + 1)
ring_draw()
ring_set(2)
circ_par("y.to")

draw_circle(0, 0, 1.1, border="grey")
circ_par(y.to= c(1.1, 1.1))
circ_points(a, 0, pch=16, cex=.4, col="#00FF00FF")

draw_circle(0, 0, 1.2, border="grey")
circ_par(y.to= c(1.2, 1.2))
circ_points(b, 0, pch=16, cex=.4, col="#FF0000FF")

circ_par(y.to= c(1, 1))
circ_funnel(a, col="#00ff0030")
circ_funnel(b, col="#FF000030")

# polar coordinates - densities
plot(NULL, xlim=c(-1.5, 1.5), ylim=c(-1.5,1.5), asp=1)
draw_circle(0, 0, 1)
circ_par(y.to= c(1, 1.2))
circ_density(a, col="#00FF0030")
circ_density(b, col="#FF000030")

#### Defining rings ####

# polar coordinates - ring plots
plot(NULL, xlim=c(-1.5, 1.5), ylim=c(-1.5,1.5), asp=1)
circ_par(rings=1:4/10 + 1)
ring_draw()
circ_par("y.to")    # default settings
ring_set(1)
circ_par("y.to")    # has changed to first ring radi
ring_set(2)
circ_par("y.to")

# draw in ring 1
a <- rnorm(100, 2, .5)
ring_set(1)
circ_density(a, col="#00FF0030")
ring_set(2)
circ_density(a, col="#00FF0030")







