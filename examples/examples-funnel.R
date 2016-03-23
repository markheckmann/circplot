# simple boxplot
x <- c(0.06, 0.1, 0.19, 0.9, 0.1, 0.1, 0.07)
circ_plot()
circ_par_init()
ring_set(1)
circ_points(x, .5 , col="red", pch=16)
ring_set(2)
circ_funnel(x)
