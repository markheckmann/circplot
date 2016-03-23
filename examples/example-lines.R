circ_plot()
circ_par_init()
ring_set(1)
circ_lines(0,.5, .2, lwd=2)
ring_set(2)
circ_lines(1,.5, .2, lwd=2)

# lines are drawn using increasing x values by default.
ring_set(3)
circ_lines(1,.5, .2, increasing=FALSE, lwd=2)
