
circ_plot()
circ_par_init()
ring_set(1)
circ_rect(0,0,.2,1, col="#FF000030")
ring_set(2)
circ_rect(1,0,.2,1, col="#FF000030")

# rectangles are drawn using increasing values by default.
ring_set(3)
circ_rect(1,0,.2,1, increasing=FALSE, col="#FF000030")
