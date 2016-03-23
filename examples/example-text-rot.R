# text automatically rotated with respüect to x-axis

lim <- c(-1.5,1.5)
plot(NULL, asp=1, xlim=lim, ylim=lim)
plotrix::draw.circle(0, 0, 1)
s <- 1.05
phis <- seq(0, 2*pi, by=.4)
for (phi in phis) {
  v <- c(cos(phi), sin(phi))
  segments(0,0, v[1], v[2])
  text_rot(v * 1.1, label=phi)
}
