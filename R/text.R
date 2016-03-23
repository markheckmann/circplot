### special text functions


#' Plot text rotated depending angle to x-axis  
#' 
#' @inheritParams graphics::text
#' @example examples/example-text-rot.R
#' @export
#' 
text_rot <- function(x, y=NULL, label, ...)
{
  xy = xy.coords(rbind(x), y)
  x = xy$x
  y = xy$y
  v <- cbind(x,y)
  a <- vec_angle(v, c(1,0), mode=2, rad = F)     # angles to x axis
  srt <- ifelse (a > 90 & a < 270, a-180, a)      # text rotation depends on quadrant to please the eye
  adj <- ifelse (a > 90 & a < 270, 1, 0)          # rext adjustment by quadrant
  text(v[1], v[2], labels = label, adj=adj, srt=srt, ...)
}

 



