
#### UTILS ####

as.radian <- function (degree) 
{
  degree/180 * pi
}


as.degree <- function (rad) 
{
  rad * 180 / pi
}

# as.degree(as.radian(1:18*20))


#' Draw a circle
#' 
#' Wrapper for \code{\link{plotrix::draw.circle}}
#' 
draw_circle <- function(...)
{
  plotrix::draw.circle(...)
}