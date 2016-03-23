
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
#' @export
#' 
draw_circle <- function(...)
{
  plotrix::draw.circle(...)
}



#' Check if x is a matrix and convert vectors to matrix
#' 
#' @keywords internal
#' 
check_matrix <- function(x)
{
  if (is.vector(x)) 
    x <- matrix(x, nrow=1)
  if (! is.matrix(x) & ! is.data.frame(x))
    stop("'x' must be a vector, matrix or dataframe")
  x
} 


#' Angle / radian between two vectors.
#' 
#' Function comes in two flavors. Either it calculates the smallest angle 
#' [0, 180) or the anti-clockwise angle with respect to the second 
#' vector [0, 360).
#' 
#' @param a,b  Vector
#' @param rad  Angular measure is radian?  (default \code{TRUE}).
#' @param mode 1= smallest angle in [0, 180); 2=anti-clockwise angle with respect 
#'   vector b in [0, 360).
#' @return Angle
#' @export
#' @keywords internal
#' 
vec_angle <- function(a, b, rad=TRUE, mode=1) 
{
  # minimal radian between vectors
  if (mode == 1) {
    r <- acos(sum(a * b) /     
                ( sum(a^2)^.5 * sum(b^2)^.5) )    
  } 
  # anti-clockwise radian with respect to vector b
  if (mode == 2) {
    #r = atan2(a[1]*b[2] - b[1]*a[2], a[1]*b[1] + a[2]*b[2])
    r = atan2(b[1]*a[2] - a[1]*b[2], b[1]*a[1] + b[2]*a[2])
    
    #     r = (-180/pi * ang) %% 360     # restrict to 0 to 260 range     
    #     r = r * pi /180                # convert to radians
    r = r %% (2*pi)   # restrict to 2pi range     
  }
  
  if (!rad)   # return degress if requested
    r <- r * 180 / pi  # convert radians to angle
  r
}



#' Angles / radian between two or more vectors.
#' 
#' Function comes in two flavors. Either it calculates the smallest angle 
#' [0, 180) or the anti-clockwise angle with respect to the second 
#' vector [0, 360).
#' 
#' @param a  A matrix or a vector
#' @param b  A reference vector to find angle against.
#' @param rad  Angular measure is radian?  (default \code{TRUE}).
#' @param mode 1= smallest angle in [0, 180); 2=anti-clockwise angle with respect 
#'    vector b in [0, 360).
#' @return Vector of angles
#' @export
#' @keywords internal
#' @example examples/example-vecangles.R
#' 
vec_angles <- function(a, b, rad=TRUE, mode=1)
{
  a <- check_matrix(a)
  b <- check_matrix(b)
  if (nrow(b) != 1)
    stop("b must be a vector or matrix with one row.")
  ii <- 1L:nrow(a)
  sapply(ii, function(i) {
    vec_angle(a[i, ], b, mode=mode, rad=rad)
  })
}

