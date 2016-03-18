
#' Set and query circplot settings
#' 
#' @param ... Key values pairs seperated by comma or a list of key value pairs as 
#'  e.g. returned by the function itself.
#' @examples
#' circ_par()
#' circ_par(x.from = c(0,1))   # change input range from 0 to 1
#' 
circ_par <- function(...)
{
  cur.settings <- options("circ")[[1]]
  parnames <- names(cur.settings)
  
  args <- list(...)
  
  if (length(args) == 0) {
    return(cur.settings)
  }
  
  # flatten if a list key values is supplied (e.g. the old par object)
  # Note that list(...) wraps it into another list which is not wanted
  if (is.list(args[[1]])) {
    args <- args[[1]]
  }
      
  if (is.null(names(args)) & all(unlist(lapply(args, is.character)))) {
    pm <- pmatch(unlist(args), parnames)
    return(cur.settings[na.omit(pm)])
  } else {
    names(args) <- parnames[pmatch(names(args), parnames)]
    new.settings <- modifyList(cur.settings, args)
    options(circ=new.settings)
    invisible(new.settings)
  }
}


#' Initialize circplot parameters
#' 
#' The values are saved in the field \code{circ} using the \code{options} function.
#' The following parameters can be set:
#' \enumerate{
#'   \item x.from Range of original data (default \code{[0,1]}).
#'   \item x.to Range data is mapped to, usually 0 to 2pi.
#'   \item y.from Range of original data (default \code{[0,1]}).
#'   \item y.to Range data is mapped to (default \code{[1,1.1]})
#'   \item n.seg Number of segments used when drawing lines.
#'   \item rings Border radi of rings (defaults is three rings \code{c(1.0, 1.1, 1.2, 1.3)})
#' }
#' 
circ_par_init <- function()
{
  l <- list()
  l$x.from <- c(0, 1)
  l$x.to <- c(0, 2*pi)
  l$y.from <- c(0, 1)
  l$y.to <- c(1, 1.1)  
  l$n.seg <- 100          # number of segments used to draw forms
  l$rings <- 10:14/10
  # circ$width <- .1
  # circ$line <- 1
  options(circ=l)
}


#' Workhorse: original range to cartesian coords according to \code{circ_par} settings.
#' 
#' @param x,y Coordinates from original data range
#' @param to.cart Convert to cartesian coordinates? (default \code{TRUE}).
#' 
circ_convert <- function(x, y, to.cart=TRUE)
{
  x.from <- circ_par()$x.from
  if (is.na(x.from[1]))
    x.from <- range(x)
  
  y.from <- circ_par()$y.from
  if (is.na(y.from[1]))
    y.from <- range(y)
  x.to <- circ_par()$x.to
  y.to <- circ_par()$y.to
  
  theta = scales::rescale(x, to = x.to, from=x.from)
  rho = scales::rescale(y, to = y.to, from=y.from)
  p = cbind(theta, rho)
  if (to.cart) {
    p <- pracma::pol2cart(p)
    p <- as.matrix(rbind(p))    # make sure that always a matrix is returned
    colnames(p) <- c("x", "y")
  } 
  p
}


#' Set up default plotting area
#' 
#' This is just a convenient wrapper for \code{plot} for pure convencience and 
#' quick results.
#' 
circ_plot <- function(xlim=NULL, ylim=NULL, ...)
{
  par(mar=rep(2, 4))
  y.to <- circ_par()$y.to
  mx <- max(y.to)
  mx <- mx * 1.4 
  lim <- c(-mx, mx)
  if (is.null(xlim))
    xlim <- lim
  if (is.null(ylim))
    ylim <- lim
  
  plot(NULL, xlim=xlim, ylim=xlim, asp=1, xlab="", ylab="", las=1, cex.axis=.7, ...)
  draw_circle(0,0,1)
}


#' Draw lines
#' 
circ_lines <- function(x0, y0, x1 = x0, y1 = y0, ...)
{
  n.seg <- circ_par()$n.seg
  x <- seq(x0, x1, length = n.seg)
  y <- seq(y0, y1, length = n.seg)
  c <- circ_convert(x, y)
  lines(c, ...)
}


#' Draw polygon
#' 
circ_polygon <- function(x, y, ...)
{
  c <- circ_convert(x, y)
  polygon(c, ...)
}


#' Draw rectangle
#' 
circ_rect <- function(xleft, ybottom, xright, ytop, ...)
{
  xx_lr <- seq(xleft, xright, length=100)
  yy <- rep(c(ybottom, ytop), each=100)
  
  xx <- c(xx_lr, rev(xx_lr))
  circ_polygon(xx, yy, ...)
}


#' Draw boxplot
#' 
circ_boxplot <- function(x, label=NA, height=1, cex=.7, col=grey(.95), ...)
{   
  f <- fivenum(x)
  lo <- (1 - height) / 2
  hi <- lo + height
  
  circ_rect(f[2], lo, f[4], hi, col=col, ...)
  circ_lines(f[1], .5, f[2], .5)
  circ_lines(f[4], .5, f[5], .5)
  circ_lines(f[3], lo, f[3], hi, lwd=2)
  circ_lines(f[1], lo, f[1], hi)
  circ_lines(f[5], lo, f[5], hi)
  
  if (!is.na(label)) {
    offset <- diff(circ_par()$x.from) / 150
    y <- diff(circ_par()$y.from) / 2
    circ_text(label, f[1] - offset, y, xadj=0, cex=cex)    
  }
}


#' Draw density plot
#' 
circ_density <- function(x, ...) 
{
  xy <- density(x, from=min(x), to=max(x))
  xx <- c(xy$x, rev(xy$x))
  xy$y <- scales::rescale(xy$y)
  yy <- c(xy$y, rep(0, length(xy$x)))
  circ_polygon(xx, yy, ...)  
}


# Draw text
#  
# circ_text <- function(label, x, y, ...)
# {
#   p <- circ_convert(x, y, to.cart=FALSE)
#   #p <- pracma::cart2pol(c)
#   plotrix::arctext(x = label, radius = p[2], start = p[1], ...)  
# }


#' Draw text
#' 
#' Has to be rewritten to include nice \code{facing} options (see \code{circlize::circos.text}).
#' Currently \code{plotrix::arctext} has a buch when setting \code{clockwise = FALSE}
#' and  using the \code{end} argument. Once this is fixed (mail to Jim Lemon on 2016-03-18)
#' \code{circ_text} can be improved. Especially nicefacing would be good.
#' @param label A label.
#' @param x,y Coordinates in original system
#' @param xadj x adjustement. One of \code{c(0, .5, 1)}.
#' @param ... passed to \code{plotrix::arctext}.
#' 
circ_text <- function(label, x, y,  xadj=.5, ...)
{
  p <- circ_convert(x, y, to.cart=FALSE)
  #p <- pracma::cart2pol(c)
  
  # x-positioning of text
  if (xadj == 0)
    plotrix::arctext(x = label, radius = p[2], start = p[1], ...)  
  if (xadj == .5)
    plotrix::arctext(x = label, radius = p[2], middle = p[1], ...)  
  if (xadj == 1)
    plotrix::arctext(x = label, radius = p[2], end = p[1], ...)  
}




#' Draw points
#' 
circ_points <- function(x, y, ...)
{
  p <- circ_convert(x, y)
  points(p, ...)
}


# #' Draw points
# #' 
# #' @param x Original data.
# #' @param r Radius for plot (optional).
# #' 
# circ_funnel <- function(x, r=NULL, ...)
# {
#   # temporarily replace y.to
#   old <- circ_par()$y.to
#   if (!is.null(r)) {
#     circ_par(y.to = c(r,r))    
#   }
#   
#   rng <- range(x)
#   xx <- seq(rng[1], rng[2], length=100)
#   yy <- rep(1, length(xx))
#   c <- circ_convert(xx, yy)
#   xc <- c[, 1]
#   yc <- c[, 2]
#   xc <- c(0, xc)
#   yc <- c(0, yc)
#   polygon(xc, yc, ...)
#   circ_par(y.to = old)    
# }


#' Draw funnel over range of points
#' 
#' @param x Original data.
#' @param y Choose where the edge of the funnel is (scaled to \code{[0,1]}).
#' @param r Radius for plot (optional). If not set the max of \code{circ_par("y.to")} 
#'  is used as radius.
#' @param outer Use outer radius for drawing? (default \code{TRUE}).
#' 
circ_funnel <- function(x, r=NULL, outer=TRUE, ...)
{
  old <- circ_par()   # save old pars
  
  # inner or outer radius used as edge?
  if (outer) {
    f <- max
  } else {
    f <- min
  }
  
  # radius is supplied explicitly
  if (is.null(r)) {
    r <- f(old$y.to)    
  } 
  
  # temporarily replace pars to standardize input
  # and output range
  circ_par(y.from = c(0, 1),  
           y.to = c(0, r))    

  rng <- range(x)  
  circ_rect(rng[1], 0, rng[2], 1, ...)
  circ_par(old)    
}





#### Dev ####

# n rings are specfied by n + 1 radi. Two succesive values specify the 
# radi of the ring. The values can be decreasing or increasing. The rings are 
# indexed succesively


# get info about ring 
#
ring_get <- function(index=1) 
{
  rr <- circ_par()$rings
  if (is.na(rr[1]))
    stop("no rings defined. Please defined rings radi using circ_par", call. = FALSE)
  n.rings <- length(rr) - 1
  if (index > n.rings)
    stop("ring with index ", index, " is not defined", call. = FALSE)
  
  radi <- rr[index:(index + 1)]  
  list(radi = radi)
}


#' Set active ring
#' 
ring_set <- function(index=1)
{
  r <- ring_get(index)
  circ_par(y.to = r$radi)
}


# get number of rings defined
#
rings_n <- function()
{
  rr <- circ_par()$rings
  if (is.na(rr[1]))
    return(NA)
  length(rr)- 1
}


#' Show one or all rings
#'
ring_show <- function(index=NULL, ...)
{
  if (is.null(index))
    index <- 1L:rings_n()
  
  for (i in index) {
    ring_show_(i, ...)
  }
}


# Show one ring
# 
ring_show_ <- function(index, col=grey(.8), ...)
{
    r <- ring_get(index)
    rr <- r$radi
    
    draw_circle(0, 0, rr[1], border=col, ...)    
    draw_circle(0, 0, rr[2], border=col, ...)        
    
    r.mean <- mean(rr)
    plotrix::arctext(paste("ring:", index), radius = r.mean, middle = pi/2, cex=.7, col=col)  
}


#rings.padding <- c()










