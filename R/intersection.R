#' Find intersections of circles given a value of tau
#'
#' @param df A dataframe with two columns
#' @param tau A radial distance to calculate if the circles intersect.
#'
#' @return A dataframe of intersections given tau.
#' @export
#'
intersection <- function(df, tau){
  
  intersections <- data.frame()
  
  colnames(df) <- c('x', 'y')
  
  for(i in 1:nrow(df)) {
    for(j in (i+1):nrow(df)) {
      if(!is.na(df$x[i]) & !is.na(df$y[i]) & !is.na(df$x[j]) & !is.na(df$y[j])) {
        if(distance(df$x[i], df$y[i], df$x[j], df$y[j]) < 2 * tau) {
          intersections <- rbind(intersections, c(df$x[i], df$y[i], df$x[j], df$y[j]))
        }
      }
    }
  }
  return(intersections)
}