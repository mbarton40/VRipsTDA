#' Find triangles between three points
#'
#' @description [trianglecalculate()] is a function to calculate triangles between three points.
#' This is a helper function for [aniVRips()] and [staticVRips()]
#' 
#' @param df A dataframe with two columns
#' @param r A radial distance to calculate if triangles are formed.
#'
#' @return A dataframe with three columns x, y, and group where group is the indicator
#' of what triangle the points are apart of.
#' @export
#'
triangle_calculate <- function(df, r){
  
  triangles <- data.frame()
  
  colnames(df) <- c('x', 'y')
  
  group_counter <- 1
  groups <- data.frame(x = numeric(0), y = numeric(0), group = numeric(0))
  
  for (i in 1:(nrow(df))) {
    for (j in (i):(nrow(df))) {
      for (k in (j):nrow(df)) {
        if (i != j && j != k && k != i){
          d1 <- distance(df$x[i], df$y[i], df$x[j], df$y[j])
          d2 <- distance(df$x[i], df$y[i], df$x[k], df$y[k])
          d3 <- distance(df$x[j], df$y[j], df$x[k], df$y[k])
          
          if (d1 <= 2*r && d2 <= 2*r && d3 <= 2*r) {
            group <- data.frame(x = c(df$x[i], df$x[j], df$x[k]),
                                y = c(df$y[i], df$y[j], df$y[k]),
                                group = group_counter)
            groups <- rbind(groups, group)
            group_counter <- group_counter + 1
          }
        }
      }
    }
    
  }
  return(groups)
}