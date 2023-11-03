triangle_calculate <- function(df, r){
  
  triangles <- data.frame()
  
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