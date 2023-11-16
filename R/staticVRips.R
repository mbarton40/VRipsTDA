#' Create static Vietoris-Rips complexes
#'
#' @description [staticVRips()] takes the input of a dataframe with two columns, calculates the persistence homology, 
# and creates a graph of the complex given a value of tau. This can also show the current barcode
# and create a png of the complex.
#' 
#' @param df A data frame with two columns to create the Vietoris-Rips from.
#' @param tau A value of to compute the Vietoris-Rips for.
#' @param inc_barcode If [TRUE], the image includes the persistence barcode plot.
#' @param png If [TRUE], the png of the output is saved in the current directory.
#' @param filename The name of the png file.
#'
#' @return A plot of the Vietoris-Rips complex with given value tau and the barcode plot.
#' 
#' @import dplyr
#' @import ggforce
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom R.devices suppressGraphics
#' @import TDA
#' @import TDAstats
#' 
#' @export
#'
staticVRips <- function(df, tau, inc_barcode = FALSE,
                        png = FALSE, filename = "VRComplexStatic_plot"){
  
  birth=death=xstart=ystart=xend=yend=x=y=group=NULL
  
  ph <- calculate_homology(df)
  
  ph_df <- as.data.frame(ph)
  ph_barcode_df <- ph_df %>%
    filter(birth <= tau) %>%
    mutate(death = ifelse(death > tau, tau, death))
  ph_barcode_matrix <- as.matrix(ph_barcode_df)
  
  barcode_plot <- plot_barcode(ph_barcode_matrix)
  
  taus_needed <- data.frame(taus_needed_nondf = sort(union(ph_df$death, ph_df$birth)))
  
  # Calculate intersections and initiate triangles df
  
  intersections <- intersection(df, tau)
  triangles <- data.frame()
  
  # Initial plot if there are no segments. Followed by just the segments
  VRplot <- ggplot(data = df, aes(.data[[colnames(df)[1]]], .data[[colnames(df)[2]]])) +
    geom_point() +
    geom_circle(aes(x0 = .data[[colnames(df)[1]]], y0 = .data[[colnames(df)[2]]], r = tau), fill = NA, color = "blue") +
    theme_classic() +
    annotate("text", label = paste("Tau =", tau), x = Inf, y = -Inf, vjust = -1, hjust = 1)
  
  if(nrow(intersections) != 0){
    colnames(intersections) <- c('xstart', 'ystart', 'xend', 'yend')
    VRplot <- VRplot +
      geom_segment(data = intersections, aes(x = xstart, y = ystart, xend = xend, yend = yend), color = "red")
    
    # Make df for triangle calculation
    triangle_test_points <- tibble(x = c(intersections$xstart, intersections$xend), 
                                   y = c(intersections$ystart, intersections$yend)) %>%
      unique()
    
    # Calculate triangles
    triangles <- triangle_calculate(triangle_test_points, tau)
    
  }
  
  if(nrow(triangles) != 0){
    VRplot <- VRplot +
      geom_polygon(data = triangles, aes(x = x, y = y, group = group), color = "red", fill = "lightgreen", alpha = 0.4)
  }
  
  if(png){
    
    if(inc_barcode){
      suppressGraphics(suppressMessages(ggsave(paste(filename, ".png", sep = ""),grid.arrange(VRplot,barcode_plot, ncol = 2))))
    } else{
      suppressGraphics(suppressMessages(ggsave(paste(filename, ".png", sep = ""),VRplot)))
    }
  }
  
  if(inc_barcode){
    grid.arrange(VRplot,barcode_plot, ncol = 2)
  } else{
    return(VRplot)
  }
  
}