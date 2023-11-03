aniVRips <- function(df, framerate, extra_thresh = 0.25, inc_barcode = FALSE, filename = "VRComplexAni",
                     title = "Animation of V-R Complex over Tau", 
                     description = "This animation represents the creation of the V-R complex of some set \n
                                      of data. Very cool!"){
  ph <- calculate_homology(df)
  
  ph_df <- as.data.frame(ph)
  
  taus_needed <- data.frame(taus_needed_nondf = sort(union(ph_df$death, ph_df$birth)))
  
  tau_seq <- seq(from = min(taus_needed), to = max(taus_needed) + extra_thresh, length.out = framerate)
  
  saveHTML({
    for (tau in tau_seq){
      
      # Making the barcode for the given tau
      ph_barcode_df <- ph_df %>%
        filter(birth <= tau) %>%
        mutate(death = ifelse(death > tau, tau, death))
      ph_barcode_matrix <- as.matrix(ph_barcode_df)
      barcode_plot <- plot_barcode(ph_barcode_matrix)
      
      # Calculate intersections and initiate triangles df
      
      intersections <- intersection(df, tau)
      triangles <- data.frame()
      
      # Initial plot if there are no segments. Followed by just the segments
      VRplot <- ggplot(data = df, aes(.data[[colnames(df)[1]]], .data[[colnames(df)[2]]]))
        geom_point() +
        geom_circle(aes(x0 = .data[[colnames(df)[1]]], y0 = .data[[colnames(df)[2]]], r = tau), fill = NA, color = "blue") +
        xlim(-max(taus_needed) - 10*extra_thresh, max(taus_needed) + 10*extra_thresh) +
        ylim(-max(taus_needed) - 10*extra_thresh, max(taus_needed) + 10*extra_thresh) +
        coord_quickmap() +
        theme_classic() +
        annotate("text", label = paste("Tau =", tau), 
                 x = max(taus_needed) + 10*extra_thresh, y = -max(taus_needed) - 10*extra_thresh, vjust = 0, hjust = 1)
      
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
      if(inc_barcode){
        grid.arrange(VRplot,barcode_plot, ncol = 2)
      } else{
        print(VRplot)
      }
      ani.pause()
    }
  },
  img.name = paste(filename, "_plot", sep = ""), imgdir = paste(filename, "_dir", sep = ""), htmlfile = paste(filename, ".html", sep = ""), 
  autobrowse = FALSE, title = title, 
  description = description)
}