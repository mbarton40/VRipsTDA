#' Create animated Vietoris-Rips complexes
#' 
#' @description [aniVRips()] takes the input of a dataframe with two columns, 
#' calculates the persistence homology, and creates an HTML animation showing 
#' the formation of the complex over tau.
#' 
#' @param df A data frame with two columns to create the Vietoris-Rips from.
#' @param framerate The number of frames you want the animation to use.
#' @param mintau The smallest tau value plotted.
#' @param maxtau The largest tau value plotted.
#' @param inc_barcode If [TRUE], the animation includes the persistence barcode plot.
#' @param filename The file name of the .html file.
#' @param title The title of the .html file.
#' @param description The description of the animation.
#'
#' @return An HTML file and its corresponding plots.
#' 
#' @import animation
#' @import dplyr
#' 
#' @export
#'
aniVRips <- function(df, framerate, mintau, maxtau, inc_barcode = FALSE, filename = "VRComplexAni",
                     title = "Animation of V-R Complex over Tau", 
                     description = 
                       "This animation represents the creation of the V-R complex of some set of data. Very cool!"){
  
  tau_seq <- seq(from = mintau, to = maxtau, length.out = framerate)
  
  saveHTML({
    for (tau in tau_seq){
      staticVRips(df, tau, inc_barcode = inc_barcode)

      ani.pause()
    }
  },
  img.name = paste(filename, "_plot", sep = ""), imgdir = paste(filename, "_dir", sep = ""), htmlfile = paste(filename, ".html", sep = ""), 
  autobrowse = FALSE, title = title, 
  description = description)
}