#' x`predextinct_plot() Function
#'
#' This function plot the Abundance and Occupancy of species.
#' This function run RangeshiftR output file from RangeShiftR::plotAbundance and RangeShiftR::plotOccupancy.
#' @param s : The output of setscenario()
#' @param folder: [complete].
#' @keywords predextinct_plot
#' @import RangeShiftR
#' @export
#' @examples
#' predextinct_plot()






predextinct_plot <- function(s, folder = "data/"){

  for(i in 1:length(s)){

    name <- strsplit(s[[i]]@land@LandscapeFile, split = "[_.]")[[1]][2]

    par(mfrow = c(1, 2), mar = c(4, 4, 4, 1), oma = c(0.5, 0.5, 2, 0.5))

    plotAbundance(s[[i]], dirpath = "data/", main = "Abundance")

    plotOccupancy(s[[i]], dirpath = "data/", main = "Occupancy")

    mtext(name, outer = T, cex = 2)

  }

}
