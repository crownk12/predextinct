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


predextinct_plot <-function(s, folder = "data/"){

  opar <- par()

  par(mfrow = c(1, 2))

  RangeShiftR::plotAbundance(s, folder, main = "Abundance")
  RangeShiftR::plotOccupancy(s, folder, main = "Occupancy")

  par(opar)

}
