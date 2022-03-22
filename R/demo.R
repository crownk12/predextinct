#' demo() Function
#'
#' This function is a sub-function of setscenario().
#' @param Stages : Number of life-Stages (integer). Requires a minimum of 2, for "juvenile" (stage 0) and "adult". Maximum is 3.
#' @param MaxAge : Maximum age in years
#' @param prob_reproduction : Probability to produce offsprings
#' @param num_offsprings : Expected the number of offsprings
#' @param prob_surv : Probability to survive
#' @keywords demo
#' @export
#' @examples
#' demo()


# Demography

demo <- function(Stages, MaxAge, prob_reproduction, num_offsprings, prob_surv){

  require(RangeShiftR) # RangeShiftR should be installed.

  # Sub-function
  stg <- function(Stages, MaxAge, prob_reproduction, num_offsprings, prob_surv){

    trans_mat <- function(Stages, prob_reproduction, num_offsprings, prob_surv){

      matrix(c(0, prob_reproduction, num_offsprings, prob_surv), nrow = Stages, ncol = Stages, byrow = F)

    }

    StageStructure(Stages = Stages,
                   TransMatrix = trans_mat(Stages,
                                           prob_reproduction,
                                           num_offsprings,
                                           prob_surv),
                   MaxAge = MaxAge,
                   SurvDensDep = T)

  }

  Demography(StageStruct = stg(Stages,
                               MaxAge,
                               prob_reproduction,
                               num_offsprings,
                               prob_surv),
             ReproductionType = 0) # Some species are the limiting sex regarding reproduction and dispersal, and we hence use an asexual model.

}
