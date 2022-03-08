#' demo() Function
#'
#' This function is a sub-function of setscenario().
#' @param Stages : Number of life-Stages (integer). Requires a minimum of 2, for "juvenile" (stage 0) and "adult". Maximum is 3.
#' @param MaxAge : Maximum age in years
#' @param ReproductionType : 0 = asexual / only female model (default), 1 = simple sexual model, 2 = sexual model with explicit mating system
#' @param prob_reproduction : Probability to produce offsprings
#' @param num_offsprings : Expected the number of offsprings
#' @param prob_surv : Probability to survive
#' @keywords demo
#' @export
#' @examples
#' demo()


# Demography

demo <- function(Stages, MaxAge, ReproductionType, prob_reproduction, num_offsprings, prob_surv){

  require(RangeShiftR) # RangeShiftR should be installed.

  # Sub-function
  stg <- function(Stages, MaxAge, ReproductionType, prob_reproduction, num_offsprings, prob_surv){

    trans_mat <- function(Stages, ReproductionType = 0, prob_reproduction, num_offsprings, prob_surv){
      if(ReproductionType %in% c(0, 1)){
        matrix(c(0, prob_reproduction, num_offsprings, prob_surv), nrow = Stages, ncol = Stages, byrow = F)
      }
      else if(ReproductionType == 2){
        print("Still evolving")
      }
      else{
        print("You input a wrong reproduction parameter.")
      }
    }

    StageStructure(Stages = Stages,
                   TransMatrix = trans_mat(Stages, ReproductionType = 0, prob_reproduction, num_offsprings, prob_surv),
                   MaxAge = MaxAge,
                   SurvDensDep = T)

  }

  Demography(StageStruct = stg(Stages, MaxAge, ReproductionType, prob_reproduction, num_offsprings, prob_surv),
             ReproductionType = ReproductionType)

}
