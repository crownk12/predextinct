#' demo() Function
#'
#' This function 
#' @param a ,b : two numbers to be operated
#' @keywords Add
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
