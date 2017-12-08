library(dplyr)
#' Replace the worst-performing offsprings with their best-performing parents.
#' The function will automatically compare the G worst-performing offsprings with the G best-performing parents. Suppose among them, k parents outperform k offsprings; the parents will then replace the k offsprings.
#' 
#' @param old_gen Output of ranked_models() of the previous generation. A data frame.
#' @param new_gen Output of ranked_models() of the current generation. A data frame.
#' @param G Number of worst-performing offsprings the user wishes to replace. The default is the obeservation number of new_gen.
#' @return The updated new_gen.
#' @examples
#' generation_gap(old_df, new_df, 10)

generation_gap <- function(old_gen, new_gen, G=nrow(new_gen)) {
  new.AIC <- na.omit(new_gen$AIC[nrow(new_gen)-G+1:nrow(new_gen)])
  old_gen <- arrange(old_gen, desc(AIC))
  old.AIC <- na.omit(old_gen$AIC[nrow(old_gen)-G+1:nrow(old_gen)])
  G.adjusted <- G - which(new.AIC >= old.AIC)[1] + 1
  new_gen[(nrow(new_gen)-G.adjusted+1):nrow(new_gen),] <- old_gen[(nrow(old_gen)-G.adjusted+1):nrow(old_gen),]
  new_gen <- arrange(new_gen,AIC)
  return(new_gen)
}
