Lmer_Model_Plot <- function(df, outcomeMeasure,   model){
  df$PrePost <- as.factor(df$PrePost)
  means <- estimate_means(model)
  test_means <- emmeans(model, list(pairwise ~ Group * PrePost  ))
  
  emm <- emmeans(model, list(pairwise ~ Group * PrePost  ))
  ci_s <- confint((emmeans(model, list(pairwise ~ Group * PrePost  ))), level = 0.95)
  options(scipen = 100)
  df <- data.frame(group=c("1", "2"), Estimate=c(round(ci_s$`pairwise differences of Group, PrePost`[2,2], 2), 
                                                           round(ci_s$`pairwise differences of Group, PrePost`[5,2],2)),  
                   CI_Lower=c(round(ci_s$`pairwise differences of Group, PrePost`[2,5],3), 
                              round(ci_s$`pairwise differences of Group, PrePost`[5,5],3)), 
                   CI_Upper=c(round(ci_s$`pairwise differences of Group, PrePost`[2,6],3), 
                              round(ci_s$`pairwise differences of Group, PrePost`[5,6],3)), 
                   p_value=c(round(summary(emm)$`pairwise differences of Group, PrePost`[2,6],3), round(summary(emm)$`pairwise differences of Group, PrePost`[5,6],3)))
  
  mean_Diffs <- df %>%
    kbl(caption = "Mean Differences Pre and Post Intervention") %>%
    kable_classic(full_width = F, html_font = "Cambria")
  return(list(test_means, mean_Diffs))
}
