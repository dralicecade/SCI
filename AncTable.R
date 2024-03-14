ancova_Table <- function(df, changeVal, preVal, Group){
  model <- Anova(lm(changeVal ~ Group + preVal, data = df, type="II"))
  
  qqPlot(lm(changeVal ~ Group + preVal, data = df, type="II")) 
  
  modellm <- lm(changeVal ~ Group + preVal, data = df, type="II")
  
  emm <- emmeans(modellm, list(pairwise ~ Group   ))
  ci_s <- confint((emmeans(modellm, list(pairwise ~ Group  ))), level = 0.95)
  
  df <- data.frame(group=c("value"), change=c(paste( "F(1, 1) = ", round( model$`F value`[1],2), ", ",round(model$`Pr(>F)`[1],3) )),
                   meanDiff=c(paste(round(ci_s$`pairwise differences of Group`[1,2],2), " (",round(ci_s$`pairwise differences of Group`[1,5],3),", ", round(ci_s$`pairwise differences of Group`[1,6],3),"), ",
                                    round(summary(emm)$`pairwise differences of Group`[1,6],3))),
                   Group1_EMMEAN=c(paste(round(ci_s$`emmeans of Group`[1,2],2), " (", round(ci_s$`emmeans of Group`[1,5],3),", ",round(ci_s$`emmeans of Group`[1,6],3), ")" )),
                   Group2_EMMEAN=c(paste(round(ci_s$`emmeans of Group`[2,2],2), " (",round(ci_s$`emmeans of Group`[2,5],3),", ",round(ci_s$`emmeans of Group`[2,6],3),")" ))
                   
  )
  
  tableAnc <- df %>%
    kbl(caption = "Mean Differences Pre and Post Intervention") %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  return(list(tableAnc))
  
}