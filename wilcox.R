Apply.wilcox <-function(df, Error, Pre_Post){
  stat.test <- df  %>%
    wilcox_test(Error ~ Pre_Post, paired = TRUE) %>%
    add_significance()
  pp <- stat.test$p
  Zstat<-qnorm(stat.test$p/2)
  return(list(pp, Zstat))
}

Apply.es <-function(df, Error, Pre_Post){
  eff.size <- df  %>%
    wilcox_effsize(Error ~ Pre_Post, paired = TRUE)
  pe <- eff.size$effsize
  return(pe)
}

Apply.WGraph <- function(df, Error, Pre_Post, title ){
  stat.test <- df  %>%
    wilcox_test(Error ~ Pre_Post, paired = TRUE) %>%
    add_significance()
  stat.test <- stat.test %>% add_xy_position(x = "Pre_Post")
  tp <- ggpaired(df, x = "Pre_Post", y = "Error", 
                 #order = c("Tot_Diff.mean.PRE", "Tot_Diff.mean.POST"),
                 color = "Pre_Post", palette = c("#eb5234", "#80dde0"),
                 line.color = "dark grey", linetype = "dashed",
                 ylab = "M. Error", xlab = "Pre_Post", title = title) + 
    stat_pvalue_manual(stat.test, tip.length = 0) +
    theme(axis.text.x = element_text(angle = 10, vjust = 1, hjust = 1)) +
    labs(subtitle = get_test_label(stat.test, detailed= TRUE))
  tp
}

ApplyBar_testsCorrect <-  function(df, Error, Pre_Post, title){
  
  bar <- ggplot(df, aes(Error, fill = Pre_Post)) + geom_histogram(alpha = 0.7, aes(y = ..count..), 
        position = 'dodge', bins = 28) + ggtitle(title) + theme(legend.position = c(0.2, 0.8))+ 
    scale_colour_manual(values = c("#D55E00", "#56B4E9"))
  bar  + scale_fill_manual(values=c( "#D55E00", "#56B4E9"))
}

Apply.wilcox.unpaired <-function(df, Error, PatternType){
  stat.test <- df  %>%
    wilcox_test(Error ~ PatternType, exact = FALSE) %>%
    add_significance()
  pp <- stat.test$p
  Zstat<-qnorm(stat.test$p/2)
  return(list(pp, Zstat))
}

Apply.es.unpaired  <-function(df, Error, PatternType){
  eff.size<- df  %>%
    wilcox_effsize(Error ~ PatternType, exact = FALSE)
  pe <- eff.size$effsize
  return(pe)
}

Apply.wilcox.unpaired.Str <-function(df, Saccade_Latency, Test_Corr){
  stat.test <- df  %>%
    wilcox_test(Saccade_Latency ~ Test_Corr, exact = FALSE) %>%
    add_significance()
  pp <- stat.test$p
  Zstat<-qnorm(stat.test$p/2)
  return(list(pp, Zstat))
}

Apply.es.paired.Str  <-function(df, Saccade_Latency, Test_Corr){
  eff.size<- df  %>%
    wilcox_effsize(Saccade_Latency ~ Test_Corr, paired=T)
  pe <- eff.size$effsize
  return(pe)
}

Apply.wilcox.paired.Str <-function(df, Saccade_Latency, Test_Corr){
  stat.test <- df  %>%
    wilcox_test(Saccade_Latency ~ Test_Corr, paired=T) %>%
    add_significance()
  pp <- stat.test$p
  Zstat<-qnorm(stat.test$p/2)
  return(list(pp, Zstat))
}

Apply.es.unpaired.Str  <-function(df, Saccade_Latency, Test_Corr){
  eff.size<- df  %>%
    wilcox_effsize(Saccade_Latency ~ Test_Corr, exact = FALSE)
  pe <- eff.size$effsize
  return(pe)
}


Apply.wilcox.unpaired.sac <-function(df, Error, L_R){
  stat.test <- df  %>%
    wilcox_test(Error ~ L_R, exact = FALSE, p.adjust.method = "bonferroni") %>%
    add_significance()
  pp <- stat.test$p
  Zstat<-qnorm(stat.test$p/2)
  return(list(pp, Zstat))
}

Apply.es.unpaired.sac  <-function(df, Error, L_R){
  eff.size<- df  %>%
    wilcox_effsize(Error ~ L_R, exact = FALSE)
  pe <- eff.size$effsize
  return(pe)
}

Apply.WGraph.sac <- function(df, Error, L_R, title ){
  stat.test <- df  %>%
    wilcox_test(Error ~ L_R, paired = TRUE) %>%
    add_significance()
  stat.test <- stat.test %>% add_xy_position(x = "L_R")
  tp <- ggpaired(df, x = "L_R", y = "Error", 
                 #order = c("Tot_Diff.mean.PRE", "Tot_Diff.mean.POST"),
                 color = "L_R", palette = c("#eb5234", "#80dde0"),
                 line.color = "dark grey", linetype = "dashed",
                 ylab = "M. Error", xlab = "L_R", title = title) + 
    stat_pvalue_manual(stat.test, tip.length = 0) +
    theme(axis.text.x = element_text(angle = 10, vjust = 1, hjust = 1)) +
    labs(subtitle = get_test_label(stat.test, detailed= TRUE))
  tp
}


