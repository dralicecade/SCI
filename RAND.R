require("tidyverse")
require("here")
require("readr")
library(plyr)
require("dplyr")
require("blandr")
require("ggplot2")
library(data.table)
library(rstatix)
library(ggpubr)
library("readxl")
require("kableExtra")
# import plotrix package
library("plotrix")



RAND <- read_excel("SF_36.xlsx")
#view(RAND)

RAND$Group[RAND$Group == "1"] <- "Chiro"
RAND$Group[RAND$Group == "2"] <- "Control"

RAND$PrePost <- as.factor(RAND$PrePost)
RAND$Group <- as.factor(RAND$Group)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname, "se" = paste0(varname, "_se")))
  
  return(data_sum)
}


phys <- data_summary(RAND, varname="Physical", 
                    groupnames=c("Group", "PrePost"))


PFG <- ggplot(data=phys, aes(x=PrePost, y=Physical, group=Group))+
  geom_line( aes(linetype=Group ), color="black", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=Physical-Physical_se, ymax=Physical+Physical_se), color="black", width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="black", size = 5) + coord_cartesian(ylim = c(0,30))  +
  labs(title="Physical functioning",x="", y = "Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom") + 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))
  
ggsave("PFG.png", width = 15, height = 10, units = "cm")


R_phys <- data_summary(RAND, varname="Role_Physical", 
                     groupnames=c("Group", "PrePost"))

df2 <- merge(x=phys, y=R_phys, by = c("Group", "PrePost"))

RPHG <- ggplot(data=R_phys, aes(x=PrePost, y=Role_Physical, group=Group))+
  geom_line( aes(linetype=Group ), color="blue", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=Role_Physical-Role_Physical_se, ymax=Role_Physical+Role_Physical_se), color="blue", width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="blue", size = 5)+ #coord_cartesian(ylim = c(20,100))  +
  labs(title="Physical role limitations",x="", y = "Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom")+ 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))

ggsave("RPHG.png", width = 15, height = 10, units = "cm")


R_emot <- data_summary(RAND, varname="Role_Emotional", 
                       groupnames=c("Group", "PrePost"))

df2 <- merge(x= df2, y= R_emot, by = c("Group", "PrePost"))

REP <- ggplot(data=R_emot, aes(x=PrePost, y=Role_Emotional, group=Group))+
  geom_line( aes(linetype=Group ), color="#00CC00", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=Role_Emotional-Role_Emotional_se, ymax=Role_Emotional+Role_Emotional_se), color="#00CC00", width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="#00CC00",size = 5)+ #coord_cartesian(ylim = c(20,100))  +
  labs(title="Emotional role limitations",x="", y = "Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom")+ 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))
ggsave("REP.png", width = 15, height = 10, units = "cm")

energy <- data_summary(RAND, varname="Energy", 
                       groupnames=c("Group", "PrePost"))

df2 <- merge(x= df2, y= energy, by = c("Group", "PrePost"))

EFG <- ggplot(data=energy, aes(x=PrePost, y=Energy, group=Group))+
  geom_line( aes(linetype=Group ), color="dark green", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=Energy-Energy_se, ymax=Energy+Energy_se), color="dark green",  width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="dark green", size = 5)+#coord_cartesian(ylim = c(20,100))  +
  labs(title="Energy & fatigue",x="", y="Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom")+ 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))
ggsave("EFG.png", width = 15, height = 10, units = "cm")


emot <- data_summary(RAND, varname="Emotional", 
                       groupnames=c("Group", "PrePost"))

df2 <- merge(x= df2, y= emot, by = c("Group", "PrePost"))

EMG <- ggplot(data=emot, aes(x=PrePost, y=Emotional, group=Group))+
  geom_line( aes(linetype=Group ), color="dark orange", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=Emotional-Emotional_se, ymax=Emotional+Emotional_se), color="dark orange", width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="dark orange", size = 5)+#coord_cartesian(ylim = c(20,100))  +
  labs(title="Emotional wellbeing",x="", y="Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom")+ 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))
ggsave("EMG.png", width = 15, height = 10, units = "cm")


SF <- data_summary(RAND, varname="Social", 
                     groupnames=c("Group", "PrePost"))

df2 <- merge(x= df2, y= SF, by = c("Group", "PrePost"))


SFG <- ggplot(data=SF, aes(x=PrePost, y=Social, group=Group))+
  geom_line( aes(linetype=Group ), color="maroon2", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=Social-Social_se, ymax=Social+Social_se), color="maroon2",  width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="maroon2", size = 5)+#coord_cartesian(ylim = c(20,100))  +
  labs(title="Social functioning",x="", y="Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom")+ 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))

ggsave("SFG.png", width = 15, height = 10, units = "cm")


P <- data_summary(RAND, varname="Pain", 
                   groupnames=c("Group", "PrePost"))

df2 <- merge(x= df2, y= P, by = c("Group", "PrePost"))


PG <- ggplot(data=P, aes(x=PrePost, y=Pain, group=Group))+
  geom_line( aes(linetype=Group ), color="darkorchid4", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=Pain-Pain_se, ymax=Pain+Pain_se), color="darkorchid4",  width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="darkorchid4", size = 5)+#coord_cartesian(ylim = c(20,100))  +
  labs(title="Pain",x="", y="Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom")+ 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))

ggsave("PG.png", width = 15, height = 10, units = "cm")


GH <- data_summary(RAND, varname="General_Health", 
                  groupnames=c("Group", "PrePost"))

Full_RAND <- merge(x= df2, y= GH, by = c("Group", "PrePost"))
library("writexl")
write_xlsx(Full_RAND,"C:\\Users\\Alice Cade\\Documents\\R_Analysis\\SCI\\Full_RAND.xlsx")
write_xlsx(Full_RAND,"C:\\Users\\alice.cade\\Downloads\\Full_RAND.xlsx")

GHG <- ggplot(data=GH, aes(x=PrePost, y=General_Health, group=Group))+
  geom_line( aes(linetype=Group ), color="royalblue4", linewidth=2, show.legend = F) +
  geom_errorbar(aes(ymin=General_Health-General_Health_se, ymax=General_Health+General_Health_se), color="royalblue4", width=.1, alpha=.5)+
  geom_point(aes(shape=Group), color="royalblue4", size = 3)+#coord_cartesian(ylim = c(20,100))  +
  labs(title="General health",x="", y="Percentage") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14),
        legend.title = element_blank(), legend.position = "bottom")+ 
  scale_x_discrete(limits=c("1","2"), labels=c("Pre", "Post"))

ggsave("GHG.png", width = 15, height = 10, units = "cm")



allPlots <- ggarrange(PFG, RPHG, REP, EFG, EMG, SFG, PG, GHG, ncol = 2, nrow= 4, common.legend = T, legend = "bottom")
ggsave("allPlots.png", width = 30, height = 45, units = "cm")




library(ggpubr)
#ggqqplot(RAND$Y1_PF)

Data <- data.frame(RAND$Y1_PF,RAND$Y2_PF,RAND$Y3_PF,RAND$Y4_PF)
#physical
a <- 92.4
n <- 87.5
Data <- data.frame(RAND$Y1_RPH,RAND$Y2_RPH,RAND$Y3_RPH,RAND$Y4_RPH)
# role phys
a <- 87.1
n <- 86.8
Data <- data.frame(RAND$Y1_REP,RAND$Y2_REP,RAND$Y3_REP,RAND$Y4_REP)
# role emot
a <- 82.9
n <- 93.6 
Data <- data.frame(RAND$Y1_EF,RAND$Y2_EF,RAND$Y3_EF,RAND$Y4_EF)
# energy
a <- 64.9
n <- 64.0
Data <- data.frame(RAND$Y1_EW,RAND$Y2_EW,RAND$Y3_EW,RAND$Y4_EW)
#emotional wellbeing
a <- 75.9
n <- 81.9
Data <- data.frame(RAND$Y1_SF,RAND$Y2_SF,RAND$Y3_SF,RAND$Y4_SF)
#social functioning
a <- 86.3
n <- 88.5
Data <- data.frame(RAND$Y1_P,RAND$Y2_P,RAND$Y3_P,RAND$Y4_P)
#pain
a <- 77
n <- 75.8
Data <- data.frame(RAND$Y1_GH,RAND$Y2_GH,RAND$Y3_GH,RAND$Y4_GH)
# general health
a <- 79
n <- 74.7

combos <- combn(ncol(Data),4)
adply(combos, 1, function(x) {
  test <- shapiro.test(Data[, x[1]])
  
  out <- data.frame("var1" = colnames(Data)[x[1]]
                    , "p.value" = sprintf("%.3f", test$p.value)
  )
  return(out)
})

#Diff bw Normative or age matched Groups
adply(combos, 1, function(x) {
  test <- wilcox.test(Data[, x[1]], mu = a, paired = F)
  
  out <- data.frame("var1" = colnames(Data)[x[1]]
                    , "var2" = sprintf("%.1f", a)
                    , "z" = sprintf("%.0f", test$statistic)
                    ,  "p.value" = sprintf("%.3f", test$p.value)
                
  )
  return(out)
  
})
#t.test(RAND$Y4_GH, mu = m)



adply(combos, 1, function(x) {
  test <- wilcox.test(Data[, x[1]], mu = m, paired = F)
  
  out <- data.frame("var1" = colnames(Data)[x[1]]
                    , "var2" = sprintf("%.0f", m)
                    , "z" = sprintf("%.0f", test$statistic)
                    ,  "p.value" = sprintf("%.3f", test$p.value)
                    
  )
  return(out)
  
})




#Difference between pre and post COVID Groups
PrePost_Wilcox <- data.frame()
Student_Group <- "Group 4"
  stat.test <- RAND %>% filter(Group == Student_Group)  %>%
    wilcox_test(General_Health ~ PrePost, paired = F) %>%
    add_significance()
  Group <- data.frame("Group" = Student_Group,
                     "domain" = "General_Health",
    "z stat" = qnorm(stat.test$p/2),
             "p value" = stat.test$p)
  PrePost_Wilcox <- rbind(PrePost_Wilcox, Group)
  library("writexl")
  #write_xlsx(PrePost_Wilcox,"C:\\Users\\Alice\\Documents\\R\\RAND\\PrePost_Wilcox.xlsx")
  write_xlsx(PrePost_Wilcox,"C:\\Users\\alice.cade\\Downloads\\Rand_Sheets\\PrePost_Wilcox.xlsx")

  
RAND$Role_Emotional


#Difference BW Group groups for both pre and post COVID times
Time_Wilcox <- data.frame()

  #PrePost <- "Pre"
  stat.test <- RAND %>% filter(PrePost == "Post")  %>%
    wilcox_test(Role_Emotional ~ Group, paired = F) %>%
    add_significance()
  Time <- data.frame("COVID" = "Post",
                     "group1" = stat.test$group1,
                     "group2" = stat.test$group2,
                     "domain" = "Role_Emotional",
                     "z stat" = qnorm(stat.test$p/2),
                     "p value" = stat.test$p)
  Time_Wilcox <- rbind(Time_Wilcox, Time)
  
  library("writexl")
  #write_xlsx(Group_Wilcox,"C:\\Users\\Alice\\Documents\\R\\RAND\\PrePost_Wilcox.xlsx")
  write_xlsx(Time_Wilcox,"C:\\Users\\alice.cade\\Downloads\\Rand_Sheets\\Time_Wilcox.xlsx")
  
  
  RAND$General_Health
  #Difference between pre and post COVID Groups to averages
  PrePost_NZ_Ave <- data.frame()
  #PrePost_AGE_Ave <- data.frame()
  # Physical
  a <- 92.4
  n <- 87.5
  # Role_Physical
  a <- 87.1
  n <- 86.8
  # Role_Emotional
  a <- 82.9
  n <- 93.6 
  # Energy
  a <- 64.9
  n <- 64.0
  # Emotional
  a <- 75.9
  n <- 81.9
  # Social
  a <- 86.3
  n <- 88.5
  # Pain
  a <- 77
  n <- 75.8
  # General_Health
  a <- 79
  n <- 74.7 
  
  df <- RAND %>% filter(Group == Student_Group)  %>% filter(PrePost == "Post")
  
  Student_Group <- "Group 4"
  stat.test <- RAND %>% filter(Group == Student_Group)  %>% filter(PrePost == "Post")  %>%
    wilcox_test(Role_Physical~1, mu = a, paired = F) %>%
    add_significance()
  Group <- data.frame("Group" = Student_Group,
                     "Time" = "Post",
                     "domain" = "Physical",
                     "z stat" = qnorm(stat.test$p/2),
                     "p value" = stat.test$p)
  PrePost_NZ_Ave <- rbind(PrePost_NZ_Ave, Group)
  library("writexl")
  #write_xlsx(PrePost_Wilcox,"C:\\Users\\Alice\\Documents\\R\\RAND\\PrePost_Wilcox.xlsx")
  write_xlsx(PrePost_NZ_Ave,"C:\\Users\\alice.cade\\Downloads\\Rand_Sheets\\PrePost_NZ_Ave.xlsx")

  