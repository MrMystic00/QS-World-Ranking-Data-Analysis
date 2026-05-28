library(dplyr)
library(countrycode)

## Data Cleaning
rankings = read.csv("2024 QS World University Rankings.csv", header = TRUE)
colnames(rankings)<-c("y2024_rank","y2023_rank","institution","country_code","country",
                "institution_size", "focus", "research_output", "age_band", "status", 
                "ar_score", "ar_rank",
                "er_score", "er_rank",
                "fs_score", "fs_rank",
                "cpf_score", "cpf_rank",
                "ifr_score", "ifr_rank",
                "isr_score", "isr_rank",
                "irn_score", "irn_rank",
                "ger_score", "ger_rank",
                "sus_score", "sus_rank",
                "overall_score") #setting up the correct column names
rankings = rankings[-1,] #remove the 1st row from double column naming
head(rankings)
str(rankings)

rankings <- rankings[rowSums(rankings != "") == ncol(rankings), ]

# Adding raw overall score column
rankings = rankings %>% mutate(Overall_Score_Raw = 0.3*as.numeric(rankings$ar_score) + 0.15*as.numeric(rankings$er_score) + 0.1*as.numeric(rankings$fs_score) + 0.2*as.numeric(rankings$cpf_score) + 0.05*as.numeric(rankings$ifr_score) + 0.05*as.numeric(rankings$isr_score) + 0.05*as.numeric(rankings$irn_score) + 0.05*as.numeric(rankings$ger_score) + 0.05*as.numeric(rankings$sus_score))

# Removing all the blank entries from every column
for (colname in colnames(rankings)) {
  if (grepl('_score', colname)){
    rankings[[colname]] <- as.numeric(rankings[[colname]])}
}

# Converting country code to continent
rankings$country <- recode(rankings$country,
                           "USA" = "United States",
                           "UK" = "United Kingdom")
rankings$continents <- countrycode(
  rankings$country,
  origin = "country.name",
  destination = "continent"
)

# Removing unused variables
rankings <- rankings %>% select(-(y2024_rank:country)) %>% select(-ends_with("_rank")) %>% select(-overall_score)


## Summary Statistics for Categorical variables
# institution_size
rankings$institution_size<- factor(rankings$institution_size, levels = c("S","M","L","XL"))
institutional_size_freq = table(rankings$institution_size)
barplot(institutional_size_freq, main = "Barplot of Number of Universities by Institution Size", xlab = "institution_size", ylab = "count")

# focus
rankings$focus<- factor(rankings$focus)
barplot(table(rankings$focus), main = "Barplot of Number of Universities by Subject Area Focus", xlab = "focus", ylab = "count")

# research_output
rankings$research_output<- factor(rankings$research_output, levels = c("LO","MD","HI","VH"))
barplot(table(rankings$research_output), main = "Barplot of Number of Universities by Research Output", xlab = "research_output", ylab = "count")

# age_band
rankings$age_band<- factor(rankings$age_band)
barplot(table(rankings$age_band) , main = "Barplot of Number of Universities by Age Band", xlab = "age_band", ylab = "count")

# status
barplot(table(rankings$status) , main = "Barplot of Number of Universities by Status", xlab = "status", ylab = "count")

#Regrouping "status" variable 
rankings<-rankings%>%
  mutate(institution_status=factor(if_else(status=="A", "public","private")))%>%
  relocate(institution_status,.before=where(is.numeric)) %>%
  filter(focus!="SP",research_output!="LO") %>%
  select(-status) 

rankings$focus<- factor(rankings$focus, levels = c("CO","FC","FO"))
rankings$research_output<- factor(rankings$research_output, levels = c("MD","HI","VH"))
#status (regrouped)

barplot(table(rankings$institution_status) , main = "Barplot of Number of Universities by Status", xlab = "institution_status", ylab = "count")


## Numerical Variables
par(mfrow=c(1,2))
# Academic Reputation Score 
hist(rankings$ar_score, main = "Histogram for Academic Reputation Score", xlab = "ar_score", ylab = "count", breaks = 20)
boxplot(rankings$ar_score, main = "Boxplot for Academic Reputation Score", ylab = "ar_score")
ln_acad_rep = log(rankings$ar_score) #ln transformation
hist(ln_acad_rep, main = "Histogram for ln(Academic Reputation Score)", xlab = "ln_acad_rep", ylab = "count", breaks = 15)
boxplot(ln_acad_rep, main = "Boxplot for ln(Academic Reputation Score)", ylab = "ln_acad_rep")

# Employment Reputation Score
hist(rankings$er_score, main = "Histogram for Employment Reputation Score", xlab = "er_score", ylab = "count", breaks = 20)
boxplot(rankings$er_score, main = "Boxplot for Employment Reputation Score", ylab = "er_score")
ln_employ_rep = log(rankings$er_score) #ln transformation
hist(ln_employ_rep, main ="Histogram for ln(Employment Reputation Score)" , xlab = "ln_employ_rep", ylab = "count", breaks = 15)
boxplot(ln_employ_rep, main = "Boxplot for ln(Employment Reputation Score)", ylab = "ln_employ_rep")

# Faculty Student Score
hist(rankings$fs_score, main="Histogram of Faculty Student Score", xlab = "fs_score", ylab = "count", breaks = 20)
boxplot(rankings$fs_score, main="Boxplot of Faculty Student Score", ylab = "fs_score")
ln_faculty_student_score = log(rankings$fs_score) #ln transformation
hist(ln_faculty_student_score, main="Histogram of ln(Faculty Student Score)", xlab = "ln_faculty_student_score", ylab = "count", breaks = 15)
boxplot(ln_faculty_student_score, main="Boxplot of ln(Faculty Student Score)", ylab = "ln_faculty_student_score")

# Citations per Faculty Score 
hist(rankings$cpf_score, main="Histogram of Citation per Faculty Score", xlab = "cpf_score", ylab = "count", breaks = 20)
boxplot(rankings$cpf_score, main="Boxplot of Citation per Faculty Score", ylab = "cpf_score")
ln_citation_per_faculty_score = log(rankings$cpf_score)  #ln transformation
hist(ln_citation_per_faculty_score, main="Histogram of ln(Citation per Faculty Score)", xlab = "ln_citation_per_faculty score", ylab = "count", breaks = 15)
boxplot(ln_citation_per_faculty_score, main="Boxplot of ln(Citation per Faculty Score)", ylab = "ln_citation_per_faculty_score")

# International Faculty Ratio Score  
hist(rankings$ifr_score, main="Histogram of International Faculty Score", xlab = "ifr_score", ylab = "count", breaks = 20)
boxplot(rankings$ifr_score, main="Boxplot of International Faculty Score", ylab = "ifr_score")
ln_IFR_score = log(rankings$ifr_score)  #ln transformation
hist(ln_IFR_score, main="Histogram of ln(International Faculty Score)",  xlab = "ln_IFR_score", breaks = 15)
boxplot(ln_IFR_score, main="Boxplot of ln(International Faculty Score)", ylab = "ln_IFR_score")

# International Students Score
hist(rankings$isr_score, main="Histogram of International Student Score", xlab = "isr_score", breaks = 20)
boxplot(rankings$isr_score, main="Boxplot of International Student Score", ylab = "isr_score")
ln_IS_score = log(rankings$isr_score) #ln transformation
hist(ln_IS_score, main="Histogram of ln(International Student Score)",  xlab = "ln_IS_score", breaks = 15)
boxplot(ln_IS_score, main="Boxplot of ln(International Student Score)", ylab = "ln_IS_score")

# International Research Score
hist(rankings$irn_score, main="Histogram of International Research Score", xlab = "irn_score", breaks = 20)
boxplot(rankings$irn_score, main="Boxplot of International Research Score", ylab = "irn_score")
ln_IR_score = log(rankings$irn_score) #ln transformation
hist(ln_IR_score, main="Histogram of ln(International Research Score)",  xlab = "ln_IR_score", breaks = 15)
boxplot(ln_IR_score, main="Boxplot of ln(International Research Score)", ylab = "ln_IR_score")

# Employment Score
hist(rankings$ger_score, main="Histogram of Employment Outcome Score", xlab = "ger_score", breaks = 20)
boxplot(rankings$ger_score, main="Boxplot of Employment Outcome Score", ylab = "ger_score")
ln_EO_score = log(rankings$ger_score) #ln transformation
hist(ln_EO_score, main="Histogram of ln(Employment Outcome Score)",  xlab = "ln_EO_score", breaks = 15)
boxplot(ln_EO_score, main="Boxplot of ln(Employment Outcome Score)", ylab = "ln_EO_score")

# Sustainability Score
hist(rankings$sus_score, main="Histogram of Sustainability Score", xlab = "Sustainability Score", breaks = 20)                   
boxplot(rankings$sus_score, main="Boxplot of Sustainability Score", ylab = "Sustainability Score")
ln_sustainability = log(rankings$sus_score) #log transform
hist(ln_sustainability, main="Histogram of ln(Sustainability Score)", xlab = "ln_sustainability_score", breaks = 15)                  
boxplot(ln_sustainability, main="Boxplot of ln(Sustainability Score)", ylab = "ln_sustainability_score")

# Overall Raw Score
par(mfrow=c(2,2))
hist(rankings$Overall_Score_Raw, main="Histogram of Overall Raw Score", xlab = "Overall_Score_Raw", breaks = 20)                   
boxplot(rankings$Overall_Score_Raw, main="Boxplot of Overall Raw Score", xlab = "Overall_Score_Raw")
ln_overall_raw_scores = log(rankings$Overall_Score_Raw) #log transform
hist(ln_overall_raw_scores, main="Histogram of ln(Overall Raw Score)", xlab = "ln_overall_raw_scores", breaks = 15) 
boxplot(ln_overall_raw_scores, main="Boxplot of Overall Raw Score", xlab = "Overall_Score_Raw")


## Correlation between ar_score and other score components 
library(corrplot)
variables=cbind.data.frame(rankings$ar_score, rankings$er_score, rankings$fs_score, rankings$cpf_score, rankings$ifr_score, rankings$isr_score, rankings$irn_score, rankings$ger_score, rankings$sus_score)
corrplot(cor(variables), type = "upper", method="color", addCoef.col="black", number.cex=0.6)


## Statistical Tests:
# 4.2.1 Overall_Score_Raw VS age_band
par(mfrow=c(1,1))
boxplot(ln_overall_raw_scores~rankings$age_band, main = "Boxplot of ln(Overall_Score_Raw) vs age_band")

for (i in c(1:5)){
  print(shapiro.test(ln_overall_raw_scores[rankings$age_band==as.character(i)]))}
kruskal.test(ln_overall_raw_scores, rankings$age_band)
pairwise.wilcox.test(ln_overall_raw_scores,rankings$age_band)
# 4.2.2 Overall_Score_Raw VS focus
boxplot(ln_overall_raw_scores~rankings$focus,
        xlab = "focus (excluding SP)",
        ylab = "ln(Overall_Score_Raw)",
        main = "Boxplot of ln(Overall_Score_Raw) vs focus")
aggregate(ln_overall_raw_scores, list(rankings$focus), FUN=var)
rankings_focus<- aov(ln_overall_raw_scores~factor(rankings$focus))
summary(rankings_focus)
pairwise.t.test(ln_overall_raw_scores, rankings$focus, p.adjust.method = "none")
# 4.2.3 Overall_Score_Raw VS research_output 
bartlett.test(ln_overall_raw_scores~research_output, data = rankings)
oneway.test(ln_overall_raw_scores~research_output, data=rankings)
pairwise.t.test(ln_overall_raw_scores, rankings$research_output, p.adjust.method = p.adjust.methods, pool.sd = FALSE)
# 4.2.4 Overall_Score_Raw VS institution_size 
aggregate(ln_overall_raw_scores, list(rankings$age_band), FUN=var)
aggregate(ln_overall_raw_scores, list(rankings$institution_size), FUN=var)
bartlett.test(ln_overall_raw_scores~institution_size, data = rankings)
oneway.test(ln_overall_raw_scores~institution_size, data=rankings)
pairwise.t.test(ln_overall_raw_scores, rankings$institution_size, p.adjust.method = p.adjust.methods, pool.sd = FALSE)
# 4.2.5 Overall_Score_Raw VS institution_status
boxplot(ln_overall_raw_scores~rankings$institution_status,
        xlab = "institution_status",
        ylab = "ln(Overall_Score_Raw)",
        main = "Boxplot of ln(Overall_Score_Raw) vs institution_status")
var.test(ln_overall_raw_scores[rankings$institution_status=="public"],
         ln_overall_raw_scores[rankings$institution_status=="private"])
t.test(ln_overall_raw_scores[rankings$institution_status=="public"],
       ln_overall_raw_scores[rankings$institution_status=="private"],
       var.equal=T, alternative="greater")
# 4.2.6 IFR_score VS continents
rankings$continents = ifelse(rankings$continents %in% c("Oceania", "Asia"), "Asia and Oceania", rankings$continents) #merge Oceania and Asia as one variable
rankings <- rankings %>% filter(continents %in% c("Asia and Oceania", "Americas", "Europe")) #filter out Africa variable
boxplot(ln_IFR_score~rankings$continents, main = "Boxplot for ln(IFR_score) vs continents", xlab = "continents")
ln_IFR_score = log(rankings$ifr_score) #log transform
ln_IS_score = log(rankings$isr_score) #log transform
rankings_contients<- aov(ln_IFR_score~factor(rankings$continents))
summary(rankings_contients)
pairwise.t.test(ln_IFR_score, rankings$continents, p.adjust.method = "none")
#Creating each continent variables 
rankings$ln_IFR_score <- ln_IFR_score
America <- rankings %>% 
  filter(continents == "Americas") %>% 
  dplyr::select(all_of("ln_IFR_score"))
Asia_Oceania <- rankings %>% 
  filter(continents == "Asia and Oceania") %>% 
  dplyr::select(all_of("ln_IFR_score"))
Europe <- rankings %>% 
  filter(continents == "Europe") %>% 
  dplyr::select(all_of("ln_IFR_score"))
CI95 = function(x) {
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  z <- qnorm(1-0.025)
  CI95p <- c(xbar - z*s/sqrt(n), xbar + z*s/sqrt(n))
  print(paste("95% CI = [", CI95p[1], CI95p[2], "]"))
}
CI95(America[,1])
CI95(Asia_Oceania[,1])
CI95(Europe[,1])


## Simple linear regression & qqplot for ar_score against other continuous variables:
#acad rep score over employment reputation score
er_model <- lm(ar_score~er_score, data=rankings)
summary(er_model)
er_res <- resid(er_model)
qqnorm(er_res)
qqline(er_res)

#acad rep score over faculty student score
fs_model <- lm(ar_score~fs_score, data=rankings)
summary(fs_model)
fs_res <- resid(fs_model)
qqnorm(fs_res)
qqline(fs_res)

#acad rep score over citations per faculty score
cpf_model <- lm(ar_score~cpf_score, data=rankings)
summary(cpf_model)
cpf_res <- resid(cpf_model)
qqnorm(cpf_res)
qqline(cpf_res)

#acad rep score over international faculty ratio score
ifr_model <- lm(ar_score~ifr_score, data=rankings)
summary(ifr_model)
ifr_res <- resid(ifr_model)
qqnorm(ifr_res)
qqline(ifr_res)

#acad rep score over international students score
isr_model <- lm(ar_score~isr_score, data=rankings)
summary(isr_model)
isr_res <- resid(isr_model)
qqnorm(isr_res)
qqline(isr_res)

#acad rep score over international research score
irn_model <- lm(ar_score~irn_score, data=rankings)
summary(irn_model)
irn_res <- resid(irn_model)
qqnorm(irn_res)
qqline(irn_res)

#acad rep score over employment outcome score
ger_model <- lm(ar_score~ger_score, data=rankings)
summary(ger_model)
ger_res <- resid(ger_model)
qqnorm(ger_res)
qqline(ger_res)

#acad rep score over sustainability score
sus_model <- lm(ar_score~sus_score, data=rankings)
summary(sus_model)
sus_res <- resid(sus_model)
qqnorm(sus_res)
qqline(sus_res)

# Code for multi linear regression:
multi_variable <- lm(ar_score~er_score+irn_score+ger_score+sus_score, data=rankings)
step(multi_variable, direction="backward")

