#SURVEY PACKAGE USAGE
library(ahpsurvey)
library(tidyr)
library(tidyverse)
library(kableExtra)
library(Quandl)
library(reshape2)
library(magrittr)
library(ggplot2)


install.packages("kableExtra")

#PROCESS 1
library(ahp)
library(data.tree)
setwd("C:/AHP/artifacts")
nofxnAhp <- LoadFile("tomdickharry.txt")
Calculate(nofxnAhp)
fxnAhp <- LoadFile("tomdickharry-fxns.txt")
Calculate(fxnAhp)
print(nofxnAhp, "weight")
print(fxnAhp, "weight")
> ShowTable(fxnAhp)


#PROCESS 2
install.packages("Quandl")
devtools::install_github("gluc/ahp", build_vignettes = TRUE)
library(ahp)
#list example files provided by the package
list.files(system.file("extdata", package="ahp"))
#load a specific example
ahpFile <- system.file("extdata", "car.ahp", package="ahp")
carAhp <- Load(ahpFile)
Calculate(carAhp)
Analyze(carAhp)
AnalyzeTable(carAhp)

#the vacation.ahp file provides an example with multiple decision makers
ahpFile <- system.file("extdata", "vacation.ahp", package="ahp")
vacationAhp <- Load(ahpFile)
Calculate(vacationAhp)
Visualize(vacationAhp)
Analyze(vacationAhp, decisionMaker = "Dad")
AnalyzeTable(vacationAhp, decisionMaker = "Mom")
AnalyzeTable(vacationAhp, 
             decisionMaker = "Kid",
             variable = "priority", 
             sort = "orig", 
             pruneFun = function(node, dm) PruneByCutoff(node, dm, minWeight = 0.1))





#Load data
atts <- c("cult", "fam", "house", "jobs", "trans")
data(city200)
head(city200)


write.csv(city200, 'city200.csv')

#df: the dataframe

#atts: a list of attributes in the correct order

#negconvert: whether to convert all positive values to negative (logical, defaults to FALSE)

#reciprocal: whether to convert negative values (after negconvert) to its reciprocals (defaults to TRUE).

city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  head(3)

#difference of using arithmetic aggregation and dominant eigenvalue methods. 


cityahp <- city200 %>% 
  ahp.mat(atts, negconvert = T)
head(cityahp)
eigentrue <- ahp.indpref(cityahp, atts, method = "eigen")
geom <- ahp.indpref(cityahp, atts, method = "arithmetic")
error <- data.frame(id = 1:length(cityahp), maxdiff = apply(abs(eigentrue - geom), 1, max))
error %>%
  ggplot(aes(x = id, y = maxdiff)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_x_continuous("Respondent ID") +
  scale_y_continuous("Maximum difference") +
  theme_minimal()

#This visualisation offers researchers a good way to determine the amount of preference weights to be trimmed.
amean <- ahp.aggpref(cityahp, atts, method = "arithmetic")

amean


# it is possible to specify different aggregation methods for the individual and group level. 
#For instance, one can specify that in the individual level, the arithmetic mean is used to compute the individual priorities;

qtresults <- matrix(nrow = 50, ncol = 5, data = NA)
for (q in 1:50){
  qtresults[q,] <- ahp.aggpref(cityahp, atts, method = "arithmetic", 
                               aggmethod = "tmean", qt = (q-1)/100)
}

colnames(qtresults) <- atts
qtresults %>%
  as.data.frame() %>%
  mutate(trimperc = 1:nrow(qtresults)-1) %>%
  mutate(cult = cult - amean[1],
         fam = fam - amean[2],
         house = house - amean[3],
         jobs = jobs - amean[4],
         trans = trans - amean[5]) %>%
  gather(cult, fam, house, jobs, trans, key = "att", value = "weight") %>%
  ggplot(aes(x = trimperc, y = weight, group = att, shape = att, color = att, fill = att)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Quantile (from top and bottom) trimmed") +
  scale_y_continuous("Change from untrimmed mean") +
  geom_hline(yintercept = 0, color = "gray") +
  theme_minimal()


#It is also possible to quantify the heterogeneity amongst decision-makers’ priorities, information possibly lost by group aggregation. 
#This is specified using aggmethod = "sd":It is also possible to quantify the heterogeneity amongst decision-makers’ priorities, information possibly lost by group aggregation. 
#This is specified using aggmethod = "sd":


mean <- city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic")

sd <- city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic", aggmethod = "sd")

t(data.frame(mean, sd))%>% kable()

#Similarly, ahp.aggjudge aggregates the individual judgements of all decision-makers to generate a row-standardised pairwise 
#comparison matrix of all decision-makers. This allows one to compare priorities directly based on the aggregated pairwise judgements 
#of all decision-makers.
 
city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggjudge(atts, aggmethod = "geometric")


weight <- c(5,-3,2,-5,
            -7,-1,-7,
            4,-3,
            -7)
sample_mat <- ahp.mat(t(weight), atts, negconvert = TRUE)

(cr_std <- ahp.cr(sample_mat, atts))



cr <- city200 %>%
  ahp.mat(atts, negconvert = T) %>% 
  ahp.cr(atts)
table(cr <= 0.1)


## Generate a random index with 1000 simulations, 5 dimensions and seed 30000 for reproducibility (seed = 42 by default).
(RI <- ahp.ri(nsims = 1000, dim = 5, seed = 30000))


## Use this RI to calculate the consistency ratio instead of the default one.
ahp.cr(sample_mat, atts, RI)


#The ahp.indpref function provides a detailed account of each individuals’ priorities and its corresponding weighting. 
#An overlay of the violin density, boxplots and jitter plots is useful in visualising the heterogeneity in weights each respondent gives.

thres <- 0.1
dict <- c("cult" = "Culture", 
          "fam" = "Family", 
          "house" = "Housing", 
          "jobs" = "Jobs", 
          "trans" = "Transportation")

cr.df <- city200 %>%
  ahp.mat(atts, negconvert = TRUE) %>% 
  ahp.cr(atts) %>% 
  data.frame() %>%
  mutate(rowid = 1:length(cr), cr.dum = as.factor(ifelse(cr <= thres, 1, 0))) %>%
  select(cr.dum, rowid)

city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.indpref(atts, method = "eigen") %>% 
  mutate(rowid = 1:nrow(eigentrue)) %>%
  left_join(cr.df, by = 'rowid') %>%
  gather(cult, fam, house, jobs, trans, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption = paste("n =", nrow(city200), ",", "Mean CR =",
                           round(mean(cr),3)))+
  theme_minimal()


#A better way to visualise the pairwise comparisons is a bar chart:

preference <- t(ahp.indpref(sample_mat, atts, method = "eigen"))
preference

S <- preference %*% t((preference)^-1)
S

sample_mat[[1]] * t(S)

error <- ahp.error(sample_mat, atts, reciprocal = TRUE)
error

gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

mat <- cityahp %>%
  ahp.error(atts, reciprocal = TRUE) %>%
  unlist() %>%
  as.numeric() %>%
  array(dim=c(length(atts), length(atts), length(cityahp))) %>%
  apply(c(1,2), gm_mean)

colnames(mat) <- rownames(mat) <- atts

mat


#Finding inconsistent pairwise comparisons by maximum

city200 %>%
  ahp.mat(atts) %>%
  ahp.pwerror(atts) %>%
  head()


cityahp %>%
  ahp.pwerror(atts) %>% 
  gather(top1, top2, top3, key = "max", value = "pair") %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(x = pair, y = Freq, fill = max)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous("Frequency", breaks = c(seq(0,180,20))) +
  scale_fill_discrete(breaks = c("top1", "top2", "top3"), labels = c("1", "2", "3")) +
  scale_x_discrete("Pair") +
  guides(fill = guide_legend(title="Rank")) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.ontop = FALSE)


#Inconsistent pairwise matrices are problematic for AHP survey analysts. 
#Harker (1987) described a method to replace inconsistent values:

family <- c(1,1/5,1/3,1/7,1/6,1/6,3,4,
            5,1,3,1/5,1/3,1/3,5,7,
            3,1/3,1,1/6,1/3,1/4,1/6,5,
            7,5,6,1,3,4,7,8,
            6,3,3,1/3,1,2,5,6,
            6,3,4,1/4,1/2,1,5,6,
            1/3,1/5,6,1/7,1/5,1/5,1,2,
            1/4,1/7,1/5,1/8,1/6,1/6,1/2,1)

fam.mat <- list(matrix(family, nrow = 8 , ncol = 8))

atts <- c("size", "trans", "nbrhd", "age", "yard", "modern", "cond", "finance")

rownames(fam.mat[[1]]) <- colnames(fam.mat[[1]]) <- atts

fam.mat[[1]] %>% kable()

