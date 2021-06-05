
#ELIZA DATA
library("likert")
library(dplyr)

#Make Variables 

Eliza<- read.csv(file="ELIZA2.csv",head=TRUE,sep=",")
head(Eliza)


##### Item 24: Reading Attitudes
items24 <- Eliza[,substr(names(Eliza), 1,5) == 'ST24Q']
head(items24); ncol(items24)
str(items24)


sapply(items24, class) #Verify that all the columns are indeed factors
sapply(items24, function(x) { length(levels(x)) } ) # The number of levels in each factor

#CONVERT MY DATA TO FACTOR
items24<-as.data.frame((unclass(items24)))# If doesn't work we move below
items24[sapply(items24, is.character)]<-lapply(items24[sapply(items24, is.character)],as.factor)

levels(ST24Q01)# not working why?

#solving

levels(items24$ST24Q01)
#OR
print(levels(items24$ST24Q01))
levels(items24$ST24Q02)
levels(items24$ST24Q03)
levels(items24$ST24Q04)

# SOlving this issue
levels(items24$ST24Q01)[levels(items24$ST24Q01)==""]<-"Un Decided(UN)"
levels(items24$ST24Q02)[levels(items24$ST24Q02)==""]<-"Strongly Disagree(SD)"
levels(items24$ST24Q03)[levels(items24$ST24Q03)==""]<-"Strongly Disagree(SD)"
levels(items24$ST24Q04)[levels(items24$ST24Q04)==""]<-"Un Decided(UN)"
levels(items24$ST24Q05)[levels(items24$ST24Q05)==""]<-"Un Decided(UN)"
levels(items24$ST24Q06)[levels(items24$ST24Q06)==""]<-"Un Decided(UN)"
levels(items24$ST24Q07)[levels(items24$ST24Q07)==""]<-"Un Decided(UN)"
levels(items24$ST24Q08)[levels(items24$ST24Q08)==""]<-"Un Decided(UN)"


levels(items24$Has not motivated student effort)
str(items24)

#HII NI YA KUTRY TU
levels<-c("Strongly Agree(SA)","Agree(A)","Un Decided(UN)","Disagree(D)","Strongly Disagree(SD)")
ordered_factor<-ordered(items24$ST24Q01, levels=levels)
items24<-factor(items24$ST24Q01,levels(items24)[c(Strongly Agree(SA),Agree(A),Un Decided(UN),Disagree(D),Strongly Disagree(SD)])


# CONTI....
fine <- likert(items24)

l24 #print(fine)
summary(fine)
summary(fine, center=1.5)
summary(fine, center=2)

# xtable
xtable(fine)

# Plots
# Plots
plot(fine)
plot(fine, ordered=FALSE, group.order=names(items24)) #Specify the exact order of the y-axis
plot(fine, centered=FALSE, wrap=30)
plot(l24, center=1.5, wrap=30)
plot(l24, center=2, wrap=30)
plot(l24, center=2, include.center=FALSE, wrap=30)
plot(l24, center=2, include.center=FALSE, wrap=20)
plot(l24, plot.percents=TRUE, plot.percent.low=FALSE, plot.percent.high=FALSE)
plot(l24, center=2, plot.percents=TRUE, plot.percent.low=FALSE, plot.percent.high=FALSE)


Eliza<- read.csv(file="ELIZA2.csv",head=TRUE,sep=",")
head(Eliza)

##### Item 24: Reading Attitudes
items24 <- Eliza[,substr(names(Eliza), 1,5) == 'ST24Q']

# INTRODUCE VARIABLE NAMES TWO METHODS
# BEING SPECIFIC-EXACT
names(items24) <- c(
			ST24Q01="Has not motivated student effort ",
			ST24Q02="Been associated with modest positive improvements",
			ST24Q03="Students are more responsive to financial incentives",
			ST24Q04="Not cost effective means to increase student effort",
			ST24Q05="Cash rewards improve administrator/students relations",
			ST24Q06="influencing effect on student’s academic achievement",
			ST24Q07="reduces students’ academic achievement",
			ST24Q08="responsiveness to short-term monetary incentives than girls")
			
			#OR

names(items24) = c("Has not motivated student effort ", 
" Been associated with modest positive improvements", 
"Students are more responsive to financial incentives", 
" Not cost effective means to increase student effort", 
"Cash rewards improve administrator/students relations",
"influencing effect on student’s academic achievement",
"reduces students’ academic achievement",
"responsiveness to short-term monetary incentives than girls")



##### Group by Experience level
fine2 <- likert(items24, grouping=Eliza$CNT)


fine2 <- likert(fine2)
l24 #print(fine2)
summary(fine2)
summary(fine2, center=1.5)
summary(fine2, center=2)

# xtable
xtable(fine2)



print(l24g)
summary(l24g)
summary(l24g, center=1.5)
summary(l24g, center=2)



# xtable
# xtable(l24g) # TODO: Doesn't work!

# Plots
plot(l24g)
plot(l24g, centered=FALSE)
plot(l24g, center=1.5)
plot(l24g, center=2)
plot(l24g, center=2, include.center=FALSE)
plot(l24g, group.order=c('Student', 'Teacher', 'Principal'))

# Alternate panel arrangements.
plot(l24g, panel.arrange='h', wrap=20)
plot(l24g, panel.arrange=NULL, wrap=40)# this is better


# Density plots
plot(l24g, type='density')


# Reordering the groups
plot(l24g, group.order=c('Student', 'Teacher', 'Principal'))

#GIVING TITLE
title <- "Monetary Incentives and Students’ Academic achievement"


items24 <- Eliza[,substr(names(Eliza), 1,5) == 'ST24Q']



