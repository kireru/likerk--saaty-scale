#NEW DATA
    
pisaitems<- read.csv(file="pisaitems.csv",head=TRUE,sep=",")
data("pisaitems")
head(pisaitems)
##### Item 29: How often do you read these materials because you want to?
title <- "How often do you read these materials because you want to?"
items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
names(items29) = c("Magazines", "Comic books", "Fiction", 
				"Non-fiction books", "Newspapers")
head(items29); ncol(items29)

sapply(items29, class) #Verify that all the columns are indeed factors
sapply(items29, function(x) { length(levels(x)) } ) 

l29 <- likert(items29)

str(l29)

#Summary Results

l29s <- likert(summary = l29$results)

str(l29s)
summary(l29s)

#Plotting
#Scale figure height and width, and reduce size of legend.text for plots:

scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

# Plots
plot(l29s) + ggtitle(title)

# Plots for when Not centered
plot(l29s, centered=FALSE) + ggtitle(title)

# Plots for without a center
plot(l29s, include.center=FALSE) + ggtitle(title)

# Plots for with a center
plot(l29s, center=2) + ggtitle(title)

# HEAT PLOT
plot(l29s, type = 'heat') + ggtitle(title) + 
    theme(legend.position = 'none')

#GROUPED SUMMARY
l29g <- likert(items29, grouping=pisaitems$CNT)

str(l29g$results)

#GROUPED SUMMARY: Make a summary from results, grouped by the variable in column 1 of results:
l29gs <- likert(summary = l29g$results, grouping = l29g$results[,1])
str(l29gs)

#Grouped plots
#Rescale figures to full height:

knitr::opts_chunk$set(fig.height = knitr::opts_chunk$get('fig.height')*2)
plot(l29gs) + ggtitle(title)

#Centered
plot(l29gs, center=2) + ggtitle(title)
