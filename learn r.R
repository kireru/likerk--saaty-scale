# Load in the data set from disk.
data.file <- file.path('data', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')

#summary of one particular vector
heights <- with(heights.weights, Height)
summary(heights)

# Define our own mean and median functions.
my.mean <- function(x)
{
  return(sum(x) / length(x))
}

my.median <- function(x)
{
  sorted.x <- sort(x)
  if (length(x) %% 2 == 0)
  {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  }
  else
  {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

#finding mean using random value examples
my.vector <- c(0, 100)

my.vector
# [1]	0 100

mean(my.vector)
my.mean(my.vector)    #gives the same result
#[1] 50

median(my.vector)
#[1] 50

# finding our mean and median functions 
my.mean(heights)

# Methods of finding quantiles.
quantile(heights)
# specific quantiles
quantile(heights, probs = seq(0, 1, by = 0.20))

# Define a variance function to assess the spread of data with bias.
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / length(x))
}

my.var(heights)

# Update the variance function to make it unbiased.
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x) - 1))
}

my.var(heights)


#  standard deviations instead for thinking about ranges.
my.sd <- function(x)
{
  return(sqrt(my.var(x)))
}

my.sd(heights)
 


# Start visualizing data using the ggplot2 package.
library('ggplot2')

# Experiment with histograms.
ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 1)

ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 0.01)

# Experiment with kernel density estimates.
ggplot(heights.weights, aes(x = Height)) +
  geom_density()

# Separate out heights and weights based on gender.also add title and a caption
ggplot(heights.weights, aes(x = Height, fill = Gender)) +
  geom_density()+ 
  labs(title="Height Vs. Weight", 
       subtitle="Grouped by Gender",
       caption="Source: andy")

ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
  geom_density()

# Produce two facets in a single plot to make it easier to see the hidden structure.
ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
  geom_density() +
  facet_grid(Gender ~ .)

# Generate scatterplots of the heights and weights to see their relationship.
ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point()

# Add a smooth shape that relates the two explicitly.
ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()

# Visualize how gender depends on height and weight.
ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide = "none") + 
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw()

# An alternative using bright colors.
ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) +
  geom_point()

ggplot(heights.weights, aes(x = Height, fill = Gender)) +
  geom_density()+ 
  labs(title="Height Vs. Weight", 
       subtitle="Grouped by Gender",
       caption="Source: andy")

New.data <- read.csv("ELIZA.csv", header = TRUE, sep = ',')

#Summary
summary(New.data)

# Histogram on a Categorical variable
g <- ggplot(New.data, aes(Age))
g + geom_bar(aes(fill=Type_of_respondent), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes")

ggplot(New.data, aes(x = Weight, fill = Gender)) +
  geom_density() +
  facet_grid(Age ~ .)


ggplot(New.data, aes(x = Education_level, fill = ï..Your_gender)) +
  geom_density()+ 
  labs(title="Education Vs. Gender", 
       subtitle="Grouped by Gender",
       caption="Source: andy")


Eliza<- read.csv(file="ELIZA.csv",head=TRUE,sep=",")

library("likert")
 
data("pisaitems")
head(pisaitems)


# create bar plot data
New.data <- New.data %>%
  dplyr::group_by(Satisfaction) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%


# create pie chart
ggplot(New.data,  aes("", Percent, fill = Satisfaction)) + 
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = clrs5) +
  theme_void()
