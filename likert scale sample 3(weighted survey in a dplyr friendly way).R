install.packages("tidyverse")
library(tidyverse)
n_sample <- 100
sample_data <- tibble(
  id = seq(1, n_sample, 1),
  q1 = sample.int(5, n_sample, replace = TRUE),
  q2 = sample.int(5, n_sample, replace = TRUE),
  q3 = sample.int(5, n_sample, replace = TRUE),
  q4 = sample.int(5, n_sample, replace = TRUE),
  q5 = sample.int(5, n_sample, replace = TRUE),
  group = sample(c("A","B"), n_sample, replace = TRUE),
  weight = runif(n_sample, 1, 3)
)




library(srvyr)

# Tranform sample data into survey data
survey_data <- sample_data %>%
  as_survey_design(id = 1,
                   weights = weight)# Function that gets the weighted frequency of answers for a single question
get_weighted_results <- function(q, survey_data){
  survey_data %>%
    group_by(group) %>%
    summarize(a = survey_total(get(q) == 1),
              b = survey_total(get(q) == 2),
              c = survey_total(get(q) == 3),
              d = survey_total(get(q) == 4),
              e = survey_total(get(q) == 5)) %>%
    pivot_longer(-group,
                 names_to = "ans",
                 values_to = "freq") %>%
    filter(!str_detect(ans, "se")) 
}# Apply function to each question and calculate proportion
likert_results <- tibble(q = c("q1", "q2", "q3", "q4", "q5")) %>%
  mutate(results = map(q, get_weighted_results, survey_data)) %>%
  unnest(results) %>%
  group_by(q, group) %>%
  mutate(prop = freq / sum(freq)) %>%
  ungroup()



plot_weighted_likert <- function(x, my_labels, likert_results) {
  # Filter data for questions needed
  likert_results2 <- likert_results %>%
    filter(q %in% x)
  # Create data frame with labels
  prop_labels <- likert_results2 %>%
    mutate(
      position = case_when(
        ans == "a" | ans == "b" ~ "left",
        ans == "c" ~ "center",
        ans == "d" | ans == "e" ~ "right"
      )
    ) %>%
    group_by(q, group, position) %>%
    summarize(label = sum(prop * 100)) %>%
    pivot_wider(names_from = position,
                values_from = label)
  # Data frame with right side values
  high_columns <- likert_results2 %>%
    filter(ans == "c" | ans == "d" | ans == "e") %>%
    mutate(prop = case_when(ans == "c" ~ prop / 2 * 100,
                            ans == "d" ~ prop * 100,
                            ans == "e" ~ prop * 100,)) %>%
    arrange(ans)
  # Data frame with left side values
  low_columns <- likert_results2 %>%
    filter(ans == "a" | ans == "b" | ans == "c") %>%
    mutate(prop = case_when(ans == "a" ~ prop * 100,
                            ans == "b" ~ prop * 100,
                            ans == "c" ~ prop / 2 * 100)) %>%
    arrange(ans)
  # Define empty ggplot object
  p <- ggplot() +
    # Add central black line
    geom_hline(yintercept = 0) +
    # Add right side columns
    geom_bar(
      data = high_columns,
      mapping = aes(x = group,
                    y = prop,
                    fill = ans),
      position = position_stack(reverse = TRUE),
      stat = "identity"
    ) +
    # Add left side columns
    geom_bar(
      data = low_columns,
      mapping = aes(x = group,
                    y = -prop,
                    fill = ans),
      position = "stack",
      stat = "identity"
    ) +
    # Left side labels
    geom_text(
      data = prop_labels,
      mapping = aes(
        x = group,
        y = -100,
        label = paste(round(left) , "%", sep = "")),
        hjust = 1,
        color = "black",
        size = 3
    ) +
    # Central labels
    geom_text(
      data = prop_labels,
      mapping = aes(
        x = group,
        y = 0,
        label = paste(round(center) , "%", sep = "")),
        hjust = 0.5,
        color = "black",
        size = 3
    ) +
    # Right side labels
    geom_text(
      data = prop_labels,
      mapping = aes(
        x = group,
        y = 100,
        label = paste(round(right) , "%", sep = "")),
        hjust = -0.2,
        color = "black",
        size = 3
    )  +
    # Scale formatting
    scale_y_continuous(
      breaks = seq(-100, 100, 50),
      limits = c(-105, 105),
      labels = abs
    )  +
    # More formatting
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.ticks = element_blank(),
          strip.background = element_rect(fill = "#F0F0F0",
                                          color = "#F0F0F0"),
          panel.background = element_blank(),
          panel.border = element_rect(
            colour = "#F0F0F0",
            fill = NA,
            size = 1.5)
          ) +
    facet_wrap(~ q, ncol = 1) +
    coord_flip() +
    ylab("Proportion") +
    xlab("") +
    # Change Likert labels
    scale_fill_discrete(labels = my_labels)
  # Print the plot
  p
}



my_labels <- c("Very\nunsatisfied",
               "Unsatisfied",
               "Neutral",
               "Satisfied",
               "Very\nsatistifed")
plot_weighted_likert(c("q1"), my_labels, likert_results)
plot_weighted_likert(c("q2", "q3", "q4"), my_labels, likert_results)
plot_weighted_likert(c("q1", "q2", "q3", "q4", "q5"), my_labels, likert_results)




