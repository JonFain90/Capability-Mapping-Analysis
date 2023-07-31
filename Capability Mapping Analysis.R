library(tidyverse)
library(readxl)
library(data.table)
library(writexl)

cap_df <- read_xlsx("test_capability.xlsx")

cap_likert_viz <- cap_df %>%
  select('Please choose the product for which you are completing this assessment', `Present State`, `Desired State`, 
         `Present State2`, `Desired State2`, 
         `Present State3`, `Desired State3`, 
         `Present State4`, `Desired State4`, 
         `Present State5`, `Desired State5`, 
         `Present State6`, `Desired State6`,
         `Present State7`, `Desired State7`) %>%
  rename(product = 'Please choose the product for which you are completing this assessment',
         'Present Data Architecture' = `Present State`,
         'Future Data Architecture' = `Desired State`,
         'Present Data Access' = `Present State2`,
         'Future Data Access' = `Desired State2`,
         'Present Data Support' = `Present State3`,
         'Future Data Support' = `Desired State3`,
         'Present Data Governance' = `Present State4`,
         'Future Data Governance' = `Desired State4`,
         'Present Data Quality' = `Present State5`,
         'Future Data Quality' = `Desired State5`,
         'Present Data Reporting' = `Present State6`,
         'Future Data Reporting' = `Desired State6`,
         'Present Data Usage' = `Present State7`,
         'Future Data Usage' = `Desired State7`) %>%
  gather(capability, rating, -product ) %>%
  mutate(state = ifelse(grepl("Present", capability),'present', 'future'),
         capability = str_replace(capability, '(.*?)Present (.*?)', '\\1'),
         capability = str_replace(capability, '(.*?)Future (.*?)', '\\1'),
         rating = as.numeric(rating))  

cap_likert <- cap_likert_viz %>%
  spread(state, rating) %>%
  mutate(gap = future - present) %>%
  select(product, capability, present, future, gap) 


# Qualitative Assessment

cap_qual <- cap_df %>%
  select(contains("Please")) %>%
  rename(product = 'Please choose the product for which you are completing this assessment') %>%
  gather(question, comment, -product ) %>%
  mutate(capability = str_extract(question, "Data (?s)(.*$)")) %>%
  select(product, capability, comment)


cap_full <- cap_likert %>%
  left_join(cap_qual, by = c("product", "capability"))


write_xlsx(cap_likert, "walk_me.xlsx")

ggplot(cap_likert_viz, aes(capability, rating, fill = state)) +
  geom_bar( position = position_dodge(width = 0), width = 2, stat = "identity", alpha = .5) +
  coord_flip()

ggplot(cap_likert_viz, aes(reorder(capability, rating), rating, fill = state)) +
  geom_bar(position = 'dodge', stat = "identity", alpha = .5) +
  coord_flip()  +
  theme_classic()


# Cleveland dot plot
right_label <- cap_likert_viz %>%
  filter(state == 'future') 

left_label <- cap_likert_viz %>%
  filter(state == 'present') 


ggplot(cap_likert_viz, aes(reorder(capability, -rating), rating))  +
  geom_line(aes(group = capability)) +
  geom_point(aes(color = state), size = 4) +
  geom_text(data = right_label, aes(color = state, label = round(rating, 0)),
            size = 3.5, hjust = -2) +
  geom_text(data = left_label, aes(color = state, label = round(rating, 0)),
            size = 3.5, hjust = 2.5) +
 # scale_x_discrete(limits = c(-5, 10))
  labs(title = "Gap Analysis of Present vs. Future State Capabilities \nfor Walk Me, Ordered by Present Rating") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank())
