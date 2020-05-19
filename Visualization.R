##########
# Project: Analyzing early online support for Justin Amash
# Contributor: Suzie Mulesky
# Date: May 8, 2020
#
# Task: data visualization
##########

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(extrafont)

# Load dataset
df <- read.csv("amash.csv")

# Import fonts
#font_import()
#loadfonts(device = "win")
windowsFonts()

# How likely are you to support Justin Amash's 2020 Presidential run?
n <- nrow(df)

question1 <- df %>% 
  group_by(Q1) %>% 
  summarise(q1_prop = round(n()/n, 2)) %>% 
  mutate(y_label = paste(q1_prop*100, "%", sep = "")) %>% 
  ggplot(aes(x = reorder(Q1, q1_prop), y = q1_prop)) +
  geom_bar(stat = "identity", fill = "#016c59", color = "black", size = 1, alpha = 0.6) +
  geom_text(aes(label = y_label), hjust = -.05, color = "black", size = 6.8, family = "Georgia") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "",
       caption = "Source: Qualtrics survey conducted on May 6-7, 2020 of early online Amash supporters sampled from the \nr/Amash4President2020 subreddit and the Justin Amash 2020 Discord channel.") +
  ggtitle("\"How likely are you to support Justin Amash's 2020 Presidential run?\"") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 14, color = "grey40", hjust = 0),
        axis.text.y = element_text(size = 18, color = "black", margin = margin(r = -20)),
        axis.text.x = element_text(size = 18, color = "black"),
        text = element_text(family = "Georgia"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 14, r = 14, b = 14))

ggsave("question1.png", plot = question1, scale = 2)


# Before Amash announced his candidacy, who would you most likely have voted for?
n_supporters <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  nrow()

question2 <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  group_by(Q2) %>% 
  summarise(q2_prop = round(n()/n_supporters, 2)) %>% 
  mutate(y_label = paste(q2_prop*100, "%", sep = ""),
         color_code = ifelse(Q2 == "Biden", "blue",
                             ifelse(Q2 == "Trump", "red", 
                                    ifelse(Q2 == "The Libertarian Party nominee", "yellow", "gray")))) %>% 
  ggplot(aes(x = reorder(Q2, q2_prop), y = q2_prop)) +
  geom_bar(stat = "identity", aes(fill = color_code), size = 1, color = "black", alpha = 0.75) +
  geom_text(aes(label = y_label), hjust = 1.2, color = "black", size = 6.8, family = "Georgia") +
  ggtitle("\"Before Amash announced his candidacy, who would you most likely have voted for?\"") +
  scale_fill_manual(values = c("#4575b4", "#878787", "#d73027", "goldenrod1")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "",
       caption = "Source: Qualtrics survey conducted on May 6-7, 2020 of early online Amash supporters sampled from the \nr/Amash4President2020 subreddit and the Justin Amash 2020 Discord channel.") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 17, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 14, color = "grey40", hjust = 0),
        axis.text.y = element_text(size = 18, color = "black", margin = margin(r = -20)),
        axis.text.x = element_text(size = 18, color = "black"),
        text = element_text(family = "Georgia"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 14, r = 14, b = 14),
        legend.position = "none"
        )

ggsave("question2.png", plot = question2, scale = 2)




# Which TWO issues are most important to you as a voter?
q3_1 <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  select(q3 = Q3_1)
q3_2 <- df %>%
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  select(q3 = Q3_2)
q3 <- rbind(q3_1, q3_2)
rm(q3_1, q3_2)

n_top2 <- nrow(q3)

question3 <- q3 %>% 
  group_by(q3) %>% 
  summarise(q3_prop = round(n()/n_top2, 2)) %>% 
  mutate(y_label = paste(q3_prop*100, "%", sep = "")) %>% 
  ggplot(aes(x = reorder(q3, q3_prop), y = q3_prop)) +
  geom_bar(stat = "identity", fill = "#016c59", color = "black", size = 1, alpha = 0.6) +
  geom_text(aes(label = y_label), hjust = 1.2, color = "black", size = 6.8, family = "Georgia") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("\"Which TWO issues are most important to you as a voter?\"") +
  labs(x = "", y = "",
       caption = "Source: Qualtrics survey conducted on May 6-7, 2020 of early online Amash supporters sampled from the \nr/Amash4President2020 subreddit and the Justin Amash 2020 Discord channel.") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
        plot.caption = element_text(size = 14, color = "grey40", hjust = 0),
        axis.text.y = element_text(size = 18, color = "black", margin = margin(r = -20)),
        axis.text.x = element_text(size = 18, color = "black"),
        text = element_text(family = "Georgia"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 14, r = 14, b = 14))

ggsave("question3.png", plot = question3, scale = 2)



# --- Rank UP TO 3 reasons that you support or are considering supporting Justin Amash for President.
q4_1 <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  group_by(Q4_1) %>% 
  summarise(n_q41 = n()) %>% 
  rename(rank = Q4_1) %>% 
  na.omit()

q4_2 <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  group_by(Q4_2) %>% 
  summarise(n_q42 = n()) %>% 
  rename(rank = Q4_2) %>% 
  na.omit()

q4_3 <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  group_by(Q4_3) %>% 
  summarise(n_q43 = n()) %>% 
  rename(rank = Q4_3) %>% 
  na.omit()

q4_4 <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  group_by(Q4_4) %>% 
  summarise(n_q44 = n()) %>% 
  rename(rank = Q4_4) %>% 
  na.omit()

q4_5 <- df %>% 
  filter(Q1 %in% c("I am very likely to support him", "I may support him")) %>% 
  group_by(Q4_5) %>% 
  summarise(n_q45 = n()) %>% 
  rename(rank = Q4_5) %>% 
  na.omit()

q4 <- Reduce(function(x, y) merge(x = x, y = y, by = "rank"),
             list(q4_1, q4_2, q4_3, q4_4, q4_5))

q4 <- as.data.frame(t(q4))
names(q4) <- c("Rank 1", "Rank 2", "Rank 3")
q4 <- q4[-1,]
q4$option <- c("Amash's domestic policy views",
               "Amash's foreign policy views",
               "I do not trust Trump or Biden to operate in my interest",
               "I believe Trump and Biden are mentally unfit or unhealthy",
               "I am trying to disturb the current 2-party system")
q4$`Rank 1` <- as.numeric(as.character(q4$`Rank 1`))
q4$`Rank 2` <- as.numeric(as.character(q4$`Rank 2`))
q4$`Rank 3` <- as.numeric(as.character(q4$`Rank 3`))
q4$option <- as.factor(q4$option)

# Most important reason
rank1 <- q4 %>% 
  mutate(`Rank 1` = round(`Rank 1`/n_supporters, 2),
         y_label = paste(`Rank 1`*100, "%", sep = "")) %>% 
  ggplot(aes(x = reorder(option, `Rank 1`), y = `Rank 1`)) +
  geom_bar(stat = "identity", aes(fill = option), color = "black", size = 1, alpha = 0.75) +
  geom_text(aes(label = y_label), hjust = 1.2, color = "black", size = 6, family = "Georgia") +
  scale_y_continuous(limits = c(0, .45), labels = scales::percent) +
  scale_fill_manual(values = c("#a6cee3", "#fb9a99", "#b2df8a", "#33a02c", "#1f78b4")) +
  ggtitle("\"Rank UP TO 3 reasons that you support or are considering \nsupporting Justin Amash for President.\"",
          subtitle = "Most important reason") +
  labs(x = "", y = "") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, color = "black", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, color = "black", face = "bold", margin = margin(t = 16), hjust = 0.5),
        axis.text.y = element_text(size = 16, color = "black", margin = margin(r = -20)),
        #axis.text.x = element_text(size = 16, color = "black"),
        axis.text.x = element_blank(),
        text = element_text(family = "Georgia"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(r = 14)
  )

# Second most important reason
rank2 <- q4 %>% 
  mutate(`Rank 2` = round(`Rank 2`/n_supporters, 2),
         y_label = paste(`Rank 2`*100, "%", sep = "")) %>% 
  ggplot(aes(x = reorder(option, `Rank 1`), y = `Rank 2`)) +
  geom_bar(stat = "identity", aes(fill = option), color = "black", size = 1, alpha = 0.75) +
  geom_text(aes(label = y_label), hjust = 1.2, color = "black", size = 6, family = "Georgia") +
  scale_y_continuous(limits = c(0, 0.45), labels = scales::percent) +
  scale_fill_manual(values = c("#a6cee3", "#fb9a99", "#b2df8a", "#33a02c", "#1f78b4")) +
  ggtitle("",
          subtitle = "Second most important reason") +
  labs(x = "", y = "") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 16, color = "black", face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 16, color = "black", margin = margin(r = -20)),
        #axis.text.x = element_text(size = 16, color = "black"),
        axis.text.x = element_blank(),
        text = element_text(family = "Georgia"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(r = 14)
  )

# Third most important reason
rank3 <- q4 %>% 
  mutate(`Rank 3` = round(`Rank 3`/n_supporters, 2),
         y_label = paste(`Rank 3`*100, "%", sep = "")) %>% 
  ggplot(aes(x = reorder(option, `Rank 1`), y = `Rank 3`)) +
  geom_bar(stat = "identity", aes(fill = option), color = "black", size = 1, alpha = 0.75) +
  geom_text(aes(label = y_label), hjust = 1.2, color = "black", size = 6, family = "Georgia") +
  scale_y_continuous(limits = c(0, 0.45), labels = scales::percent) +
  scale_fill_manual(values = c("#a6cee3", "#fb9a99", "#b2df8a", "#33a02c", "#1f78b4")) +
  ggtitle("", subtitle = "Third most important reason") +
  labs(x = "", y = "",
       caption = "Source: Qualtrics survey conducted on May 6-7, 2020 of early online Amash supporters sampled from the \nr/Amash4President2020 subreddit and the Justin Amash 2020 Discord channel.") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 16, color = "black", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 14, color = "grey40", hjust = 0),
        axis.text.y = element_text(size = 16, color = "black", margin = margin(r = -20)),
        axis.text.x = element_text(size = 16, color = "black"),
        text = element_text(family = "Georgia"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(r = 14, b = 14)
  )

question4 <- grid.arrange(rank1, rank2, rank3, nrow = 3, 
             padding = unit(2, "line"))


ggsave("question4.png", plot = question4, scale = 2)


