# tidy tuesday ------------------------------------------------------------

# Dog Breed Data
# February 1, 2022

# libraries ---------------------------------------------------------------

install.packages("tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(waffle)
library(viridis)

# read data ---------------------------------------------------------------

# Import breed traits
breed_traits <- 
  readr::read_csv(
    paste0(
      "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/da",
      "ta/2022/2022-02-01/breed_traits.csv")) 
  #rename_all(~tolower(.x) %>% str_replace_all(., " ", "_")) 

# Import breed ranks
breed_rank_all <- 
  readr::read_csv(
  paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master",
  "/data/2022/2022-02-01/breed_rank.csv")) %>% 
  select(-c("links", "Image"))

# Import trait descriptions
trait_description <- 
  readr::read_csv(
    paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/mast",
    "er/data/2022/2022-02-01/trait_description.csv"))

# data cleaning -----------------------------------------------------------

breed_ranks <- 
  breed_rank_all %>% 
  pivot_longer(cols = contains("Rank"), names_to = "yr") %>% 
  rename(breed = Breed, rank = value) %>% 
  mutate(yr = str_extract(yr, "[0-9]{4}")) %>% 
  group_by(breed) %>% 
  summarize(mean_rank = mean(rank, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(id = row_number())

traits <- 
  breed_traits %>% 
  mutate(across(contains(" "), ~as.character(.x))) %>% 
  pivot_longer(cols = contains(" "), names_to = "trait") %>% 
  rename(breed = Breed) %>% 
  arrange(breed) %>% 
  group_by(breed) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup() %>% 
  left_join(breed_ranks %>% select(-breed), by = c("id")) %>% 
  filter(str_detect(value, "[0-9]"))

coat_traits <- 
  breed_traits %>% 
  mutate(across(contains(" "), ~as.character(.x))) %>% 
  pivot_longer(cols = contains(" "), names_to = "trait") %>% 
  rename(breed = Breed) %>% 
  arrange(breed) %>% 
  group_by(breed) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup() %>% 
  left_join(breed_ranks %>% select(-breed), by = c("id")) %>% 
  filter(!str_detect(value, "[0-9]"))

# plot --------------------------------------------------------------------

# Top 20 (good things)

good20 <- 
  traits %>% 
  arrange(mean_rank) %>% 
  filter(mean_rank < 20) %>% 
  filter(!str_detect(trait, "Grooming")) %>%
  filter(!str_detect(trait, "Mental")) %>% 
  filter(!str_detect(trait, "Shedding")) %>%
  filter(!str_detect(trait, "Barking")) %>%
  filter(!str_detect(trait, "Drooling")) %>%
  mutate(
    trait_order = 
      case_when(
        str_detect(trait, "Adaptability") ~ 1,
        str_detect(trait, "Affectionate") ~ 3,
        str_detect(trait, "Dogs") ~ 5,
        str_detect(trait, "Children") ~ 4,
        str_detect(trait, "Energy") ~ 8,
        str_detect(trait, "Strangers") ~ 6,
        str_detect(trait, "Playfulness") ~ 9,
        str_detect(trait, "Trainability") ~ 2,
        str_detect(trait, "Nature") ~ 7,
        TRUE ~ 0),
    trait = 
      case_when(
        str_detect(trait, "Adaptability") ~ "Adaptable",
        str_detect(trait, "Affectionate") ~ "Affectionate",
        str_detect(trait, "Dogs") ~ "Dog-friendly",
        str_detect(trait, "Children") ~ "Kid-friendly",
        str_detect(trait, "Mental") ~ "Intelligence",
        str_detect(trait, "Strangers") ~ "Stranger-friendly",
        str_detect(trait, "Playfulness") ~ "Playfulness",
        str_detect(trait, "Trainability") ~ "Trainable",
        str_detect(trait, "Nature") ~ "Protective",
        TRUE ~ trait),
  ) %>% 
  ggplot(
    aes(
      x = reorder(trait, trait_order), 
      y = reorder(breed, -mean_rank), 
      fill = value)) +
  geom_tile() +
  coord_equal(expand = T) +
  labs(x = NULL, y = "Dog Breed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, family = "mono"),
        axis.text.y = element_text(family = "mono"),
        axis.title.y = element_text(family = "mono"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "vertical",
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_fill_viridis(
    labels = c("1 = Low (bad)", "2", "3", "4", "5 = High (good)"),
    discrete = T) + 
  guides(fill = guide_legend(reverse = TRUE))

# Top 20 (bad things)

bad20 <- 
  traits %>% 
  arrange(mean_rank) %>% 
  filter(mean_rank < 20) %>% 
  filter(str_detect(trait, "Grooming") | 
           #str_detect(trait, "Mental") |
           str_detect(trait, "Shedding") |
           str_detect(trait, "Drooling") |
           str_detect(trait, "Barking")) %>% 
  mutate(
    trait_order = 
      case_when(
        str_detect(trait, "Grooming") ~ 1,
        str_detect(trait, "Shedding") ~ 2,
        str_detect(trait, "Barking") ~ 3,
        str_detect(trait, "Drooling") ~ 4,
        TRUE ~ 0),
    trait = 
      case_when(
        str_detect(trait, "Barking") ~ "Barking",
        str_detect(trait, "Grooming") ~ "Grooming",
        str_detect(trait, "Drooling") ~ "         Drooling",
        str_detect(trait, "Shedding") ~ "Shedding",
        TRUE ~ trait),
  ) %>% 
  ggplot(
    aes(
      x = reorder(trait, trait_order), 
      y = reorder(breed, -mean_rank), 
      fill = value)) +
  geom_tile() +
  coord_equal(expand = T) +
  labs(x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, family = "mono"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "vertical",
    plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_fill_viridis(
    labels = c("1 = Low (good)", "2", "3", "4", "5 = High (bad)"), 
    direction = -1,
    discrete = T) 

# Combined top 20
plot_row <- 
  cowplot::plot_grid(good20, bad20) 

title <- 
  cowplot::ggdraw() + 
  cowplot::draw_label(
    "Traits of the top 20 AKC dog breeds by registration (2013-2020)",
    fontface = "bold",
    fontfamily = "mono",
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

cowplot::plot_grid(title, plot_row, ncol = 1, rel_heights = c(0.1, 1))

# 101 dogs ----------------------------------------------------------------

# Top 101

good101 <- 
  traits %>% 
  arrange(mean_rank) %>% 
  filter(mean_rank < 99) %>% 
  filter(!str_detect(trait, "Grooming")) %>%
  filter(!str_detect(trait, "Mental")) %>% 
  filter(!str_detect(trait, "Shedding")) %>%
  filter(!str_detect(trait, "Barking")) %>%
  filter(!str_detect(trait, "Drooling")) %>%
  mutate(
    trait_order = 
      case_when(
        str_detect(trait, "Adaptability") ~ 1,
        str_detect(trait, "Affectionate") ~ 3,
        str_detect(trait, "Dogs") ~ 5,
        str_detect(trait, "Children") ~ 4,
        str_detect(trait, "Energy") ~ 8,
        str_detect(trait, "Strangers") ~ 6,
        str_detect(trait, "Playfulness") ~ 9,
        str_detect(trait, "Trainability") ~ 2,
        str_detect(trait, "Nature") ~ 7,
        TRUE ~ 0),
    trait = 
      case_when(
        str_detect(trait, "Adaptability") ~ "Adaptable",
        str_detect(trait, "Affectionate") ~ "Affectionate",
        str_detect(trait, "Dogs") ~ "Dog-friendly",
        str_detect(trait, "Children") ~ "Kid-friendly",
        str_detect(trait, "Mental") ~ "Intelligence",
        str_detect(trait, "Strangers") ~ "Stranger-friendly",
        str_detect(trait, "Playfulness") ~ "Playfulness",
        str_detect(trait, "Trainability") ~ "Trainable",
        str_detect(trait, "Nature") ~ "Protective",
        TRUE ~ trait),
  ) %>% 
  ggplot(
    aes(
      x = reorder(trait, trait_order), 
      y = reorder(breed, -mean_rank), 
      fill = value)) +
  geom_tile() +
  coord_equal(expand = T) +
  labs(x = NULL, y = "Dog Breed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, family = "mono", size = 7),
        axis.text.y = element_text(family = "mono"),
        axis.title.y = element_text(family = "mono"),
        legend.text = element_text(family = "mono", size = 7),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "vertical",
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_fill_viridis(
    labels = c("1 = Low (bad)", "2", "3", "4", "5 = High (good)"),
    discrete = T) + 
  guides(fill = guide_legend(reverse = TRUE))

# Top 101 (bad things)

bad101 <- 
  traits %>% 
  arrange(mean_rank) %>% 
  filter(mean_rank < 99) %>% 
  filter(str_detect(trait, "Grooming") | 
           #str_detect(trait, "Mental") |
           str_detect(trait, "Shedding") |
           str_detect(trait, "Drooling") |
           str_detect(trait, "Barking")) %>% 
  mutate(
    trait_order = 
      case_when(
        str_detect(trait, "Grooming") ~ 1,
        str_detect(trait, "Shedding") ~ 2,
        str_detect(trait, "Barking") ~ 3,
        str_detect(trait, "Drooling") ~ 4,
        TRUE ~ 0),
    trait = 
      case_when(
        str_detect(trait, "Barking") ~ "Barking",
        str_detect(trait, "Grooming") ~ "Grooming",
        str_detect(trait, "Drooling") ~ "         Drooling",
        str_detect(trait, "Shedding") ~ "Shedding",
        TRUE ~ trait),
  ) %>% 
  ggplot(
    aes(
      x = reorder(trait, trait_order), 
      y = reorder(breed, -mean_rank), 
      fill = value)) +
  geom_tile() +
  coord_equal(expand = T) +
  labs(x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, family = "mono", size = 7),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(family = "mono", size = 7),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "vertical",
    plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_fill_viridis(
    labels = c("1 = Low (good)", "2", "3", "4", "5 = High (bad)"), 
    direction = -1,
    discrete = T) 

# Combined top 20
plot_row101 <- 
  cowplot::plot_grid(good101, bad101, rel_widths = c(3, 1)) 

title101 <- 
  cowplot::ggdraw() + 
  cowplot::draw_label(
    "Traits of the top 101 AKC dog breeds\nby registration (2013-2020)",
    fontface = "bold",
    fontfamily = "mono",
    x = .5,
    hjust = .5) +
  theme(plot.margin = margin(0, 0, 0, 7))

final101 <- cowplot::plot_grid(title101, plot_row101, ncol = 1, rel_heights = c(0.1, 1)) 
  
  ggsave(
    plot = final101,
    filename = "2022_dogs.png",
    #device = "png",
    path = "H:/r_projects/tidy_tuesday/output/",
    width = 6,
    height = 16,
    units = "in")

