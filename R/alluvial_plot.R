library(tidyverse)
library(ggalluvial)
library(viridis)
library(ggsci)
library(extrafont)

font_import()
loadfonts(device="win") # for times new roman

dat_lmtp <- read_rds(here::here("data/derived/dat_final.rds")) 

dat_lmtp %>%
  select(id, event, cr, starts_with("I_")) %>%
  pivot_longer(cols = starts_with("I_")) %>%
  mutate(day = parse_number(name),
    status = case_when(value == 0 ~ "No Supp O2",
                       value == 1 ~ "Supp O2",
                       value == 2 ~ "Intubated",
                       event == 1 ~ "AKI",
                       cr == 1 ~ "Deceased",
                       TRUE ~ "Discharged"
                       )) %>%
  group_by(day, status)

outcome_day <- 14
padded_days <- str_pad(0:(outcome_day-1), 2, pad = "0")

vars_to_group <- paste0("I_",padded_days)

# JAMA color scheme
type_colors <- c("#374E55", # gray
                 "#79AF97", # green
                 "#DF8F44", #orange
                  "#00A1D5", #blue
                 "#B24745", #red
                 "#6A6599" # purple  
                 ) 

alluv <-
  dat_lmtp %>%
  select(id, event, cr, starts_with("I_")) %>%
  mutate(across(starts_with("I_"),
                            ~ case_when(
                            .x == 0 ~ "No supplemental oxygen",
                            .x == 1 ~ "Non-IMV supplemental oxygen",
                            .x == 2 ~ "IMV",
                            event == 1 ~ "AKI",
                            cr == 1 ~ "Deceased",
                            TRUE ~ "Discharged"
         ))) %>%
  group_by_at(vars_to_group) %>%
  count() %>%
  to_lodes_form(key = "day", axes = 1:outcome_day) %>%
  mutate(day = factor(parse_number(as.character(day))+1),
        stratum = fct_relevel(stratum,
                                      "No supplemental oxygen",
                                      "Non-IMV supplemental oxygen",
                                      "IMV", 
                                      "AKI","Deceased","Discharged")) %>%
  # begin alluvial plot code
  ggplot(
    aes(x = day,
        stratum = stratum, 
        col = stratum,
        alluvium = alluvium,
        y=n,
        fill = stratum)
  ) +
  geom_flow() +
  geom_stratum() +
  scale_fill_manual(values = type_colors) +
  scale_color_manual(values = type_colors) +
  theme_classic() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Study Day", y = "Number of Patients", title = "Number of Patients per Exposure-Outcome Status by Day",
       fill = "Status", col = "Status") +
  theme(text=element_text(family="Times", size=11),
        legend.text = element_text(
          size=9))
        
alluv

ggsave( "graphs/figure_alluvial.pdf", alluv, width=8.5, height=4.7)
