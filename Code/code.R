### Densities by visit ### 

library(haven)
library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(broom)
library(stringr)
library(gridExtra)
library(grid)
library(ggpubr)
library(zoo)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")




### Load Dataset ###

dataset$Visit <- factor(dataset$Visit, levels = unique(dataset$Visit[order(dataset$Visit_nbr)])) 

freq_visit <- dataset %>% 
  group_by(Visit, Visit_nbr) %>% 
  dplyr::summarise(n = n(), 
                   nbr = sum(freq_cat)) %>% 
  mutate(Param = paste(n, "\n", nbr, " (", round(nbr/n*100, 1), "%)*", sep = ""), 
         TRT = "OVERALL")


data_visit <- dataset %>% 
  group_by(Visit) %>% 
  filter(!duplicated(Visit)) %>% 
  select(Visit, Visit_nbr, target, low, high) %>% 
  mutate(order = as.integer(factor(Visit)), 
         center =  0,
         label_visit = paste(gsub(".*-","",Visit), "\n", target, " (", low, ";+", high, ")", sep = "")) %>% 
  arrange(Visit_nbr)

data_visit$Visit <- factor(data_visit$Visit, levels = unique(data_visit$Visit[order(data_visit$Visit_nbr)]))


sum_cal <- dataset %>% 
  group_by(TRT, Visit, Visit_nbr) %>% 
  dplyr::summarise(mean = round(mean(Value, na.rm = TRUE), 1), 
                   sd = round(sd(Value, na.rm = TRUE), 2),
                   n = n()) %>% 
  arrange(Visit_nbr) %>% 
  mutate(Param = paste(n, " \n", sprintf("%.1f", mean), " (", sprintf("%.2f",sd), ")'", sep = "")) %>% 
  ungroup() %>% 
  bind_rows(freq_visit) %>% 
  mutate(TRT = ifelse(TRT == "OVERALL", paste(TRT, " \n", sep = ""), paste(TRT, " \n", sep = "")))

sum_cal$TRT <- factor(sum_cal$TRT, levels = c("Treatment B \n", "Treatment A \n", "OVERALL \n"))


titles <- c("Variable A: Distribution of days difference between the targetted visit day and the actual assessment day by planned visit")
footnotes <- c("*: Total number of observations and total number of observations in the range and associated frequency \n': Number of observations and associated mean and standard deviation")


g <- ggplot(data = dataset, aes(y = dif_day, x = Visit, fill = Visit)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = dif_day, color = Visit), position = position_jitter(width = .15, height = 0), size = .5, alpha = 0.8) +
  geom_pointrange(data = data_visit, aes(x = Visit, y = center,  ymin = low, ymax = high)) +
  #geom_text(data = freq_visit, aes(y = -55, label = vis_fre, col = Visit), size = 3) +
  geom_hline(yintercept = 0, alpha = 0.4, linetype = "dashed") +
  scale_x_discrete(labels = data_visit$label_visit) +
  scale_y_continuous(limits = c(-42, 65), breaks = seq(-40, 60, 10)) +
  guides(colour = "none", fill = "none") + 
  xlab("") + 
  ylab("Days difference") + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        plot.caption = element_text(hjust = 0), 
        axis.title.y = element_text(vjust = -15), 
        plot.margin =  unit(c(0.5,0.5,-0.3,0.5), "lines"))

t <- ggplot(sum_cal, aes(x = Visit, y = TRT, label = Param, col = Visit)) + 
  geom_text(size = 3) + 
  guides(color = "none") +
  theme_void() + 
  theme(axis.text.y = element_text(size = 9, hjust = 0.95), 
        plot.margin =  unit(c(-0.3,0,0.1,0), "lines"))

c <- ggpubr::ggarrange(g, t, ncol = 1, nrow = 2, heights = c(17, 3), align = "v")        

grid.arrange(c, bottom = textGrob(footnotes,
                                  x = 0.01, hjust = 0, gp = gpar(fontface = 3L, fontsize = 10)),
             top = textGrob(titles,
                            x = 0.01, hjust = 0, gp = gpar(fontface = 3L, fontsize = 13))
)     
