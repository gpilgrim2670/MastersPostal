library(dplyr)
library(ggplot2)

Postal <- read.csv("Postal.csv")

facet_labs <- c("Women", "Men")
names(facet_labs) <- c("F", "M")

Postal %>%
  filter(Year == 2020) %>%
  ggplot(aes(y = Distance, x = Age_Group)) +
  geom_boxplot(aes(fill = as.factor(Gender)), alpha = 0.6, position = position_dodge(1), outlier.shape = NA) +
  geom_dotplot(
    aes(color =  as.factor(Gender), fill = as.factor(Gender)),
    binaxis = "y",
    stackdir = 'center',
    position = position_dodge(1),
    binwidth = 25,
    dotsize = 3
  ) +
  #geom_jitter(position = position_jitter(width = 0.1), alpha = 0.5) +
  theme_bw() +
  labs(x = "Age Group", y = "Distance (y)",
       title = "2020 US Masters ePostal Results") +
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold")) +
  theme(
    # legend.position = "bottom",
    # legend.key.size = unit(3, "line"),
    axis.text.x  = element_text(angle = 45, vjust = 0.5),
  ) +
  scale_fill_manual(
    values = c("F" = "maroon", "M" = "seagreen"),
    labels = c("F" = "Female", "M" = "Male")
  ) +
  scale_color_manual(values = c("F" = "maroon", "M" = "seagreen")) +
  guides(color = FALSE, alpha = FALSE, fill = FALSE) +
  facet_wrap(.~Gender, labeller =  labeller(Gender = facet_labs))
