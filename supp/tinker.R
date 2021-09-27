library(here)
library(tidyverse)
library(RRPP)
library(ggpubr)
library(ggfortify)
library(cluster)
library(wesanderson)
library(ggExtra)

# data
data <- read.csv("qdata.csv")

# plots (change between geom_histogram/density)
maxl <- ggplot(data, aes(x = maxl, fill = region)) +
  geom_histogram(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

maxw <- ggplot(data, aes(x = maxw, fill = region)) +
  geom_histogram(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

maxstl <- ggplot(data, aes(x = maxstl, fill = region)) +
  geom_histogram(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

maxstw <- ggplot(data, aes(x = maxstw, fill = region)) +
  geom_histogram(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

## render figure
figure <- ggarrange(maxl,maxw,maxstl,maxstw,
                    labels = c("a","b","c","d"),
                    ncol = 2, nrow = 2)

# plot figure
figure
