library(here)
library(tidyverse)
library(RRPP)
library(ggpubr)
library(ggfortify)
library(cluster)
library(wesanderson)
library(ggExtra)

# data
data <- read.csv("qdata-temporal.csv")

# define variables
maxl <- data$maxl # maximum length
maxw <- data$maxw # maximum width
maxth <- data$maxth # maximum thickness
maxstl <- data$maxstl # maximum stem length
maxstw <- data$maxstw # maximum stem width
site <- data$site # site

# size standardize vars
data$ss.maxw <- maxl/maxw
data$ss.maxth <- maxl/maxth
data$ss.maxstl <- maxl/maxstl
data$ss.maxstw <- maxl/maxstw

ggplot(data = data,
       aes(x = maxw,
           y = maxth,
           colour = site)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Width") + ylab("Stem Width")

  
  ## Principal Components Analysis
  df<-data[c(9:12)]
  pch.gps.gp <- c(15,17)[as.factor(site)]
  col.gps.gp <- wes_palette("Moonrise2")[as.factor(site)]
  
  ## pca plot
  pca <- autoplot(prcomp(df),
                  data = data,
                  asp = 1,
                  shape = pch.gps.gp,
                  colour = "site",
                  variance_percentage = TRUE,
                  loadings = TRUE, 
                  loadings.colour = 'blue',
                  loadings.label = TRUE,
                  loadings.label.size = 3,
                  frame = TRUE,
                  frame.type = 't') +
    scale_fill_manual(values = wes_palette("Moonrise2")) +
    scale_colour_manual(values = wes_palette("Moonrise2"))
  
  ggMarginal(pca, groupColour = TRUE)

  
  ## Analyses of Variance (ANOVA) for _variable_ ~ _site_
  
  ### _Maximum width_ of Perdiz arrow points
    # anova = maximum width ~ site
  sitemw <- lm.rrpp(maxw ~ site, 
                      SS.type = "I", 
                      data = data, 
                      iter = 9999, 
                      print.progress = FALSE)
  anova(sitemw)

  ### _Maximum thickness_ of Perdiz arrow points
  # anova = maximum thickness ~ site
  sitemth <- lm.rrpp(maxth ~ site, 
                       SS.type = "I", 
                       data = data, 
                       iter = 9999, 
                       print.progress = FALSE)
  anova(sitemth)

  ### _Maximum stem length_ of Perdiz arrow points
    # anova = maximum stem length ~ site
  sitemstl <- lm.rrpp(maxstl ~ site, 
                        SS.type = "I", 
                        data = data, 
                        iter = 9999, 
                        print.progress = FALSE)
  anova(sitemstl)

  ### _Maximum stem width_ of Perdiz arrow points
    # anova = maximum stem width ~ site
  sitemstw <- lm.rrpp(maxstw ~ site, 
                        SS.type = "I", 
                        data = data, 
                        iter = 9999, 
                        print.progress = FALSE)
  anova(sitemstw)

# plots (change between geom_histogram/density)
maxl <- ggplot(data, aes(x = maxl, fill = group)) +
  geom_density(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

maxw <- ggplot(data, aes(x = maxw, fill = group)) +
  geom_density(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

maxstl <- ggplot(data, aes(x = maxstl, fill = group)) +
  geom_density(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

maxstw <- ggplot(data, aes(x = maxstw, fill = group)) +
  geom_density(position = "identity", alpha = 0.4) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme(legend.position = "none")

## render figure
figure <- ggarrange(maxl,maxw,maxstl,maxstw,
                    labels = c("a","b","c","d"),
                    ncol = 2, nrow = 2)

# plot figure
figure
