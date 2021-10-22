library(here)
library(tidyverse)
library(RRPP)
library(ggpubr)
library(ggfortify)
library(cluster)
library(wesanderson)
library(ggExtra)

# data
data <- read.csv("qdata-sy27.csv")

# define variables
maxl <- data$maxl # maximum length
maxw <- data$maxw # maximum width
maxth <- data$maxth # maximum thickness
maxstl <- data$maxstl # maximum stem length
maxstw <- data$maxstw # maximum stem width
group <- data$group # site

ggplot(data = data,
       aes(x = maxw,
           y = maxstl,
           colour = group)) +
  geom_point(size = 2) +
  stat_ellipse(geom = "polygon",
               aes(fill = site),
               alpha = 0.05)
  xlab("Width") + ylab("Stem Width")

  
  ## Principal Components Analysis
  df<-data[c(4:8)]
  pch.gps.gp <- c(15:17)[as.factor(group)]
  col.gps.gp <- wes_palette("Moonrise2")[as.factor(group)]
  
  ## pca plot
  pca <- autoplot(prcomp(df),
                  data = data,
                  asp = 1,
                  shape = pch.gps.gp,
                  colour = "group",
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

  
  ## Analyses of Variance (ANOVA) for _variable_ ~ _region_
  
  ### _Maximum length_ of Perdiz arrow points
  
  ```{r anovaregionmaxl, out.width = "100%", dpi = 300, echo=TRUE}
  # anova = maximum length ~ region
  regionml <- lm.rrpp(maxl ~ group, 
                      SS.type = "I", 
                      data = data, iter = 9999, 
                      print.progress = FALSE)
  anova(regionml)
  ```
  
  ### _Maximum width_ of Perdiz arrow points
  
  ```{r anovaregionmaxw, out.width = "100%", dpi = 300, echo=TRUE}
  # anova = maximum width ~ region
  regionmw <- lm.rrpp(maxw ~ group, 
                      SS.type = "I", 
                      data = data, 
                      iter = 9999, 
                      print.progress = FALSE)
  anova(regionmw)
  ```
  
  ### _Maximum thickness_ of Perdiz arrow points
  
  ```{r anovaregionmaxth, out.width = "100%", dpi = 300, echo=TRUE}
  # anova = maximum thickness ~ region
  regionmth <- lm.rrpp(maxth ~ group, 
                       SS.type = "I", 
                       data = data, 
                       iter = 9999, 
                       print.progress = FALSE)
  anova(regionmth)
  ```
  
  ### _Maximum stem length_ of Perdiz arrow points
  
  ```{r anovaregionmaxstl, out.width = "100%", dpi = 300, echo=TRUE}
  # anova = maximum stem length ~ region
  regionmstl <- lm.rrpp(maxstl ~ group, 
                        SS.type = "I", 
                        data = data, 
                        iter = 9999, 
                        print.progress = FALSE)
  anova(regionmstl)
  ```
  
  ### _Maximum stem width_ of Perdiz arrow points
  
  ```{r anovaregionmaxstw, out.width = "100%", dpi = 300, echo=TRUE}
  # anova = maximum stem width ~ region
  regionmstw <- lm.rrpp(maxstw ~ group, 
                        SS.type = "I", 
                        data = data, 
                        iter = 9999, 
                        print.progress = FALSE)
  anova(regionmstw)
  ```  
  
  
  
  
  
# standardize vars
#perdiz_standardized <- as.data.frame(scale(data[4:8]))

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
