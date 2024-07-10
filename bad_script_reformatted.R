# Reworked script 

# Introduction ----
# Refroamtted script for analysis of vertebrate population change based on the Living Planet Index 
# script from codingclub github avilable at: https://github.com/candela-ferrer-diez/Coding-session-3-etiquette/tree/master
# Data available from http://www.livingplanetindex.org/

# Candela Ferrer Diez
# 10-07-2024

# Libraries ----
library(tidyr)  # Formatting data for analysis
library(dplyr)  # Manipulating data
library(ggplot2)  # Visualising results
library(readr)  # Manipulating data
library (RCurl)

#Functions ----
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

#Analysis ----
# Set working directory 
setwd("C:/Users/Candela/OneDrive - Nexus365/Biology - Yr 2/Intro_to_R/Session 3")

# Import data ----
#note: the data must be pre-saved in the wroking directory
LPI <- read.csv("LPIdata_CC.csv")

# Clean data ----

# long format, so abundance records in one col.
LPI_long <- gather(LPI, "year", "abundance", 9:53)  

# Force years to numeric
LPI_long$year <- parse_number(LPI_long$year)

# Check names and make consistent case
names(LPI_long)
names(LPI_long) <- tolower(names(LPI_long))

# Force abundance to numeric
LPI_long$abundance <- as.numeric(LPI_long$abundance)

# Create summary data frame of population `n`
LPI_biome_summ <- LPI_long %>% 
  group_by(biome) %>%
  summarise(population = n())

# Check data frame has formed correctly
head(LPI_biome_summ)

# Create plots of population frequency per biome ----

# bar graph
type = "bar"
biome_barplot <- ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type == "bar") geom_bar() else geom_point(stat = "count")
} +
  theme.LPI() + 
  ylab("Number of populations") + 
  xlab("Biome") +
  theme(legend.position = "none")

# dot plot
type = "point"
biome_dotplot <- ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type == "bar") geom_bar() else geom_point(stat = "count")
} +
  theme.LPI() + 
  ylab("Number of populations") + 
  xlab("Biome") +
  theme(legend.position = "none")

# Compare plots
biome_barplot
biome_dotplot

# Bar graph looks better, save to pdf
type = "bar"
pdf(file="img/biome_freq_barplot.pdf",  width = 13.33, height = 26.66)
ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type == "bar") geom_bar() else geom_point(stat = "count")
} +
  theme.LPI() + 
  ylab("Number of populations") + 
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()