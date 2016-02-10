## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)


## READ IN DATA AND ORGANIZE ####
# Read in data
data = read.table("rcourse_lesson1/data/rcourse_lesson1_data.txt", header=T, sep="\t")

# Look at dimension of data
dim(data)
# Look at first few rows of data
head(data)
# Look at final few rows of data
tail(data)
# Look at number of data points in each group
xtabs(~group, data)

# Subset out bilinguals
data_bl = data %>%
  filter(group == "bilingual")

# Look at make-up of data (dimension, first and final few rows)
dim(data_bl)
head(data_bl)
tail(data_bl)

# Look at number of data ponits by 1) group and 2) type for just bilinguals
xtabs(~group, data_bl)
xtabs(~type, data_bl)


## MAKE FIGURES ####
# By group
data.plot = ggplot(data, aes(x = group, y = rt)) +
  # Make the figure a boxplot, fill says to what the color should correspond to,
  # here it is the same as the x variable
  geom_boxplot(aes(fill = group)) +
  # Add a title
  ggtitle("Reaction Times by Group") +
  # Customize the x-axis label
  xlab("Group") +
  # Customize the y-axis label
  ylab("Reaction times in ms") +
  # These are extras to make the figure (in my opinion) prettier, look up each command to learn more
  theme_bw() + 
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

# Write figure to a pdf in the 'figures' folder
pdf("figures/data.pdf")
data.plot
dev.off()

# Within bilinguals by proficiency
data_bl.plot = ggplot(data_bl, aes(x = type, y = rt)) +
  # Make the figure a boxplot, fill says to what the color should correspond to,
  # here it is the same as the x variable
  geom_boxplot(aes(fill = type)) +
  # Add a title
  ggtitle("Reaction Times by L2 Proficiency Level") +
  # Customize the x-axis label
  xlab("Proficiency in L2") +
  # Customize the y-axis label
  ylab("Reaction times in ms") +
  # These are extras to make the figure (in my opinion) prettier, look up each command to learn more
  theme_bw() + 
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

pdf("rcourse_lesson1/figures/data_bl.pdf")
data_bl.plot
dev.off()

# Within bilinguals by proficiency with monolinguals in plot
data_blwml.plot = ggplot(data, aes(x = group, y = rt)) +
  geom_boxplot(aes(fill = type)) +
  ggtitle("Reaction Times by L2 Proficiency Level") +
  xlab("Proficiency in L2") +
  ylab("Reaction times in ms") +
  theme_bw() + 
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

pdf("rcourse_lesson1/figures/data_bl.pdf")
data_blwml.plot
dev.off()


## RUN DESCRIPTIVE STATISTICS ####
# Summarise data
data_sum = data %>%
  group_by(group) %>%
  summarise(rt_mean = mean(rt),
            rt_sd = sd(rt),
            rt_max = max(rt),
            rt_min = min(rt)) %>%
  ungroup()

data_sum

# Summarise data for bilinguals
data_bl_sum = data_bl %>%
  group_by(type) %>%
  summarise(rt_mean = mean(rt),
            rt_sd = sd(rt),
            rt_max = max(rt),
            rt_min = min(rt)) %>%
  ungroup()

data_bl_sum