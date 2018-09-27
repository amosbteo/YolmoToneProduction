library(Rmisc)
library(ggplot2)

# Function to subset data at 50% of rime
subset_f0_rime50 <- function(df) {
  # Only select midpoint data
  df <- subset(df, RimeTime == 50)
  df
}

# Function to summarize f0 at rime midpoint; default is a 2-tone analysis
summarize_f0_50 <- function(df, toneanalysis = "Tone2") {
  df <- subset_f0_rime50(df)
  toneanalysis <- paste(toneanalysis)
  # Get summary by tone category of F0 and sd
  df.summ <- summarySE(df, "F0", groupvars=c(toneanalysis, "Gender"), na.rm = TRUE)
  df.summ
}

# Plot differences in F0 across tone categories conditions
<<<<<<< HEAD
plot_boxplot_f0_rime50 <- function(df, toneanalysis = "Tone2", syllposition = "") {
  df.50 <- subset_f0_rime50(df)
  syllposition <- paste(syllposition)
=======
plot_boxplot_f0_rime50 <- function(df, toneanalysis = "Tone2", sylltype = "monosyllabic words") {
  df.50 <- subset_f0_rime50(df)
  sylltype <- paste(sylltype)
>>>>>>> f4dc5d6d54896550d973a80a3b0fbec9eb181fc8
  if (toneanalysis == "Tone4") {
  df.plot <- ggplot(data = df.50, aes(x = get(toneanalysis), y = F0, fill = Gender)) +
    geom_boxplot() +
    facet_wrap(~Gender) +
    theme_bw() +
    labs(x = "Tone category", y = "F0 (Hz)") +
<<<<<<< HEAD
    ggtitle(paste0("F0 at rime midpoint of ", syllposition)) +
=======
    ggtitle(paste0("F0 at rime midpoint of ", sylltype)) +
>>>>>>> f4dc5d6d54896550d973a80a3b0fbec9eb181fc8
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(breaks=c("T1", "T2", "T3", "T4"),
                   labels=c("Tone 1", "Tone 2", "Tone 3", "Tone 4"))
    }    else  {
    df.plot <- ggplot(data = df.50, aes(x = get(toneanalysis), y = F0, fill = Gender)) +
      geom_boxplot() +
      facet_wrap(~Gender) +
      theme_bw() +
      labs(x = "Tone category", y = "F0 (Hz)") +
<<<<<<< HEAD
      ggtitle(paste0("F0 at rime midpoint of ", syllposition)) +
=======
      ggtitle(paste0("F0 at rime midpoint of ", sylltype)) +
>>>>>>> f4dc5d6d54896550d973a80a3b0fbec9eb181fc8
      theme(plot.title = element_text(hjust = 0.5))
    }
  # Preview plot
  df.plot
}


# Save boxplots to "figures" sub-folder
<<<<<<< HEAD
save_boxplot_f0_rime50_png <- function(df, toneanalysis = "Tone2", syllposition = "", filename = "default") {
  filename <- paste("figures/", filename, ".png", sep = "")
  #ggsave(filename = zz, plot = p, width = 10, height = 7, dpi = 100)
  png(filename = filename, width = 600, height = 450, res = 72)
  print(plot_boxplot_f0_rime50(df, toneanalysis, syllposition))
  dev.off()
}

# Summarize f0 at each time interval
summarize_f0_across_time_norm <- function(df, toneanalysis = "Tone2") {
  # Get summary by tone category of F0 and sd
  toneanalysis1 <- paste(toneanalysis)
  df.summ <- summarySE(df, "F0", groupvars=c(toneanalysis1, "Gender", "RimeTime"), na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
  #df.summ$RimeTime <- df.summ$RimeTime/100
  df.summ$RimeTime <- as.integer(df.summ$RimeTime)  # Make sure time is an integer, not a factor
  df.summ$Factor <- paste(df.summ$Gender, get(toneanalysis, df.summ))  #Create "Factor" column for interaction between Gender and Tone Category
  df.summ
}

# Plot F0 across rime for each tone category
plot_f0_across_time_norm <- function(df, toneanalysis = "Tone2",  syllposition = "monosyllabic words") {
  pd <- position_dodge(5)
  df.summ <- summarize_f0_across_time_norm(df, toneanalysis)
  syllposition <- paste(syllposition)
  df.f0.traj <- ggplot(df.summ, aes(x = RimeTime, y = F0, colour = Factor, group = Factor)) + #Use x=Time for equal time
    geom_line(aes(linetype = Factor), size=1.25, position=pd) + #Line type depends on Tone.
    geom_point(data=subset(df.summ, (RimeTime==1|RimeTime==20|RimeTime==40|RimeTime==60|RimeTime==80|RimeTime==100)), aes(shape = Factor), size = 5, position = pd) + #Only show the points for a subset of the data
    geom_errorbar(data = subset(df.summ, (RimeTime==1|RimeTime==20|RimeTime==40|RimeTime==60|RimeTime==80|RimeTime==100)), aes(ymin = F0-sd/2, ymax = F0+sd/2), width = 1.0, position = pd) + #Only show half the NS for a subset of the data
    scale_x_continuous(limits = c(-1,105),
                     breaks = c(0,20,40,60,80,100)) + # Set x range + ticks
    scale_y_continuous(limits = c(100, 240), 
                     breaks = c(100,125,150,175,200,225)) + #Set y range + ticks
    labs(x = "Time (%)", y = "F0 (Hz)") +
    ggtitle(paste("F0 across time-normalized rime of", syllposition)) +
    theme_bw() +
    theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) + #Adjust font size of axes
    theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)) + #Adjust size of legend
    theme(plot.title = element_text(size = 16)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_colour_discrete(name = "Gender *\nTone Category", l=50) + # Use darker colors, lightness=50 
    scale_linetype_discrete(name = "Gender *\nTone Category") +
    scale_shape_discrete(name = "Gender *\nTone Category")
  # Preview plot
  df.f0.traj  
}

# Save F0 plots to "figures" sub-folder
save_plot_f0_across_time_norm_png <- function(df, toneanalysis = "Tone2", syllposition = "", filename = "default") {
  filename <- paste("figures/", filename, ".png", sep = "")
  #ggsave(filename = zz, plot = p, width = 10, height = 7, dpi = 100)
  png(filename = filename, width = 600, height = 450, res = 72)
  print(plot_f0_across_time_norm(df, toneanalysis, syllposition))
  dev.off()
}
=======
save_boxplot_f0_rime50_png <- function(df, toneanalysis = "Tone2", sylltype = "monosyllabic words", filename = "default") {
  filename <- paste("figures/", filename, ".png", sep = "")
  #ggsave(filename = zz, plot = p, width = 10, height = 7, dpi = 100)
  png(filename = filename, width = 600, height = 450, res = 72)
  print(plot_boxplot_f0_rime50(df, toneanalysis, sylltype))
  dev.off()
}

# Summarize f0 at each time interval
summarize_f0_across_time <- function(df, toneanalysis = "Tone2") {
  # Get summary by tone category of F0 and sd
  toneanalysis1 <- paste(toneanalysis)
  df.summ <- summarySE(df, "F0", groupvars=c(toneanalysis1, "Gender", "Time"), na.rm=TRUE, conf.interval=0.95, .drop=TRUE)
  #YolTone.f0.summ$Time <- YolTone.f0.summ$Time/100
  df.summ$Time <- as.integer(df.summ$Time)  # Make sure time is an integer, not a factor
  df.summ$Factor <- paste(df.summ$Gender, get(toneanalysis, df.summ))  #Create "Factor" column for interaction between Gender and Tone Category
  df.summ
}

# Plot F0 across rime for each tone category
plot_f0_across_time_normal <- function(df) {
pd <- position_dodge(5)
YolTone.f0.traj1 <- ggplot(YolTone.f0.summ, aes(x = Time, y = F0, colour = Factor, group = Factor)) + #Use x=Time for equal time
  geom_line(aes(linetype = Factor), size=1.25, position=pd) + #Line type depends on Tone.
  geom_point(data=subset(YolTone.f0.summ, (Time==1|Time==20|Time==40|Time==60|Time==80|Time==100)), aes(shape = Factor), size = 5, position = pd) + #Only show the points for a subset of the data
  geom_errorbar(data = subset(YolTone.f0.summ, (Time==1|Time==20|Time==40|Time==60|Time==80|Time==100)), aes(ymin = F0-sd/2, ymax = F0+sd/2), width = 1.0, position = pd) + #Only show half the NS for a subset of the data
  scale_x_continuous(limits = c(-1,105),
                     breaks = c(0,20,40,60,80,100)) + # Set x range + ticks
  scale_y_continuous(limits = c(100, 240), 
                     breaks = c(100,125,150,175,200,225)) + #Set y range + ticks
  labs(x = "Time (%)", y = "F0 (Hz)", title = expression(paste(italic("f0"), "across time-normalized rime (monosyllable, all frames)"))) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) + #Adjust font size of axes
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)) + #Adjust size of legend
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name = "Gender *\nTone Category", l=50) + # Use darker colors, lightness=50 
  scale_linetype_discrete(name = "Gender *\nTone Category") +
  scale_shape_discrete(name = "Gender *\nTone Category")
  
  # Preview plot
  YolTone.f0.traj1  
}
>>>>>>> f4dc5d6d54896550d973a80a3b0fbec9eb181fc8
