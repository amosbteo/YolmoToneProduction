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
plot_boxplot_f0_rime50 <- function(df, toneanalysis = "Tone2", syllposition = "") {
  df.50 <- subset_f0_rime50(df)
  syllposition <- paste(syllposition)
  if (toneanalysis == "Tone4") {
  df.plot <- ggplot(data = df.50, aes(x = get(toneanalysis), y = F0, fill = Gender)) +
    geom_boxplot() +
    facet_wrap(~Gender) +
    theme_bw() +
    labs(x = "Tone category", y = "F0 (Hz)") +
    ggtitle(paste0("F0 at rime midpoint of ", syllposition)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(breaks=c("T1", "T2", "T3", "T4"),
                   labels=c("Tone 1", "Tone 2", "Tone 3", "Tone 4"))
    }    else  {
    df.plot <- ggplot(data = df.50, aes(x = get(toneanalysis), y = F0, fill = Gender)) +
      geom_boxplot() +
      facet_wrap(~Gender) +
      theme_bw() +
      labs(x = "Tone category", y = "F0 (Hz)") +
      ggtitle(paste0("F0 at rime midpoint of ", syllposition)) +
      theme(plot.title = element_text(hjust = 0.5))
    }
  # Preview plot
  df.plot
}


# Save boxplots to "figures" sub-folder
save_boxplot_f0_rime50_png <- function(df, toneanalysis = "Tone2", syllposition = "", filename = "default") {
  filename <- paste("figures/", filename, ".png", sep = "")
  #ggsave(filename = zz, plot = p, width = 10, height = 7, dpi = 100)
  png(filename = filename, width = 600, height = 450, res = 72)
  print(plot_boxplot_f0_rime50(df, toneanalysis, syllposition))
  dev.off()
}

# Summarize f0 at across rime time (1% intervals)
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
    scale_y_continuous(limits = c(100, 250), 
                     breaks = c(100,125,150,175,200,225,250)) + #Set y range + ticks
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


# Plot gender-normalized F0 (z-score) (normalized duration)
normalize_byfactor_f0 <- function(df, factor = "Gender") {
  df.z <- by(data = df$F0, df[,factor], scale)
  df$z <- do.call(rbind, df.z)
  df
  } 


# Get summary of z-scores across rime in a carrier phrase
summarize_z_across_time_norm <- function(df, toneanalysis = "Tone2"){
  toneanalysis1 <- paste(toneanalysis)
  df.summ <- summarySE(df, "z", groupvars=c(toneanalysis1, "RimeTime"), na.rm=TRUE, conf.interval=0.95, .drop=TRUE)
  df.summ$Time <- as.integer(df.summ$RimeTime)  # Make sure time is an integer, not a factor
  df.summ
}

# Plot z-scores across rime for each tone category
plot_z_across_time_norm <- function(df, toneanalysis = "Tone2",  syllposition = "monosyllabic words") {
  pd <- position_dodge(5)
  df.summ <- summarize_z_across_time_norm(df, toneanalysis)
  syllposition <- paste(syllposition)
  RimeTime1 <- "RimeTime"
  z1 <- "z"
  df.f0.traj <- ggplot(df.summ, aes_string(x = RimeTime1, y = z1, colour = toneanalysis, group = toneanalysis)) + #Use x=Time for equal time
    geom_line(aes_string(linetype = toneanalysis), size=1.25, position=pd) + #Line type depends on Tone.
    geom_point(data=subset(df.summ, (RimeTime==1|RimeTime==20|RimeTime==40|RimeTime==60|RimeTime==80|RimeTime==100)), aes_string(shape = toneanalysis), size = 5, position = pd) + #Only show the points for a subset of the data
    geom_errorbar(data = subset(df.summ, (RimeTime==1|RimeTime==20|RimeTime==40|RimeTime==60|RimeTime==80|RimeTime==100)), aes(ymin = z-sd/2, ymax = z+sd/2), width = 1.0, position = pd) + #Only show half the NS for a subset of the data
    scale_x_continuous(limits = c(-1,105),
                       breaks = c(0,20,40,60,80,100)) + # Set x range + ticks
    labs(x = "Time (%)", y = "z-score") +
    ggtitle(paste("z-score across time-normalized rime of", syllposition)) +
    theme_bw() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 15)) + #Adjust font size of axes
    theme(legend.title = element_text(size = 12),
          legend.text = element_text(size = 11)) + #Adjust size of legend
    theme(plot.title = element_text(size = 16)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_colour_discrete(name = "Tone Category", l=50) + # Use darker colors, lightness=50 
    scale_linetype_discrete(name = "Tone Category") +
    scale_shape_discrete(name = "Tone Category")
  # Preview plot
  df.f0.traj
}

# Save z plots to "figures" sub-folder
save_plot_z_across_time_norm_png <- function(df, toneanalysis = "Tone2", syllposition = "", filename = "default") {
  filename <- paste("figures/", filename, ".png", sep = "")
  #ggsave(filename = zz, plot = p, width = 10, height = 7, dpi = 100)
  png(filename = filename, width = 600, height = 450, res = 72)
  print(plot_z_across_time_norm(df, toneanalysis, syllposition))
  dev.off()
}
