library(Rmisc)
library(ggplot2)

# Function to subset data at 50% of rime
subset_f0_rime50 <- function(df) {
  # Only select midpoint data
  df <- subset(df, RimeTime == 50)
  df
}

# Function to summarize f0 at rime midpoint
summarize_f0_50 <- function(df) {
  df <- subset_f0_rime50(df)
  # Get summary by tone category of F0 and sd
  df.summ <- summarySE(df, "F0", groupvars=c("Tone4", "Gender"), na.rm = TRUE)
  df.summ
}

# Plot differences in F0 across tone categories conditions
save_boxplot_f0_rime50_png <- function(df, sylltype = "monosyllabic words") {
  df.50 <- subset_f0_rime50(df)
  sylltype <- paste (sylltype)
  df.plot <- ggplot(data = df.50, aes(x = Tone4, y = F0, fill = Gender)) +
    geom_boxplot() +
    facet_wrap(~Gender) +
    theme_bw() +
    labs(x = "Tone category", y = "F0 (Hz)") +
    ggtitle(paste0("F0 at rime midpoint of ", sylltype, " (all frames)")) +
    scale_x_discrete(breaks=c("T1", "T2", "T3", "T4"),
                   labels=c("Tone 1", "Tone 2", "Tone 3", "Tone 4")) +
    theme(plot.title = element_text(hjust = 0.5))
  # Preview plot
  df.plot
}

# Save boxplots to "figures" sub-folder
save_boxplot_f0_rime50_png <- function(df, sylltype = "monosyllabic words", filename = "default") {
  filename <- paste("figures/", filename, ".png", sep = "")
  #ggsave(filename = zz, plot = p, width = 10, height = 7, dpi = 100)
  png(filename = filename, width = 600, height = 450, res = 72)
  print(boxplot_f0_rime50(df, sylltype))
  dev.off()
  }
