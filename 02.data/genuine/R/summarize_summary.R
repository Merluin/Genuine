summarize_summary <- function(df) {
  summarized_df <- df %>%
    summarise(
      genuine.acc.sd = sd(genuine.acc.mean, na.rm = TRUE),
      genuine.acc.mean = mean(genuine.acc.mean, na.rm = TRUE),
      genuine.rt1.sd = sd(genuine.rt1.mean, na.rm = TRUE),
      genuine.rt.mean = mean(genuine.rt.mean, na.rm = TRUE),
      genuine.rt1.mean = mean(genuine.rt1.mean, na.rm = TRUE),
      genuine.rt0.sd = sd(genuine.rt0.mean, na.rm = TRUE),
      genuine.rt0.mean = mean(genuine.rt0.mean, na.rm = TRUE),
      genuine.slider1.sd = sd(genuine.slider1.mean, na.rm = TRUE),
      genuine.slider1.mean = mean(genuine.slider1.mean, na.rm = TRUE),
      genuine.slider0.sd = sd(genuine.slider0.mean, na.rm = TRUE),
      genuine.slider0.mean = mean(genuine.slider0.mean, na.rm = TRUE),
      emotion.acc.sd = sd(emotion.acc.mean, na.rm = TRUE),
      emotion.acc.mean = mean(emotion.acc.mean, na.rm = TRUE),
      emotion.slider1.sd = sd(emotion.slider1.mean, na.rm = TRUE),
      emotion.slider1.mean = mean(emotion.slider1.mean, na.rm = TRUE),
      emotion.slider0.sd = sd(emotion.slider0.mean, na.rm = TRUE),
      emotion.slider0.mean = mean(emotion.slider0.mean, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  return(summarized_df)
}
