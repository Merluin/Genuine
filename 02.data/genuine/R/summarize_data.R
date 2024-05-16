summarize_data <- function(df) {
  summarized_df <- df %>%
    summarise(
      genuine.acc.mean = mean(gesino_kb.corr, na.rm = TRUE),
      genuine.acc.sd = sd(gesino_kb.corr, na.rm = TRUE),
      genuine.rt.mean = mean(gesino_kb.rt, na.rm = TRUE),
      genuine.rt1.mean = mean(gesino_kb.rt[gesino_kb.corr == 1], na.rm = TRUE),
      genuine.rt1.sd = sd(gesino_kb.rt[gesino_kb.corr == 1], na.rm = TRUE),
      genuine.rt0.mean = mean(gesino_kb.rt[gesino_kb.corr == 0], na.rm = TRUE),
      genuine.rt0.sd = sd(gesino_kb.rt[gesino_kb.corr == 0], na.rm = TRUE),
      genuine.slider1.mean = mean(genuine_slider.response[gesino_kb.corr == 1], na.rm = TRUE),
      genuine.slider1.sd = sd(genuine_slider.response[gesino_kb.corr == 1], na.rm = TRUE),
      genuine.slider0.mean = mean(genuine_slider.response[gesino_kb.corr == 0], na.rm = TRUE),
      genuine.slider0.sd = sd(genuine_slider.response[gesino_kb.corr == 0], na.rm = TRUE),
      emotion.acc.mean = mean(accuracy, na.rm = TRUE),
      emotion.acc.sd = sd(accuracy, na.rm = TRUE),
      emotion.slider1.mean = mean(intensity_slider.response[accuracy == 1], na.rm = TRUE),
      emotion.slider1.sd = sd(intensity_slider.response[accuracy == 1], na.rm = TRUE),
      emotion.slider0.mean = mean(intensity_slider.response[accuracy == 0], na.rm = TRUE),
      emotion.slider0.sd = sd(intensity_slider.response[accuracy == 0], na.rm = TRUE)
    ) %>%
    ungroup()%>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  return(summarized_df)
}
