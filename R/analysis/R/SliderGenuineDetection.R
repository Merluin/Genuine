SliderGenuineDetection <- function(data) {
  
  data <- data %>%
    mutate(sdt = case_when( # gesino_kb.keys is participant aswer emotion is genuine: v = TRUE, n = FALSE, elicitation is quality of the video
      elicitation == "genuine" & gesino_kb.keys == "v" ~ "Hits",
      elicitation == "genuine" & gesino_kb.keys == "n" ~ "Misses",
      elicitation != "genuine" & gesino_kb.keys == "v" ~ "False.Alarms",
      elicitation != "genuine" & gesino_kb.keys == "n" ~ "Correct.Rejections"),
      count = 1)%>%
    group_by(participant, emotion,session,group,sdt) %>%
    summarise(slider = mean(abs(genuine_slider.response), na.rm = TRUE),
              .groups = 'drop')  %>%
    pivot_wider(names_from = sdt, values_from = slider)%>%
    ungroup() %>%
    mutate(across(where(is.numeric), ~ {
      mean_val <- mean(., na.rm = TRUE)
      if(mean_val == 0) {
        replace_na(., median(., na.rm = TRUE))
      } else {
        replace_na(., mean_val)
      }
    })) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  return(data)
}