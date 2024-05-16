GenuineDetection <- function(data) {
  
  data <- data %>%
    mutate(sdt = case_when( # gesino_kb.keys is participant aswer emotion is genuine: v = TRUE, n = FALSE, elicitation is quality of the video
      elicitation == "genuine" & gesino_kb.keys == "v" ~ "Hits",
      elicitation == "genuine" & gesino_kb.keys == "n" ~ "Misses",
      elicitation != "genuine" & gesino_kb.keys == "v" ~ "False.Alarms",
      elicitation != "genuine" & gesino_kb.keys == "n" ~ "Correct.Rejections"), 
      count = 1)%>%
    group_by(participant, emotion,session,group,sdt) %>%
    summarise(count = sum(count), .groups = 'drop') %>%
    pivot_wider(names_from = sdt, values_from = count, values_fill = list(count = 0)) %>%
     mutate(HR = Hits / (Hits + Misses),
            FAR = False.Alarms / (False.Alarms + Correct.Rejections),
             HR = ifelse(HR == 1, 1 - 1/(2 * (Hits + Misses)), ifelse(HR == 0, 1/(2 * (Hits + Misses)), HR)),
             FAR = ifelse(FAR == 1, 1 - 1/(2 * (False.Alarms + Correct.Rejections)), ifelse(FAR == 0, 1/(2 * (False.Alarms + Correct.Rejections)), FAR)),
            ZHR = qnorm(HR),
            ZFAR = qnorm(FAR),
            d_prime = ZHR - ZFAR,
            c = -0.5 * (ZHR + ZFAR))
  
  return(data)
}
