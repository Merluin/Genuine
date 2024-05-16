AccuracyDetection <- function(data, emo) {
  
  data <- data %>%
    mutate(sdt = case_when(
      emotion == emo & emotion_ms.clicked_name == emo ~ "Hits",
      emotion == emo & emotion_ms.clicked_name != emo ~ "Misses",
      emotion != emo & emotion_ms.clicked_name == emo ~ "False.Alarms",
      emotion != emo & emotion_ms.clicked_name != emo ~ "Correct.Rejections"),
      count = 1)%>%
    group_by(participant, emotion,session,group,elicitation ,sdt) %>%
    summarise(count = sum(count), .groups = 'drop') %>%
    pivot_wider(names_from = sdt, values_from = count, values_fill = list(count = 0)) %>%
    mutate(HR = Hits / (Hits + Misses),
           FAR = False.Alarms / (False.Alarms + Correct.Rejections),
           HR = ifelse(HR == 1, 1 - 1/(2 * (Hits + Misses)), ifelse(HR == 0, 1/(2 * (Hits + Misses)), HR)),
           FAR = ifelse(FAR == 1, 1 - 1/(2 * (False.Alarms + Correct.Rejections)), ifelse(FAR == 0, 1/(2 * (False.Alarms + Correct.Rejections)), FAR))) %>%
    group_by(participant,session,group,elicitation) %>%
    summarise( HR = mean(HR,na.rm =TRUE),
               FAR = mean(FAR,na.rm =TRUE),
               .groups = 'drop') %>%
      mutate(ZHR = qnorm(HR),
             ZFAR = qnorm(FAR),
             d_prime = ZHR - ZFAR,
             c = -0.5 * (ZHR + ZFAR),
             emotion = emo)
  return(data)
}
