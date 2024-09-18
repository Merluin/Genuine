GenuineDetection <- function(data) {
  
  data <- data %>%
    mutate(sdt = case_when( 
      File.elicitation == "genuine" & AED.resp == "v" ~ "Hits",
      File.elicitation == "genuine" & AED.resp == "n" ~ "Misses",
      File.elicitation != "genuine" & AED.resp == "v" ~ "False.Alarms",
      File.elicitation != "genuine" & AED.resp == "n" ~ "Correct.Rejections"), 
      count = 1)%>%
    group_by(Pt.id, File.emotion,sdt) %>%
    summarise(count = sum(count), .Groups = 'drop') %>%
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
