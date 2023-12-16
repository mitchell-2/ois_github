# benodigde libraries voor uitvoering van het script
library(psych)

# creëren subdatasets
data_boek <- subset(ois_data, `Methode?` == "boek")
data_gpt <- subset(ois_data, `Methode?` == "ProgrammeerGPT")

# creëren boxplots SUS waarden
sus_scores <- data.frame(
  leermethode = rep(c("ProgrammeerGPT", "Boek"), each = length(data_gpt$`SUS score`)),
  SUS = c(data_gpt$`SUS score`, data_boek$`SUS score`)
)

boxplot(SUS ~ leermethode, data = sus_scores, 
        col = 'lightyellow',border = "black",
        xlab = "Conditie (Leermethode)", ylab = "SUS score") 

# uitvoeren Welch Two Sample t-test
t.test(data_boek$`SUS score`,data_gpt$`SUS score`,var.equal=FALSE)

# berekenen standaard deviatie
sd(data_boek$`SUS score`)
sd(data_gpt$`SUS score`)

# berekenen mediaan
median(data_boek$`SUS score`)
median(data_gpt$`SUS score`)

# berekenen interkwartielafstand 
IQR(data_boek$`SUS score`)
IQR(data_gpt$`SUS score`)

# berekenen minimale waarden
min(data_boek$`SUS score`)
min(data_gpt$`SUS score`)

# berekenen maximale waarden
max(data_boek$`SUS score`)
max(data_gpt$`SUS score`)

# berekenen percentielen (25% en 75%)
quantile(data_boek$`SUS score`, 0.25)
quantile(data_boek$`SUS score`, 0.75)
quantile(data_gpt$`SUS score`, 0.25)
quantile(data_gpt$`SUS score`, 0.75)

# berekenen modus
calculate_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- table(x)
  mode_value <- unique_values[which.max(frequencies)]
  return(mode_value)
}
calculate_mode(data_boek$`SUS score`)
calculate_mode(data_gpt$`SUS score`)

# berekenen van de cohen's d
sd_boek <- sd(data_boek$`SUS score`)
sd_gpt <- sd(data_gpt$`SUS score`)
mean_boek <- mean(data_boek$`SUS score`)
mean_gpt <- mean(data_gpt$`SUS score`)
(mean_boek - mean_gpt) / sqrt((sd_boek^2 + sd_gpt^2) / 2)

# cronbach's alfa berekenen
subset_data_boek <- data_boek[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")]
alpha(subset_data_boek, check.keys=TRUE)
subset_data_gpt <- data_gpt[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")]
alpha(subset_data_gpt, check.keys=TRUE)

