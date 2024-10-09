# READ IN DATA 

library(readxl)
project_sheet <- read_excel("project sheet.xlsx")
View(project_sheet)

# MAIN PLOT AND MODEL 

colors = c("green", "#A9A9A9","orange", "blue", "red")
plot(project_sheet$Ranking, project_sheet$`ESPN NEXT YR RANKING`, pch = 19, col = colors[factor(project_sheet$Position)], xlab = "Seasonal Win Share Ranking", ylab = "Seasonal ESPN Top 100 Ranking")
legend("topleft", legend = c("C", "PF", "PG", "SF", "SG"), pch = 19, col = colors)
center = ifelse(project_sheet$Position == "C", 1, 0)
plot3 = lm(`ESPN NEXT YR RANKING` ~ Ranking + center, data = project_sheet)  
summary(plot3)
abline(plot3)


# WS-BPM TESTING

plot(project_sheet$WS_BPM, project_sheet$`ESPN NEXT YR RANKING`, pch = 19, col = colors[factor(project_sheet$Position)], xlab = "Seasonal Win Share Ranking", ylab = "Seasonal ESPN Top 100 Ranking")
legend("topleft", legend = c("C", "PF", "PG", "SF", "SG"), pch = 19, col = colors)
plot5 = lm(`ESPN NEXT YR RANKING` ~ WS_BPM + center, data = project_sheet)
summary(plot5)


# MODEL FOR PREDICTING ESPN RANKING

plot4 = lm(`ESPN NEXT YR RANKING` ~ USG_PCT + BPM + `3PA_PCT`+ WS_BPM + PER + GP, data = project_sheet)  
summary(plot4)


# BPM PLOT AND MODEL

plot(project_sheet$BPM, project_sheet$`ESPN NEXT YR RANKING`, pch = 19, col = colors[factor(project_sheet$Position)], xlab = "BPM", ylab = "ESPN")
plot6 = lm(`ESPN NEXT YR RANKING` ~ BPM + center, data = project_sheet)
summary(plot6)
legend("topright", legend = c("C", "PF", "PG", "SF", "SG"), pch = 19, col = colors)
abline(plot6)










