# READ IN DATA 

library(readxl)
project_sheet <- read_excel("project sheet.xlsx")
View(project_sheet)


# MAIN PLOT AND SETTING UP THE BASE MODEL 

colors = c("green", "#A9A9A9","orange", "blue", "red")
plot(project_sheet$Ranking + sf + sg + sf + pf, project_sheet$`ESPN NEXT YR RANKING`, pch = 19, col = colors[factor(project_sheet$Position)], xlab = "Seasonal Win Share Ranking", ylab = "Seasonal ESPN Top 100 Ranking")
legend("topleft", legend = c("C", "PF", "PG", "SF", "SG"), pch = 19, col = colors)
center = ifelse(project_sheet$Position == "C", 1, 0)
sf = ifelse(project_sheet$Position == "SF", 1, 0)
sg = ifelse(project_sheet$Position == "SG", 1, 0)
pg = ifelse(project_sheet$Position == "PG", 1, 0)
pf = ifelse(project_sheet$Position == "PF", 1, 0)


#TESTING CENTER SIGNIFICANCE

plot3 = lm(`ESPN NEXT YR RANKING` ~ Ranking + sf + sg + pg + pf, data = project_sheet)
abline(plot3)
summary(plot3)


#TESTING POWER FORWARD SIGNIFICANCE

plot8 = lm(`ESPN NEXT YR RANKING` ~ Ranking + center + sg + pg + sf, data = project_sheet) 
summary(plot8)


# WS-BPM TESTING

plot(project_sheet$WS_BPM, project_sheet$`ESPN NEXT YR RANKING`, pch = 19, col = colors[factor(project_sheet$Position)], xlab = "Seasonal Win Share Ranking", ylab = "Seasonal ESPN Top 100 Ranking")
legend("topleft", legend = c("C", "PF", "PG", "SF", "SG"), pch = 19, col = colors)
plot5 = lm(`ESPN NEXT YR RANKING` ~ WS_BPM + center, data = project_sheet)
summary(plot5)


# MODEL FOR PREDICTING ESPN RANKING

sg = ifelse(project_sheet$Position == "SG", 1, 0)
center = ifelse(project_sheet$Position == "C", 1, 0)
plot4 = lm(`ESPN NEXT YR RANKING` ~ USG_PCT + BPM + PER + `3PA_PCT` + WS_BPM + Age + GP + MP + Age * GP, data = project_sheet)  
summary(plot4)


# BPM PLOT AND MODEL

plot(project_sheet$BPM, project_sheet$`ESPN NEXT YR RANKING`, pch = 19, col = colors[factor(project_sheet$Position)], xlab = "BPM", ylab = "ESPN")
plot6 = lm(`ESPN NEXT YR RANKING` ~ BPM + center, data = project_sheet)
summary(plot6)
legend("topright", legend = c("C", "PF", "PG", "SF", "SG"), pch = 19, col = colors)
abline(plot6)


#TESTING GAMES PLAYED VERSUS MINUTES FOR COLLINEARITY AND POSITIONAL SIGNIFICANCE

plot(project_sheet$MP, project_sheet$GP, pch = 19, col = colors[factor(project_sheet$Position)], xlab = "Minutes Played", ylab = "Games Played")
plot9 = lm(GP ~ MP + center, data = project_sheet)
legend("topleft", legend = c("C", "PF", "PG", "SF", "SG"), pch = 19, col = colors)
abline(plot9)
summary(plot9)
vif(plot9)


#TESTING FOR MP SIGNIFICANCE FOR POINT GUARDS AND CENTERS

plot10 = lm(GP ~ MP + center + sg + sf + pf, data = project_sheet)
summary(plot10)
abline(plot10)

plot11 = lm(GP ~ MP + pg + sg + pf + sf, data = project_sheet)
abline(plot11)
summary(plot11)

