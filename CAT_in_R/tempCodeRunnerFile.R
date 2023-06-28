ls()
rm(list = ls()) #清除變數；OR: remove(list = ls())
getwd() #獲取當前的工作目錄

library(readxl)
MST_data <- read_excel("D:/CAT_inGit/CAT_in_R/data/MST_data.xlsx", sheet = "len20")
View(MST_data)
#設定排序
MST_data$PsiSet <- factor(MST_data$PsiSet, ordered = TRUE, levels = c("1", "0.77", "0.3","0.2","0.1"))
#MST_data$method <- factor(MST_data$method, levels = c("MST有平行","CAT+Psi&cont", "OMST+cont.Psi","D-MST+Psi"))
MST_data$method <- factor(MST_data$method, levels = c("MST","CAT", "OMST","D-MST"))
#繪圖
library(ggplot2)