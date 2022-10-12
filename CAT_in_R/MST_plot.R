ls()
rm(list = ls()) #清除變數；OR: remove(list = ls())
getwd() #獲取當前的工作目錄

library(readxl)
MST_data <- read_excel("D:/CAT_inGit/CAT_in_R/data/MST_data.xlsx", sheet = "len20")
View(MST_data)
#設定排序
MST_data$PsiS <- factor(MST_data$PsiS, ordered = TRUE, levels = c("1", "0.77", "0.3","0.2","0.1"))
#MST_data$method <- 
#繪圖
library(ggplot2)

p <- ggplot(data=MST_data, aes(x=PsiS, y=Bias, 
        label=sprintf("%0.3f", round(Bias, digits = 4)),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        geom_hline(yintercept = 0, linetype = 3) +
        geom_point()+  # 繪製散布圖的點
        geom_text(check_overlap = TRUE, vjust = -1)+
        #geom_label()+
        facet_grid(stages~alpha)+  # 切割欄列
        labs(x = "Psi set")+
        theme_bw() # 設定主題

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/diam.png", width = 600, height = 400)
plot(p)
dev.off()

