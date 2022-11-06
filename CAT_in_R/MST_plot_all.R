ls()
rm(list = ls()) #清除變數；OR: remove(list = ls())
getwd() #獲取當前的工作目錄

library(readxl)
MST_data <- read_excel("D:/CAT_inGit/CAT_in_R/data/MST_data.xlsx", sheet = "len_all")
View(MST_data)
#設定排序
MST_data$method <- factor(MST_data$method, levels = c("CAT+con", "CA-MST", "OMST+cont","MST有平行"))
#繪圖
library(ggplot2)

p <- ggplot(data=MST_data, aes(x=method, y=Bias, 
        label=sprintf("%0.3f", round(Bias, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        scale_y_continuous( limits = c(-0.01,0.01)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_Bias.png", width = 600, height = 500)
plot(p)
dev.off()



p <- ggplot(data=MST_data, aes(x=method, y=RMSE, 
        label=sprintf("%0.3f", round(RMSE, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        scale_y_continuous( limits = c(0.2,0.5)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_RMSE.png", width = 600, height = 500)
plot(p)
dev.off()



p <- ggplot(data=MST_data, aes(x=method, y=PsiMax, 
        label=sprintf("%0.3f", round(PsiMax, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = 2)+
        scale_y_continuous( limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        labs(y = "PsiMax_alpha1")+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_PsiMax.png", width = 600, height = 500)
plot(p)
dev.off()



p <- ggplot(data=MST_data, aes(x=method, y=PsiMax_alpha2, 
        label=sprintf("%0.3f", round(PsiMax_alpha2, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = 2)+
        scale_y_continuous( limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_PsiMax_alpha2.png", width = 600, height = 500)
plot(p)
dev.off()




p <- ggplot(data=MST_data, aes(x=method, y=PsiMax_alpha3, 
        label=sprintf("%0.3f", round(PsiMax_alpha3, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = 2)+
        scale_y_continuous( limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_PsiMax_alpha3.png", width = 600, height = 500)
plot(p)
dev.off()





p <- ggplot(data=MST_data, aes(x=method, y=pooluseRate, 
        label=sprintf("%0.3f", round(pooluseRate, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        scale_y_continuous( limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_pooluseRate.png", width = 600, height = 500)
plot(p)
dev.off()



p <- ggplot(data=MST_data, aes(x=method, y=InforTurth, 
        label=sprintf("%0.3f", round(InforTurth, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        scale_y_continuous( limits = c(5,25)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_InforTurth.png", width = 600, height = 500)
plot(p)
dev.off()



p <- ggplot(data=MST_data, aes(x=method, y=InforEstimate, 
        label=sprintf("%0.3f", round(InforEstimate, digits = 4)),
        group=method,color=method)) + #shape
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        scale_y_continuous( limits = c(5,25)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        facet_grid(length~stages)+  # 切割欄列
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_all_InforEstimate.png", width = 600, height = 500)
plot(p)
dev.off()

