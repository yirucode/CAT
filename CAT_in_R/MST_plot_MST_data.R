ls()
rm(list = ls()) #清除變數；OR: remove(list = ls())
getwd() #獲取當前的工作目錄

library(readxl)
MST_data <- read_excel("D:/CAT_inGit/CAT_in_R/data/MST_data.xlsx", sheet = "MST_data")
# 替換資料
MST_data$stages[MST_data$stages == 2] <- 'stage = 2'
MST_data$stages[MST_data$stages == 4] <- 'stage = 4'
MST_data$length[MST_data$length == 20] <- 'length = 20'
MST_data$length[MST_data$length == 40] <- 'length = 40'
MST_data$alpha[MST_data$alpha == 1] <- 'alpha = 1'
MST_data$alpha[MST_data$alpha == 2] <- 'alpha = 2'
MST_data$alpha[MST_data$alpha == 3] <- 'alpha = 3'
# 檢視資料
View(MST_data)
#設定排序
#MST_data$PsiSet <- factor(MST_data$PsiSet, ordered = TRUE, levels = c("1", "0.77", "0.3","0.2","0.1"))
MST_data$method <- factor(MST_data$method, levels = c("one+infor","one+theta", "range+infor", "range+theta"))



#繪圖
library(ggplot2)

p <- ggplot(data=MST_data, aes(x=stages, y=Bias, 
        label=sprintf("%0.3f", round(Bias, digits = 4)),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        geom_hline(yintercept = 0, linetype = 3) + #color = "RED" 無法使用，因為已被用
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        #geom_label()+
        scale_x_discrete(limits=c("2","4"))+
        scale_y_continuous( limits = c(-0.01,0.01)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(.~length)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_MST_Bias.png", width = 600, height = 300)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=stages, y=RMSE, 
        label=sprintf("%0.3f", round(RMSE, digits = 4)),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點
        geom_text(
                #check_overlap = TRUE, 
                vjust = -1)+
        #geom_label()+
        scale_x_discrete(limits=c("2","4"))+
        scale_y_continuous( limits = c(0.27,0.42)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(.~length)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_MST_RMSE.png", width = 600, height = 300)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=stages, y=PsiMax, 
        label=sprintf("%0.3f", round(PsiMax, digits = 4)),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        #geom_label()+
        scale_x_discrete(limits=c("2","4"))+
        scale_y_continuous( limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(.~length)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_MST_PsiMax.png", width = 600, height = 300)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=stages, y=pooluseRate, 
        label=round(pooluseRate, digits = 4),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+ #hjust
        #geom_label()+
        scale_x_discrete(limits=c("2","4"))+
        scale_y_continuous( limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(.~length)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_MST_pooluseRate.png", width = 600, height = 300)
plot(p)
dev.off()





p <- ggplot(data=MST_data, aes(x=stages, y=InforTurth, 
        label=round(InforTurth, digits = 4),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點
        geom_text(
                #check_overlap = TRUE, 
                vjust = -1)+ #hjust
        #geom_label()+
        scale_x_discrete(limits=c("2","4"))+
        scale_y_continuous( limits = c(5,15) #(5,25)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(.~length)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_MST_InforTurth.png", width = 600, height = 300)
plot(p)
dev.off()





p <- ggplot(data=MST_data, aes(x=stages, y=InforEstimate, 
        label=round(InforEstimate, digits = 4),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+ #hjust
        #geom_label()+
        scale_x_discrete(limits=c("2","4"))+
        scale_y_continuous( limits = c(5,15) #5,25
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(.~length)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_MST_InforEstimate.png", width = 600, height = 300)
plot(p)
dev.off()




# ============================
# CA-MST plot #未完成

ls()
rm(list = ls()) #清除變數；OR: remove(list = ls())
getwd() #獲取當前的工作目錄

library(readxl)
MST_data <- read_excel("D:/CAT_inGit/CAT_in_R/data/MST_data.xlsx", sheet = "CA-MST_data")
View(MST_data)
#設定排序
#MST_data$PsiSet <- factor(MST_data$PsiSet, ordered = TRUE, levels = c("1", "0.77", "0.3","0.2","0.1"))
#MST_data$method <- factor(MST_data$method, levels = c("one", "range"))
#繪圖
library(ggplot2)


p <- ggplot(data=MST_data, aes(x=module, y=Bias, 
        label=sprintf("%0.3f", round(Bias, digits = 4)),
        group=method,color=method)) + #shape
        geom_line()+  # 根據group繪製線條
        geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點
        geom_text(
                check_overlap = TRUE, 
                vjust = -1)+
        #geom_label()+
        scale_x_discrete(limits=c("2","4"))+
        scale_y_continuous( limits = c(-0.01,0.01)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(.~length)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len_MST_Bias.png", width = 600, height = 300)
plot(p)
dev.off()
















