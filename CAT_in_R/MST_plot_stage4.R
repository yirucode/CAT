ls()
rm(list = ls()) #清除變數；OR: remove(list = ls())
getwd() #獲取當前的工作目錄

library(readxl)
MST_data <- read_excel("D:/CAT_inGit/CAT_in_R/data/MST_data.xlsx", sheet = "stage4")
View(MST_data)
#設定排序
MST_data$PsiSet <- factor(MST_data$PsiSet, ordered = TRUE, levels = c("1", "0.77", "0.3","0.2","0.1"))
#MST_data$method <- factor(MST_data$method, levels = c("MST有平行","CAT+Psi&cont", "OMST+cont.Psi","D-MST+Psi"))
MST_data$method <- factor(MST_data$method, levels = c("MST","CAT", "OMST","D-MST"))
#繪圖
library(ggplot2)

p <- ggplot(data=MST_data, aes(x=PsiSet, y=Bias, 
        label=sprintf("%0.3f", round(Bias, digits = 4)),
        group=method,color=method,shape=method)) + #shape
        geom_line()+  # 根據group繪製線條
        # geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+

        #geom_label()+
        scale_x_discrete(name = "Psi set",
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( limits = c(-0.01,0.01)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(length~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/stage4_Bias.png", width = 800, height = 450)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=PsiSet, y=RMSE, 
        label=sprintf("%0.3f", round(RMSE, digits = 4)),
        group=method,color=method,shape=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+

        #geom_label()+
        scale_x_discrete(name = "Psi set",
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( limits = c(0.2,0.55)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(length~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/stage4_RMSE.png", width = 800, height = 450)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=PsiSet, y=PsiMax, 
        label=sprintf("%0.3f", round(PsiMax, digits = 4)),
        group=method,color=method,shape=method)) + #shape
        geom_line()+  # 根據group繪製線條
        # geom_hline(yintercept = 0.3, linetype = 3, color = "RED") +
        # geom_hline(yintercept = 0.2, linetype = 3, color = "RED") +
        # geom_hline(yintercept = 0.1, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+

        #geom_label()+
        scale_x_discrete(name = "Psi set",
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( 
                # limits = c(0,1)
                breaks = 0.1 * c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(length~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/stage4_PsiMax.png", width = 800, height = 450)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=PsiSet, y=pooluseRate, 
        label=round(pooluseRate, digits = 4),
        group=method,color=method,shape=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+ #hjust

        #geom_label()+
        scale_x_discrete(name = "Psi set",
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(length~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/stage4_pooluseRate.png", width = 800, height = 450)
plot(p)
dev.off()





p <- ggplot(data=MST_data, aes(x=PsiSet, y=InforTurth, 
        label=round(InforTurth, digits = 4),
        group=method,color=method,shape=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+ #hjust

        #geom_label()+
        scale_x_discrete(name = "Psi set",
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( limits = c(5,25)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(length~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/stage4_InforTurth.png", width = 800, height = 450)
plot(p)
dev.off()





p <- ggplot(data=MST_data, aes(x=PsiSet, y=InforEstimate, 
        label=round(InforEstimate, digits = 4),
        group=method,color=method,shape=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point()+  # 繪製散布圖的點

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+ #hjust

        #geom_label()+
        scale_x_discrete(name = "Psi set",
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( limits = c(5,25)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(length~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/stage4_InforEstimate.png", width = 800, height = 450)
plot(p)
dev.off()

