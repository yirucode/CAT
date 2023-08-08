ls()
rm(list = ls()) #清除變數；OR: remove(list = ls())
getwd() #獲取當前的工作目錄

library(readxl)
MST_data <- read_excel("D:/CAT_inGit/CAT_in_R/data/MST_data.xlsx", sheet = "len40")
# 替換資料
MST_data$stages[MST_data$stages == 2] <- '2 stages'
MST_data$stages[MST_data$stages == 4] <- '4 stages'
MST_data$length[MST_data$length == 20] <- 'length = 20'
MST_data$length[MST_data$length == 40] <- 'length = 40'
MST_data$alpha[MST_data$alpha == 1] <- 'gamma = 1'
MST_data$alpha[MST_data$alpha == 2] <- 'gamma = 2'
MST_data$alpha[MST_data$alpha == 3] <- 'gamma = 3'
# 檢視資料
View(MST_data)
#設定排序
MST_data$PsiSet <- factor(MST_data$PsiSet, ordered = TRUE, levels = c("1", "0.77", "0.3","0.2","0.1"))
#MST_data$method <- factor(MST_data$method, levels = c("MST有平行", "CAT+Psi&cont", "OMST+cont.Psi", "D-MST+Psi"))
MST_data$method <- factor(MST_data$method, levels = c("Random", "MST", "CAT", "OMST-SH", "OMST-PSI", "D-MST"))
#繪圖
library(ggplot2)

p <- ggplot(data=MST_data, aes(x=PsiSet, y=Bias, 
        label=sprintf("%0.3f", round(Bias, digits = 4)),
        group=method,color=method,shape=method,linetype=method)) + #shape
        geom_line()+  # 根據group繪製線條
        # geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point(size = 3)+  # 繪製散布圖的點
        scale_shape_manual(values = c(20, 18, 1, 2, 5, 0))+  # 設定標記圖案
        scale_linetype_manual(values = c(3, 3, 2, 4, 5, 6))+  # 設定線段型態
        
        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+
        
        #geom_label()+
        scale_x_discrete(name = expression(paste("set  ",bar(Psi)[max]," / ",r[max])),
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( limits = c(-0.01,0.01)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(stages~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_Bias.png", width = 800, height = 450)
ggsave("D:/CAT_inGit/CAT_in_R/picture/len40_Bias.png",pointsize=10, width = 8, height = 4.5, unit = 'in',dpi = 300)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=PsiSet, y=RMSE, 
        label=sprintf("%0.3f", round(RMSE, digits = 4)),
        group=method,color=method,shape=method,linetype=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point(size = 3)+  # 繪製散布圖的點
        
        scale_shape_manual(values = c(20, 18, 1, 2, 5, 0))+  # 設定標記圖案
        scale_linetype_manual(values = c(3, 3, 2, 4, 5, 6))+  # 設定線段型態
        
        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+

        #geom_label()+
        scale_x_discrete(name = expression(paste("set  ",bar(Psi)[max]," / ",r[max])),
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( limits = c(0.2,0.55)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(stages~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_RMSE.png", width = 800, height = 450)
ggsave("D:/CAT_inGit/CAT_in_R/picture/len40_RMSE.png",pointsize=10, width = 8, height = 4.5, unit = 'in',dpi = 300)
plot(p)
dev.off()


p <- ggplot(data=MST_data, aes(x=PsiSet, y=PsiMax, 
        label=sprintf("%0.3f", round(PsiMax, digits = 4)),
        group=method,color=method,shape=method,linetype=method)) + #shape
        geom_line()+  # 根據group繪製線條
        # geom_hline(yintercept = 0.3, linetype = 3, color = "RED") +
        # geom_hline(yintercept = 0.2, linetype = 3, color = "RED") +
        # geom_hline(yintercept = 0.1, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point(size = 3)+  # 繪製散布圖的點
        
        scale_shape_manual(values = c(20, 18, 1, 2, 5, 0))+  # 設定標記圖案
        scale_linetype_manual(values = c(3, 3, 2, 4, 5, 6))+  # 設定線段型態
        
        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+

        #geom_label()+
        scale_x_discrete(name = expression(paste("set  ",bar(Psi)[max]," / ",r[max])),
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous( 
                name = expression(paste('max ',bar(Psi))),
                # limits = c(0,1)
                breaks = 0.1 * c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(stages~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_PsiMax.png", width = 800, height = 450)
ggsave("D:/CAT_inGit/CAT_in_R/picture/len40_PsiMax.png",pointsize=10, width = 8, height = 4.5, unit = 'in',dpi = 300)
plot(p)
dev.off()




p <- ggplot(data=MST_data, aes(x=PsiSet, y=pooluseRate, 
        label=round(pooluseRate, digits = 4),
        group=method,color=method,shape=method,linetype=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point(size = 3)+  # 繪製散布圖的點
        
        scale_shape_manual(values = c(20, 18, 1, 2, 5, 0))+  # 設定標記圖案
        scale_linetype_manual(values = c(3, 3, 2, 4, 5, 6))+  # 設定線段型態
        
        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+ #hjust

        #geom_label()+
        scale_x_discrete(name = expression(paste("set  ",bar(Psi)[max]," / ",r[max])),
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous(
                name = 'utilization of the item pool',
                limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(stages~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_pooluseRate.png", width = 800, height = 450)
ggsave("D:/CAT_inGit/CAT_in_R/picture/len40_pooluseRate.png",pointsize=10, width = 8, height = 4.5, unit = 'in',dpi = 300)
plot(p)
dev.off()





p <- ggplot(data=MST_data, aes(x=PsiSet, y=InforTurth, 
        label=round(InforTurth, digits = 4),
        group=method,color=method,shape=method,linetype=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point(size = 3)+  # 繪製散布圖的點
        
        scale_shape_manual(values = c(20, 18, 1, 2, 5, 0))+  # 設定標記圖案
        scale_linetype_manual(values = c(3, 3, 2, 4, 5, 6))+  # 設定線段型態
        
        # geom_text(                        # 標記文字
        #         check_overlap = TRUE,     # 防止文字重疊
        #         vjust = -1)+ #hjust

        #geom_label()+
        scale_x_discrete(name = expression(paste("set  ",bar(Psi)[max]," / ",r[max])),
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous(
                name = expression(paste('mean ','Info',(theta))),
                limits = c(0,25)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(stages~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_InforTurth.png", width = 800, height = 450, unit = 'px')
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_InforTurth.png", width = 8, height = 4.5, unit = 'in', res = 600)
ggsave("D:/CAT_inGit/CAT_in_R/picture/len40_InforTurth.png",pointsize=10, width = 8, height = 4.5, unit = 'in',dpi = 300)
plot(p)
dev.off()





p <- ggplot(data=MST_data, aes(x=PsiSet, y=InforEstimate, 
        label=round(InforEstimate, digits = 4),
        group=method,color=method,shape=method,linetype=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point(size = 3)+  # 繪製散布圖的點
        
        scale_shape_manual(values = c(20, 18, 1, 2, 5, 0))+  # 設定標記圖案
        scale_linetype_manual(values = c(3, 3, 2, 4, 5, 6))+  # 設定線段型態

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+ #hjust

        #geom_label()+
        scale_x_discrete(name = expression(paste("set  ",bar(Psi)[max]," / ",r[max])),
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous(
                name = expression(paste('mean ','Info',(hat(theta)))),
                limits = c(0,25)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(stages~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_InforEstimate.png", width = 800, height = 450)
ggsave("D:/CAT_inGit/CAT_in_R/picture/len40_InforEstimate.png",pointsize=10, width = 8, height = 4.5, unit = 'in',dpi = 300)
plot(p)
dev.off()

p <- ggplot(data=MST_data, aes(x=PsiSet, y=r_max, 
        label=round(r_max, digits = 4),
        group=method,color=method,shape=method,linetype=method)) + #shape
        geom_line()+  # 根據group繪製線條
        #geom_hline(yintercept = 0, linetype = 3, color = "RED") +
        #guides(colour = guide_legend("Year",
        #        label.position = "bottom", ncol = 3))+
        geom_point(size = 3)+  # 繪製散布圖的點
        
        scale_shape_manual(values = c(20, 18, 1, 2, 5, 0))+  # 設定標記圖案
        scale_linetype_manual(values = c(3, 3, 2, 4, 5, 6))+  # 設定線段型態

        # geom_text(
        #         check_overlap = TRUE, 
        #         vjust = -1)+ #hjust

        #geom_label()+
        scale_x_discrete(name = expression(paste("set  ",bar(Psi)[max]," / ",r[max])),
                limits=c("1","0.3","0.2","0.1"))+
        scale_y_continuous(
                name = expression(paste("max ",r)),
                limits = c(0,1)
                # breaks = 0.001 * c(1, 2, 4, 8, 16),
                # minor_breaks = NULL
                )+
        #labs(x = "Psi set")+
        facet_grid(stages~alpha)+  # 切割欄列
        theme_bw()+ # 設定主題
        #theme(legend.position = 'top') #bottom

# PNG 輸出，單位為像素
# png( file = "D:/CAT_inGit/CAT_in_R/picture/len40_InforEstimate.png", width = 800, height = 450)
ggsave("D:/CAT_inGit/CAT_in_R/picture/len40_max_r.png",pointsize=10, width = 8, height = 4.5, unit = 'in',dpi = 300)
plot(p)
dev.off()
