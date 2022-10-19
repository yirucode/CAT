install.packages("xxIRT")
install.packages("dplyr")
library(xxIRT)
library(dplyr)
pool <- read.table("D:/CAT_inGit/CAT_in_R/data/parameter.txt", header=T)
pool <- data.frame(pool,seq(1:300))
names(pool) <- c("a","b","c","content","serial")

##for original assembly
pool.ata <- pool
ML <- 10    #module題長
no.M <- length(pool$a)/ML  #module數量
con.level <- c(1,2,3)      #content種類
con.no <- c(4,4,2)         #各content數量
assembly <- data.frame()   #組卷結果df
for (i in 1:no.M){
  for (j in con.level){
    data_mod<- pool.ata %>%                            
      arrange(desc(b)) %>% 
      filter(content==j) %>%
      slice(1:con.no[j])
    pool.ata$b[data_mod$serial]=-99
    assembly <- rbind(assembly,data_mod)
  }
}
write.table(assembly, file = "parameter_CAT&MST_30.txt",sep = " ", quote = FALSE, na = "NA")



##MST 1-2-3-4 design
##P 6-5-4-3
##length = 20
##four stage
##for bottomup assembly
x <- mst(pool, "40", 1, 'bottomup', len=5, max_use=1)
#module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.4:-0.4, indices=c(1:6)) #0  間隔0.4
x <- mst_obj(x, theta=0.8:0.2, indices=c(7:11)) #0.5  間隔0.3
x <- mst_obj(x, theta=-0.8:-0.2, indices=c(12:16)) #-0.5
x <- mst_obj(x, theta=1.2:0.8, indices=c(17:20)) #1 間隔0.2
x <- mst_obj(x, theta=0.2:-0.2, indices=c(21:24)) #0
x <- mst_obj(x, theta=-1.2:-0.8, indices=c(25:28)) #-1
x <- mst_obj(x, theta=1.6:1.4, indices=c(29:31))  # 1.5  間隔0.1
x <- mst_obj(x, theta=0.6:0.4, indices=c(32:34)) #0.5
x <- mst_obj(x, theta=-0.6:-0.4, indices=c(35:37)) #-0.5
x <- mst_obj(x, theta=-1.6:-1.4, indices=c(38:40)) #-1.5
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
x$item

# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len5_1-2-3-4_P_6-5-4-3.png", width = 600, height = 500)
plot(x, byroute=FALSE)
dev.off()
#write.table(x$item, file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len5_1-2-3-4_P_6543.txt",sep = " ", quote = FALSE, na = "NA")
write.table(x$item, 
            file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len5_1-2-3-4_P_6543.csv", 
            sep = ",", col.names = NA,
            qmethod = "double")
# 有時會組不出來，要重試幾次


##MST 1-2-3-4 design
##P 4-3-2-1
##length = 40
##four stage
##for bottomup assembly
x <- mst(pool, "20", 1, 'bottomup', len=10, max_use=1)
#module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.4:-0.4, indices=c(1:4)) #0  間隔0.4
x <- mst_obj(x, theta=0.8:0.2, indices=c(5:7)) #0.5  間隔0.3
x <- mst_obj(x, theta=-0.8:-0.2, indices=c(8:10)) #-0.5
x <- mst_obj(x, theta=1.2:0.8, indices=c(11:12)) #1 間隔0.2
x <- mst_obj(x, theta=0.2:-0.2, indices=c(13:14)) #0
x <- mst_obj(x, theta=-1.2:-0.8, indices=c(15:16)) #-1
x <- mst_obj(x, theta=1.6:1.4, indices=c(17))  # 1.5  間隔0.1
x <- mst_obj(x, theta=0.6:0.4, indices=c(18)) #0.5
x <- mst_obj(x, theta=-0.6:-0.4, indices=c(19)) #-0.5
x <- mst_obj(x, theta=-1.6:-1.4, indices=c(20)) #-1.5
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len10_1-2-3-4_P_4-3-2-1.png", width = 600, height = 500)
plot(x, byroute=FALSE)
dev.off()
#write.table(x$item, file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len10_1-2-3-4_P_4321.txt",sep = "  ", quote = FALSE, na = "NA")
write.table(x$item, 
            file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len10_1-2-3-4_P_4321.csv", 
            sep = ",", col.names = NA,
            qmethod = "double")


##MST 1-2 design
##P 10-5
##length = 20
##two stage
##for bottomup assembly
x <- mst(pool, "20", 1, 'bottomup', len=10, max_use=1)
#module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.4:-0.4, indices=c(1:10)) #0 間隔0.4
x <- mst_obj(x, theta=0.7:0.3, indices=c(11:15)) #0.5 間隔0.2
x <- mst_obj(x, theta=-0.7:-0.3, indices=c(16:20)) #-0.5
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len10_1-2_P_10-5.png", width = 600, height = 500)
plot(x, byroute=FALSE)
dev.off()
#write.table(x$item, file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len10_1-2_P_10-5.txt",sep = " ", quote = FALSE, na = "NA")
write.table(x$item, 
            file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len10_1-2_P_10-5.csv", 
            sep = ",", col.names = NA,
            qmethod = "double")



##MST 1-2 design
##P 4-3
##length = 40
##two stage
##for bottomup assembly
x <- mst(pool, "10", 1, 'bottomup', len=20, max_use=1)
#module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.4:-0.4, indices=c(1:4)) #0 間隔0.4
x <- mst_obj(x, theta=0.7:0.3, indices=c(5:7)) #0.5 間隔0.2
x <- mst_obj(x, theta=-0.7:-0.3, indices=c(8:10)) #-0.5
x <- mst_constraint(x, "content", 8, 8, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 8, 8, level=2)
x <- mst_constraint(x, "content", 4, 4, level=3)
x <- mst_assemble(x)
x$item
# PNG 輸出，單位為像素
png( file = "D:/CAT_inGit/CAT_in_R/picture/len20_1-2_P_4-3.png", width = 600, height = 500)
plot(x, byroute=FALSE)
dev.off()
write.table(x$item, file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len20_1-2_P_4-3.txt",sep = " ", quote = FALSE, na = "NA")
write.table(x$item, 
            file = "D:/CAT_inGit/CAT_in_R/data/parameter_MST_len20_1-2_P_4-3.csv", 
            sep = ",", col.names = NA,
            qmethod = "double")




##try
x <- mst(pool, "1-3", 3, 'bottomup', len=20, max_use=1)
#module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.4:-0.4, indices=c(1)) #0 間隔0.4
x <- mst_obj(x, theta=0.7:0.3, indices=c(2)) #0.5 間隔0.2
x <- mst_obj(x, theta=0.3:-0.3, indices=c(3)) #0 間隔0.4
x <- mst_obj(x, theta=-0.7:-0.3, indices=c(4)) #-0.5
# x <- mst_obj(x, theta=0, indices=c(1)) #0 間隔0.4
# x <- mst_obj(x, theta=0.5, indices=c(2)) #0.5 間隔0.2
# x <- mst_obj(x, theta=0, indices=c(3)) #0 間隔0.4
# x <- mst_obj(x, theta=-0.5, indices=c(4)) #-0.5
x <- mst_constraint(x, "content", 8, 8, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 8, 8, level=2)
x <- mst_constraint(x, "content", 4, 4, level=3)
x <- mst_assemble(x)
x$item
plot(x,byroute=FALSE)
dev.new(width=999, height=999)
