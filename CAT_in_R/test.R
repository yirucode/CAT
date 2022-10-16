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
write.table(assembly, file = "~result\parameter_CAT&MST_30.txt",sep = " ", quote = FALSE, na = "NA")



##MST 1-2-3-4 design
##four stage
##for bottomup assembly
x <- mst(pool, "1-2-3-4", 1, 'bottomup', len=10, max_use=1)  #設定1-2-3-4 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1,5))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(2,8))
x <- mst_obj(x, theta=-0.5, indices=c(3,9))
x <- mst_obj(x, theta=1, indices=c(4))     
x <- mst_obj(x, theta=-1, indices=c(6))
x <- mst_obj(x, theta=1.5, indices=c(7))
x <- mst_obj(x, theta=-1.5, indices=c(10))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_1-2-3-4_data.txt",sep = " ", quote = FALSE, na = "NA")

##平行測驗
##four stage 30 module 
##每階層的module數量相同
##for bottomup assembly
x <- mst(pool, "30", 1, 'bottomup', len=10, max_use=3)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:3,13:15))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(4:6,22:24))
x <- mst_obj(x, theta=-0.5, indices=c(7:9,25:27))
x <- mst_obj(x, theta=1, indices=c(10:12))     
x <- mst_obj(x, theta=-1, indices=c(16:18))
x <- mst_obj(x, theta=1.5, indices=c(19:21))
x <- mst_obj(x, theta=-1.5, indices=c(28:30))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_30_R_cut3.txt",sep = " ", quote = FALSE, na = "NA")

##每階層的module數量不同
##for bottomup assembly
x <- mst(pool, "20", 1, 'bottomup', len=10, max_use=1)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:4,13:14))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(5:7,18))
x <- mst_obj(x, theta=-0.5, indices=c(8:10,19))
x <- mst_obj(x, theta=1, indices=c(11:12))     
x <- mst_obj(x, theta=-1, indices=c(15:16))
x <- mst_obj(x, theta=1.5, indices=c(17))
x <- mst_obj(x, theta=-1.5, indices=c(20))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_1-2-3-4_data_P.txt",sep = " ", quote = FALSE, na = "NA")


##平行測驗
##four stage 30 module 
##每階層的module數量相同
##MST 1-2-3-4 design
##four stage
##for bottomup assembly
x <- mst(pool, "1-2-3-4", 3, 'bottomup', len=10, max_use=2)  #設定1-2-3-4 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1,5))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(2,8))
x <- mst_obj(x, theta=-0.5, indices=c(3,9))
x <- mst_obj(x, theta=1, indices=c(4))     
x <- mst_obj(x, theta=-1, indices=c(6))
x <- mst_obj(x, theta=1.5, indices=c(7))
x <- mst_obj(x, theta=-1.5, indices=c(10))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_original_1-2-3-4.txt",sep = " ", quote = FALSE, na = "NA")













##one stage 30 module
##for bottomup assembly
x <- mst(pool, "30", 1, 'bottomup', len=10, max_use=1)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:10))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(11:20))
x <- mst_obj(x, theta=-1, indices=c(21:30))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_30_R_cut3.txt",sep = " ", quote = FALSE, na = "NA")


##上列代碼無法成功，因此限縮要編制的module
##one stage 24 module
##for bottomup assembly
x <- mst(pool, "24", 1, 'bottomup', len=10, max_use=1)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:8))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(9:16))
x <- mst_obj(x, theta=-1, indices=c(17:24))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_30_R_cut3.txt",sep = " ", quote = FALSE, na = "NA")


##上列代碼無法成功，因此只編制部分代碼
##two stage 30 module
##for bottomup assembly
x <- mst(pool, "30", 1, 'bottomup', len=10, max_use=1)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:10))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=-1, indices=c(11:20))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)









##four stage
##for bottomup assembly
x <- mst(pool, "1-2-3-4", 1, 'bottomup', len=10, max_use=1)  #設定1-2-3-4 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1,5))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(2,8))
x <- mst_obj(x, theta=-0.5, indices=c(3,9))
x <- mst_obj(x, theta=1, indices=c(4))     
x <- mst_obj(x, theta=-1, indices=c(6))
x <- mst_obj(x, theta=1.5, indices=c(7))
x <- mst_obj(x, theta=-1.5, indices=c(10))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)

write.table(x$item, file = "parameter_MST_original_1-2-3-4.txt",sep = " ", quote = FALSE, na = "NA")


##for bottomup assembly
x <- mst(pool, "1-3-3-3", 1, 'bottomup', len=10, max_use=1)  #設定1-3-3-3 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1,3,6,9))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(2,5,8))
x <- mst_obj(x, theta=-1, indices=c(4,7,10))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_1-3-3-3_data.txt",sep = " ", quote = FALSE, na = "NA")

##平行測驗
##for bottomup assembly
x <- mst(pool, "12", 1, 'bottomup', len=10, max_use=1)  #設定1-3-3-3 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:3,5,8,11))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(4,7,10))
x <- mst_obj(x, theta=-1, indices=c(6,9,12))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_1-3-3-3_data_P.txt",sep = " ", quote = FALSE, na = "NA")

##for bottomup assembly
x <- mst(pool, "30", 1, 'bottomup', len=10, max_use=1)  #設定1-2-3-3 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:3,13:15,22:24))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(4:6,10:12,19:21))
x <- mst_obj(x, theta=-1, indices=c(7:9,16:18,25:27))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_1-3-3-3.txt",sep = " ", quote = FALSE, na = "NA")


##for bottomup assembly
x <- mst(pool, "1-2-3-3", 1, 'bottomup', len=10, max_use=1)  #設定1-2-3-3 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1,5,7))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(2,4,8))
x <- mst_obj(x, theta=-1, indices=c(3,6,9))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_original_1-2-3-3.txt",sep = " ", quote = FALSE, na = "NA")





##four stage 30 module 
##每階層的module數量相同
##for bottomup assembly
##result: 無法成功
x <- mst(pool, "30", 1, 'bottomup', len=10, max_use=1)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:3,13:15))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(4:6,22:24))
x <- mst_obj(x, theta=-0.5, indices=c(7:9,25:27))
x <- mst_obj(x, theta=1, indices=c(10:12))     
x <- mst_obj(x, theta=-1, indices=c(16:18))
x <- mst_obj(x, theta=1.5, indices=c(19:21))
x <- mst_obj(x, theta=-1.5, indices=c(28:30))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_30_R_cut3.txt",sep = " ", quote = FALSE, na = "NA")


##four stage 30 module 
##每階層的module數量相同
##for bottomup assembly
##result: if max_use=1 無法成功
x <- mst(pool, "30", 1, 'bottomup', len=10, max_use=3)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:6))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(7:12))
x <- mst_obj(x, theta=-0.5, indices=c(13:18))
x <- mst_obj(x, theta=1, indices=c(19:21))     
x <- mst_obj(x, theta=-1, indices=c(22:24))
x <- mst_obj(x, theta=1.5, indices=c(25:27))
x <- mst_obj(x, theta=-1.5, indices=c(28:30))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_30_R_cut3.txt",sep = " ", quote = FALSE, na = "NA")







##four stage 30 module 
##每階層的module數量相同
##for bottomup assembly
x <- mst(pool, "30", 1, 'bottomup', len=10, max_use=2)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:5,17:19))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(6:9,25:26))
x <- mst_obj(x, theta=-0.5, indices=c(10:13,27:28))
x <- mst_obj(x, theta=1, indices=c(14:16))     
x <- mst_obj(x, theta=-1, indices=c(20:22))
x <- mst_obj(x, theta=1.5, indices=c(23:24))
x <- mst_obj(x, theta=-1.5, indices=c(29:30))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
write.table(x$item, file = "parameter_MST_30_R_cut_5432.txt",sep = " ", quote = FALSE, na = "NA")


#two stage 15 module
##for bottomup assembly
x <- mst(pool, "15", 1, 'bottomup', len=10, max_use=1)  #設定15 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst_obj(x, theta=0, indices=c(1:5))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(6:10))
x <- mst_obj(x, theta=-1, indices=c(11:15))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
write.table(x$item, file = "parameter_MST_15_R_cut3.txt",sep = " ", quote = FALSE, na = "NA")




#2022/5/18 
#2 stage, len = 10

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
write.table(assembly, file = "parameter_MST_len10.txt",sep = " ", quote = FALSE, na = "NA")


##for bottomup assembly
#設定1-2 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst(pool, "1-2", 1, 'bottomup', len=10, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(2))
x <- mst_obj(x, theta=-1, indices=c(3))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_len10_1-2.txt",sep = " ", quote = FALSE, na = "NA")


## two stage
##for bottomup assembly
#設定1-2 design、組出1個panel、採用bottom up法、模組題長10、每道試題最多出現1次
x <- mst(pool, "20", 1, 'bottomup', len=10, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:10))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(11:15))
x <- mst_obj(x, theta=-1, indices=c(16:20))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_len10_1-2_P.txt",sep = " ", quote = FALSE, na = "NA")






#four stages

##for original assembly
pool.ata <- pool
ML <- 5    #module題長
no.M <- length(pool$a)/ML  #module數量
con.level <- c(1,2,3)      #content種類
con.no <- c(2,2,1)         #各content數量
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
write.table(assembly, file = "parameter_MST_len5.txt",sep = " ", quote = FALSE, na = "NA")


##four stage
##for bottomup assembly
#設定1-2-3-4 design、組出1個panel、採用bottom up法、模組題長5、每道試題最多出現1次
x <- mst(pool, "1-2-3-4", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1,5))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(2,8))
x <- mst_obj(x, theta=-0.5, indices=c(3,9))
x <- mst_obj(x, theta=1, indices=c(4))     
x <- mst_obj(x, theta=-1, indices=c(6))
x <- mst_obj(x, theta=1.5, indices=c(7))
x <- mst_obj(x, theta=-1.5, indices=c(10))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_len5_1-2-3-4.txt",sep = " ", quote = FALSE, na = "NA")


##每階層的module數量不同
##for bottomup assembly
#設定20 design、組出1個panel、採用bottom up法、模組題長5、每道試題最多出現1次
x <- mst(pool, "20", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:4,13:14))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(5:7,18))
x <- mst_obj(x, theta=-0.5, indices=c(8:10,19))
x <- mst_obj(x, theta=1, indices=c(11:12))     
x <- mst_obj(x, theta=-1, indices=c(15:16))
x <- mst_obj(x, theta=1.5, indices=c(17))
x <- mst_obj(x, theta=-1.5, indices=c(20))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_len5_1-2-3-4_P.txt",sep = " ", quote = FALSE, na = "NA")



##for bottomup assembly
#設定1-3-3-3 design、組出1個panel、採用bottom up法、模組題長5、每道試題最多出現1次
x <- mst(pool, "1-3-3-3", 1, 'bottomup', len=5, max_use=1)
#module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0, indices=c(1,3,6,9))
x <- mst_obj(x, theta=1, indices=c(2,5,8))
x <- mst_obj(x, theta=-1, indices=c(4,7,10))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
plot(x, byroute=FALSE)
x$item
write.table(x$item, file = "parameter_MST_len5_1-3-3-3.txt",sep = " ", quote = FALSE, na = "NA")


##平行測驗
##for bottomup assembly
#設定12 design、組出1個panel、採用bottom up法、模組題長5、每道試題最多出現1次
x <- mst(pool, "12", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:3,5,8,11))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(4,7,10))
x <- mst_obj(x, theta=-1, indices=c(6,9,12))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len5_1-3-3-3_P.txt",sep = " ", quote = FALSE, na = "NA")





#2022/7/10
#2 stage, len = 20

##for original assembly
pool.ata <- pool
ML <- 20    #module題長
no.M <- length(pool$a)/ML  #module數量
con.level <- c(1,2,3)      #content種類
con.no <- c(8,8,4)         #各content數量
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
write.table(assembly, file = "parameter_MST_len20.txt",sep = " ", quote = FALSE, na = "NA")



# 2022/7/24
##平行測驗 !無法成功
##for bottomup assembly
#設定12 design、組出1個panel、採用bottom up法、模組題長5、每道試題最多出現1次

# Failed
x <- mst(pool, "40", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:6,21:24))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(7:11,32:34)) 
x <- mst_obj(x, theta=1, indices=c(17:20))
x <- mst_obj(x, theta=1.5, indices=c(29:31))
x <- mst_obj(x, theta=-0.5, indices=c(12:16,35:37))
x <- mst_obj(x, theta=-1, indices=c(25:28))
x <- mst_obj(x, theta=-1.5, indices=c(38:40))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len5_1-2-3-4_P2.txt",sep = " ", quote = FALSE, na = "NA")

# Failed
x <- mst(pool, "30", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:5,17:19))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(6:9,25:26)) 
x <- mst_obj(x, theta=1, indices=c(14:16))
x <- mst_obj(x, theta=1.5, indices=c(23,24))
x <- mst_obj(x, theta=-0.5, indices=c(10:13,27:28))
x <- mst_obj(x, theta=-1, indices=c(20:22))
x <- mst_obj(x, theta=-1.5, indices=c(29,30))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len5_1-2-3-4_P_5432.txt",sep = " ", quote = FALSE, na = "NA")


x <- mst(pool, "20", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:4,13:14))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(5:7,18)) 
x <- mst_obj(x, theta=1, indices=c(11:12))
x <- mst_obj(x, theta=1.5, indices=c(17))
x <- mst_obj(x, theta=-0.5, indices=c(8:10,19))
x <- mst_obj(x, theta=-1, indices=c(15:16))
x <- mst_obj(x, theta=-1.5, indices=c(20))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len5_1-2-3-4_P_4321.txt",sep = " ", quote = FALSE, na = "NA")




x <- mst(pool, "40", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:8))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(9:20)) 
x <- mst_obj(x, theta=1, indices=c())
x <- mst_obj(x, theta=1.5, indices=c(17))
x <- mst_obj(x, theta=-0.5, indices=c(8:10,19))
x <- mst_obj(x, theta=-1, indices=c(15:16))
x <- mst_obj(x, theta=-1.5, indices=c(20))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len5_1-2-3-4_P_4321.txt",sep = " ", quote = FALSE, na = "NA")









x <- mst(pool, "20", 1, 'bottomup', len=10, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:4,13:14))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(5:7,18)) 
x <- mst_obj(x, theta=1, indices=c(11:12))
x <- mst_obj(x, theta=1.5, indices=c(17))
x <- mst_obj(x, theta=-0.5, indices=c(8:10,19))
x <- mst_obj(x, theta=-1, indices=c(15:16))
x <- mst_obj(x, theta=-1.5, indices=c(20))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len10_1-2-3-4_P_4321.txt",sep = " ", quote = FALSE, na = "NA")




x <- mst(pool, "20", 1, 'bottomup', len=10, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:10))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(11:15))
x <- mst_obj(x, theta=-1, indices=c(16:20))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len10_1-2_P_10-5.txt",sep = " ", quote = FALSE, na = "NA")



x <- mst(pool, "10", 1, 'bottomup', len=20, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1:4))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(5:7))
x <- mst_obj(x, theta=-1, indices=c(8:10))
x <- mst_constraint(x, "content", 8, 8, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 8, 8, level=2)
x <- mst_constraint(x, "content", 4, 4, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len20_1-2_P_4-3.txt",sep = " ", quote = FALSE, na = "NA")


# ==== 無平行

x <- mst(pool, "3", 1, 'bottomup', len=10, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(2))
x <- mst_obj(x, theta=-1, indices=c(3))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len10_1-2.txt",sep = " ", quote = FALSE, na = "NA")




x <- mst(pool, "10", 1, 'bottomup', len=5, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1,5))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(2,8)) 
x <- mst_obj(x, theta=1, indices=c(4))
x <- mst_obj(x, theta=1.5, indices=c(7))
x <- mst_obj(x, theta=-0.5, indices=c(3,9))
x <- mst_obj(x, theta=-1, indices=c(6))
x <- mst_obj(x, theta=-1.5, indices=c(10))
x <- mst_constraint(x, "content", 2, 2, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 2, 2, level=2)
x <- mst_constraint(x, "content", 1, 1, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len5_1-2-3-4.txt",sep = " ", quote = FALSE, na = "NA")





x <- mst(pool, "3", 1, 'bottomup', len=20, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=1, indices=c(2))
x <- mst_obj(x, theta=-1, indices=c(3))
x <- mst_constraint(x, "content", 8, 8, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 8, 8, level=2)
x <- mst_constraint(x, "content", 4, 4, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len20_1-2.txt",sep = " ", quote = FALSE, na = "NA")


x <- mst(pool, "10", 1, 'bottomup', len=10, max_use=1)
x <- mst_obj(x, theta=0, indices=c(1,5))     #module編號(indices) 1設定能力0最為訊息量最大化依據，以下類推
x <- mst_obj(x, theta=0.5, indices=c(2,8)) 
x <- mst_obj(x, theta=1, indices=c(4))
x <- mst_obj(x, theta=1.5, indices=c(7))
x <- mst_obj(x, theta=-0.5, indices=c(3,9))
x <- mst_obj(x, theta=-1, indices=c(6))
x <- mst_obj(x, theta=-1.5, indices=c(10))
x <- mst_constraint(x, "content", 4, 4, level=1)  #content 1設定題數4題，以下類推
x <- mst_constraint(x, "content", 4, 4, level=2)
x <- mst_constraint(x, "content", 2, 2, level=3)
x <- mst_assemble(x)
x$item
plot(x, byroute=FALSE)
write.table(x$item, file = "parameter_MST_len10_1-2-3-4.txt",sep = " ", quote = FALSE, na = "NA")




##for original assembly
pool.ata <- pool
ML <- 20    #module題長
no.M <- length(pool$a)/ML  #module數量
con.level <- c(1,2,3)      #content種類
con.no <- c(8,8,4)         #各content數量
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
write.table(assembly, file = "parameter_MST_15.txt",sep = " ", quote = FALSE, na = "NA")




