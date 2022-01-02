library(dplyr)
covidnew<-read.csv("C:\\Users\\Rhea Arora\\Documents\\Coronanew\\covidnew.csv")
View(covidnew)

#Human Control
HC<-covidnew[,c( "ï..Subject_ID","HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
              "HC91", "HC94", "HC75", "HC76")]
HCmean<-rowMeans(Filter(is.numeric,HC))
HC$HCmean<-HCmean
View(HC)

day4<-covidnew[,c("ï..Subject_ID","Case_1.4")]

mean_ratio_day4<-day4$Case_1.4/HC$HCmean
day4$mean_ratio_day4<-mean_ratio_day4
View(day4)


#Performing t-test
library(matrixTests)

#HC v/s covidE1
tTestResults_day4<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", 
                                   "HC88","HC91", "HC94", "HC75", "HC76")]
                             ,day4[,c("Case_1.4")])
pvalue_day4<-tTestResults_day4$pvalue
day4$pvalue_day4<-pvalue_day4
View(day4)
arranged_day4<-day4[order(day4$mean_ratio_day4),]
View(arranged_day4)
day4downgenes<- arranged_day4 %>% select("ï..Subject_ID","mean_ratio_day4","pvalue_day4") %>% filter(mean_ratio_day4<0.8 & pvalue_day4<0.05)
View(day4downgenes)
day4upgenes<- arranged_day4 %>% select("ï..Subject_ID","mean_ratio_day4","pvalue_day4") %>% filter(mean_ratio_day4>1.2 & pvalue_day4<0.05)
View(day4upgenes)

day5<-covidnew[,c("ï..Subject_ID","Case_1.5")]
mean_ratio_day5<-day5$Case_1.5/HC$HCmean
day5$mean_ratio_day5<-mean_ratio_day5
View(day5)

tTestResults_day5<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", 
                                     "HC88","HC91", "HC94", "HC75", "HC76")]
                               ,day5[,c("Case_1.5")])
pvalue_day5<-tTestResults_day5$pvalue
day5$pvalue_day5<-pvalue_day5
View(day5)
arranged_day5<-day5[order(day5$mean_ratio_day5),]
View(arranged_day5)
day5downgenes<- arranged_day5 %>% select("ï..Subject_ID","mean_ratio_day5","pvalue_day5") %>% filter(mean_ratio_day5<0.8 & pvalue_day5<0.05)
View(day5downgenes)
day5upgenes<- arranged_day5 %>% select("ï..Subject_ID","mean_ratio_day5","pvalue_day5") %>% filter(mean_ratio_day5>1.2 & pvalue_day5<0.05)
View(day5upgenes)
day5downgenes$ï..Subject_ID
day5upgenes$ï..Subject_ID


day6<-covidnew[,c("ï..Subject_ID","Case_1.6","Case_2.6")]

day6mean<-rowMeans(Filter(is.numeric,day6))
day6$day6mean <- day6mean
mean_ratio_day6<-day6$day6mean/HC$HCmean
day6$mean_ratio_day6<-mean_ratio_day6
View(day6)

tTestResults_day6<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", 
                                     "HC88","HC91", "HC94", "HC75", "HC76")]
                               ,day6[,c("Case_1.6", "Case_2.6")])
pvalue_day6<-tTestResults_day6$pvalue
day6$pvalue_day6<-pvalue_day6
View(day6)
arranged_day6<-day6[order(day6$mean_ratio_day6),]
View(arranged_day6)
day6downgenes<- arranged_day6 %>% select("ï..Subject_ID","mean_ratio_day6","pvalue_day6") %>% filter(mean_ratio_day6<0.8 & pvalue_day6<0.05)
View(day6downgenes)
day6upgenes<- arranged_day6 %>% select("ï..Subject_ID","mean_ratio_day6","pvalue_day6") %>% filter(mean_ratio_day6>1.2 & pvalue_day6<0.05)
View(day6upgenes)
day6downgenes$ï..Subject_ID
day6upgenes$ï..Subject_ID

#DAY 7
day7<-covid[,c("ï..Subject_ID","Case_1.7","Case_2.7")]

day7mean<-rowMeans(Filter(is.numeric,day7))
day7$day7mean <- day7mean
mean_ratio_day7<-day7$day7mean/HC$HCmean
day7$mean_ratio_day7<-mean_ratio_day7
View(day7)

tTestResults_day7<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", 
                                     "HC88","HC91", "HC94", "HC75", "HC76")]
                               ,day7[,c("Case_1.7", "Case_2.7")])
pvalue_day7<-tTestResults_day7$pvalue
day7$pvalue_day7<-pvalue_day7
View(day7)
arranged_day7<-day7[order(day7$mean_ratio_day7),]
View(arranged_day7)
library(dplyr)
day7downgenes<- arranged_day7 %>% select("ï..Subject_ID","mean_ratio_day7","pvalue_day7") %>% filter(mean_ratio_day7<0.8 & pvalue_day7<0.05)
View(day7downgenes)
day7upgenes<- arranged_day7 %>% select("ï..Subject_ID","mean_ratio_day7","pvalue_day7") %>% filter(mean_ratio_day7>1.2 & pvalue_day7<0.05)
View(day7upgenes)
day7downgenes$ï..Subject_ID
day7upgenes$ï..Subject_ID


#DAY 8
day8<-covid[,c("ï..Subject_ID","Case_1.8","Case_2.8")]

day8mean<-rowMeans(Filter(is.numeric,day8))
day8$day8mean <- day8mean
mean_ratio_day8<-day8$day8mean/HC$HCmean
day8$mean_ratio_day8<-mean_ratio_day8
View(day8)

tTestResults_day8<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day8[,c("Case_1.8", "Case_2.8")])
pvalue_day8<-tTestResults_day8$pvalue
day8$pvalue_day8<-pvalue_day8
View(day8)
arranged_day8<-day8[order(day8$mean_ratio_day8),]
View(arranged_day8)
library(dplyr)
day8downgenes<- arranged_day8 %>% select("ï..Subject_ID","mean_ratio_day8","pvalue_day8") %>% filter(mean_ratio_day8<0.8 & pvalue_day8<0.05)
View(day8downgenes)
day8upgenes<- arranged_day8 %>% select("ï..Subject_ID","mean_ratio_day8","pvalue_day8") %>% filter(mean_ratio_day8>1.2 & pvalue_day8<0.05)
View(day8upgenes)
day8downgenes$ï..Subject_ID
day8upgenes$ï..Subject_ID


#DAY 9

day9<-covid[,c("ï..Subject_ID","Case_1.9","Case_2.9","Case_3.9")]

day9mean<-rowMeans(Filter(is.numeric,day9))
day9$day9mean <- day9mean
mean_ratio_day9<-day9$day9mean/HC$HCmean
day9$mean_ratio_day9<-mean_ratio_day9
View(day9)

tTestResults_day9<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day9[,c("Case_1.9", "Case_2.9","Case_3.9")])
pvalue_day9<-tTestResults_day9$pvalue
day9$pvalue_day9<-pvalue_day9
View(day9)
arranged_day9<-day9[order(day9$mean_ratio_day9),]
View(arranged_day9)
library(dplyr)
day9downgenes<- arranged_day9 %>% select("ï..Subject_ID","mean_ratio_day9","pvalue_day9") %>% filter(mean_ratio_day9<0.8 & pvalue_day9<0.05)
View(day9downgenes)
day9upgenes<- arranged_day9 %>% select("ï..Subject_ID","mean_ratio_day9","pvalue_day9") %>% filter(mean_ratio_day9>1.2 & pvalue_day9<0.05)
View(day9upgenes)
day9downgenes$ï..Subject_ID
day9upgenes$ï..Subject_ID

#DAY 10

day10<-covid[,c("ï..Subject_ID","Case_2.10","Case_3.10")]

day10mean<-rowMeans(Filter(is.numeric,day10))
day10$day10mean <- day10mean
mean_ratio_day10<-day10$day10mean/HC$HCmean
day10$mean_ratio_day10<-mean_ratio_day10
View(day10)

tTestResults_day10<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day10[,c("Case_2.10","Case_3.10")])
pvalue_day10<-tTestResults_day10$pvalue
day10$pvalue_day10<-pvalue_day10
View(day10)
arranged_day10<-day10[order(day10$mean_ratio_day10),]
View(arranged_day10)
library(dplyr)
day10downgenes<- arranged_day10 %>% select("ï..Subject_ID","mean_ratio_day10","pvalue_day10") %>% filter(mean_ratio_day10<0.8 & pvalue_day10<0.05)
View(day10downgenes)
day10upgenes<- arranged_day10 %>% select("ï..Subject_ID","mean_ratio_day10","pvalue_day10") %>% filter(mean_ratio_day10>1.2 & pvalue_day10<0.05)
View(day10upgenes)
day10downgenes$ï..Subject_ID
day10upgenes$ï..Subject_ID

#DAY 11

day11<-covid[,c("ï..Subject_ID","Case_1.11","Case_2.11","Case_3.11")]

day11mean<-rowMeans(Filter(is.numeric,day11))
day11$day11mean <- day11mean
mean_ratio_day11<-day11$day11mean/HC$HCmean
day11$mean_ratio_day11<-mean_ratio_day11
View(day11)

tTestResults_day11<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day11[,c("Case_1.11", "Case_2.11","Case_3.11")])
pvalue_day11<-tTestResults_day11$pvalue
day11$pvalue_day11<-pvalue_day11
View(day11)
arranged_day11<-day11[order(day11$mean_ratio_day11),]
View(arranged_day11)
library(dplyr)
day11downgenes<- arranged_day11 %>% select("ï..Subject_ID","mean_ratio_day11","pvalue_day11") %>% filter(mean_ratio_day11<0.8 & pvalue_day11<0.05)
View(day11downgenes)
day11upgenes<- arranged_day11 %>% select("ï..Subject_ID","mean_ratio_day11","pvalue_day11") %>% filter(mean_ratio_day11>1.2 & pvalue_day11<0.05)
View(day11upgenes)
day11downgenes$ï..Subject_ID
day11upgenes$ï..Subject_ID

#DAY 12

day12<-covid[,c("ï..Subject_ID","Case_1.12","Case_2.12","Case_3.12")]

day12mean<-rowMeans(Filter(is.numeric,day12))
day12$day12mean <- day12mean
mean_ratio_day12<-day12$day12mean/HC$HCmean
day12$mean_ratio_day12<-mean_ratio_day12
View(day12)

tTestResults_day12<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day12[,c("Case_1.12", "Case_2.12","Case_3.12")])
pvalue_day12<-tTestResults_day12$pvalue
day12$pvalue_day12<-pvalue_day12
View(day12)
arranged_day12<-day12[order(day12$mean_ratio_day12),]
View(arranged_day12)
library(dplyr)
day12downgenes<- arranged_day12 %>% select("ï..Subject_ID","mean_ratio_day12","pvalue_day12") %>% filter(mean_ratio_day12<0.8 & pvalue_day12<0.05)
View(day12downgenes)
day12upgenes<- arranged_day12 %>% select("ï..Subject_ID","mean_ratio_day12","pvalue_day12") %>% filter(mean_ratio_day12>1.2 & pvalue_day12<0.05)
View(day12upgenes)
day12downgenes$ï..Subject_ID
day12upgenes$ï..Subject_ID

#DAY 13

day13<-covid[,c("ï..Subject_ID","Case_2.13")]

day13mean<-rowMeans(Filter(is.numeric,day13))
day13$day13mean <- day13mean
mean_ratio_day13<-day13$day13mean/HC$HCmean
day13$mean_ratio_day13<-mean_ratio_day13
View(day13)

tTestResults_day13<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day13[,c("Case_2.13")])
pvalue_day13<-tTestResults_day13$pvalue
day13$pvalue_day13<-pvalue_day13
View(day13)
arranged_day13<-day13[order(day13$mean_ratio_day13),]
View(arranged_day13)
library(dplyr)
day13downgenes<- arranged_day13 %>% select("ï..Subject_ID","mean_ratio_day13","pvalue_day13") %>% filter(mean_ratio_day13<0.8 & pvalue_day13<0.05)
View(day13downgenes)
day13upgenes<- arranged_day13 %>% select("ï..Subject_ID","mean_ratio_day13","pvalue_day13") %>% filter(mean_ratio_day13>1.2 & pvalue_day13<0.05)
View(day13upgenes)
day13downgenes$ï..Subject_ID
day13upgenes$ï..Subject_ID

#DAY 18

day18<-covid[,c("ï..Subject_ID","Case_1.18")]

day18mean<-rowMeans(Filter(is.numeric,day18))
day18$day18mean <- day18mean
mean_ratio_day18<-day18$day18mean/HC$HCmean
day18$mean_ratio_day18<-mean_ratio_day18
View(day18)

tTestResults_day18<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day18[,c("Case_1.18")])
pvalue_day18<-tTestResults_day18$pvalue
day18$pvalue_day18<-pvalue_day18
View(day18)
arranged_day18<-day18[order(day18$mean_ratio_day18),]
View(arranged_day18)
library(dplyr)
day18downgenes<- arranged_day18 %>% select("ï..Subject_ID","mean_ratio_day18","pvalue_day18") %>% filter(mean_ratio_day18<0.8 & pvalue_day18<0.05)
View(day18downgenes)
day18upgenes<- arranged_day18 %>% select("ï..Subject_ID","mean_ratio_day18","pvalue_day18") %>% filter(mean_ratio_day18>1.2 & pvalue_day18<0.05)
View(day18upgenes)
day18downgenes$ï..Subject_ID
day18upgenes$ï..Subject_ID

#DAY 19

day19<-covid[,c("ï..Subject_ID","Case_2.19")]

day19mean<-rowMeans(Filter(is.numeric,day19))
day19$day19mean <- day19mean
mean_ratio_day19<-day19$day19mean/HC$HCmean
day19$mean_ratio_day19<-mean_ratio_day19
View(day19)

tTestResults_day19<-row_t_welch(HC[,c("HC59", "HC60",	"HC68", "HC98", "HC74", "HC88",
                                     "HC91", "HC94", "HC75", "HC76")]
                               ,day19[,c("Case_2.19")])
pvalue_day19<-tTestResults_day19$pvalue
day19$pvalue_day19<-pvalue_day19
View(day19)
arranged_day19<-day19[order(day19$mean_ratio_day19),]
View(arranged_day19)
library(dplyr)
day19downgenes<- arranged_day19 %>% select("ï..Subject_ID","mean_ratio_day19","pvalue_day19") %>% filter(mean_ratio_day19<0.8 & pvalue_day19<0.05)
View(day19downgenes)
day19upgenes<- arranged_day19 %>% select("ï..Subject_ID","mean_ratio_day19","pvalue_day19") %>% filter(mean_ratio_day19>1.2 & pvalue_day19<0.05)
View(day19upgenes)
day19downgenes$ï..Subject_ID
day19upgenes$ï..Subject_ID

View(day4downgenes$ï..Subject_ID)
View(day4upgenes$ï..Subject_ID)
View(day5downgenes$ï..Subject_ID)
View(day5upgenes$ï..Subject_ID)
View(day6downgenes$ï..Subject_ID)
View(day6upgenes$ï..Subject_ID)
View(day7downgenes$ï..Subject_ID)
View(day7upgenes$ï..Subject_ID)
View(day8downgenes$ï..Subject_ID)
View(day8upgenes$ï..Subject_ID)
View(day9downgenes$ï..Subject_ID)
View(day9upgenes$ï..Subject_ID)
View(day10downgenes$ï..Subject_ID)
View(day10upgenes$ï..Subject_ID)
View(day11downgenes$ï..Subject_ID)
View(day11upgenes$ï..Subject_ID)
View(day12downgenes$ï..Subject_ID)
View(day12upgenes$ï..Subject_ID)
View(day13downgenes$ï..Subject_ID)
View(day13upgenes$ï..Subject_ID)
View(day18downgenes$ï..Subject_ID)
View(day18upgenes$ï..Subject_ID)
View(day19downgenes$ï..Subject_ID)
View(day19upgenes$ï..Subject_ID)


# Drug-gene relationship
BiocManager::install("rDGIdb")
??rDGIdb
library(rDGIdb)
genes <- c("LGALS3","CD209","IFNG","CXCL10","IL6R","CD274","PPARG","IL10","IL4","IL21","CCL2",
           "SPP1","IL22","SOCS1","HLA-DQA1","LTA","TLR3","IL6","NOS2","HLA-DQB1","IL23A",
           "EBI3","PDCD1","IL1B","IFNA2","CDKN1A","CD40","MBL2","PDCD1LG2","IL12B","CCR1",
           "IL2","FAS")
result <- queryDGIdb(genes = genes,
                     sourceDatabases = c("DrugBank"))
View(result)
View(result@detailedResults[["Drug"]])
View(result@resultSummary)

plot(result@resultSummary)

#VennDiagrams
#HepB
HepB<-read.csv("C:\\Users\\Rhea Arora\\Documents\\Thesis\\New\\coronavirus\\HepB.csv")
HepBgenes<-HepB$Symbol
#HepC
HepC<-read.csv("C:\\Users\\Rhea Arora\\Documents\\Thesis\\New\\coronavirus\\HepC.csv")
View(HepC)
HepCgenes<-HepC$Symbol
#RA
RA<-read.csv("C:\\Users\\Rhea Arora\\Documents\\Thesis\\New\\coronavirus\\RA.csv")
View(RA)
RAgenes<-RA$Symbol
#COVID-19 dysreg genes
COV<-read.csv("C:\\Users\\Rhea Arora\\Documents\\coronanew\\allgenes.csv")
View(COV)
COVgenes<-COV$All
View(COV$All)

require(VennDiagram)
vp1 <- venn.diagram(list(" "=COVgenes,"  "= HepBgenes)
                    ,fill=2:3, alpha = 0.5, cex=2.7, cat.cex=1.4, filename = NULL)
grid.draw(vp1)

#VD for hepC
vp2 <- venn.diagram(list("  "=COVgenes," "= HepCgenes)
                    ,fill=c("red","blue"), alpha = 0.5, cex=2.7, cat.cex=1.4, filename = NULL)
grid.draw(vp2)

#VD for RA
vp3 <- venn.diagram(list("  "=COVgenes," "= RAgenes)
                    ,fill=c("red","yellow"), alpha = 0.5, cex=2.7, cat.cex=1.4, filename = NULL)
grid.draw(vp3)

library(data.table)
allgenesmeans<-data.table(day4$ï..Subject_ID, day4$Case_1.4, day5$Case_1.5, day6$day6mean, day7$day7mean,
                     day8$day8mean, day9$day9mean, day10$day10mean, day11$day11mean,
                     day12$day12mean, day13$day13mean, day18$day18mean)
View(allgenesmeans)
allgenes<-data.table(day4$ï..Subject_ID, day4$mean_ratio_day4, day4$pvalue_day4,
                          day5$mean_ratio_day5, day5$pvalue_day5, day6$mean_ratio_day6,
                          day6$pvalue_day6, day7$mean_ratio_day7, day7$pvalue_day7,
                     day8$mean_ratio_day8, day8$pvalue_day8, day9$mean_ratio_day9,
                    day9$pvalue_day9,  day10$mean_ratio_day10, day10$pvalue_day10,
                    day11$mean_ratio_day11, day11$pvalue_day11, day12$mean_ratio_day12,
                  day12$pvalue_day12, day13$mean_ratio_day13, day13$pvalue_day13,
                  day18$mean_ratio_day18, day18$pvalue_day18)
View(allgenes)
commongenesratio<-allgenes[c(178, 457,534, 305, 109, 444, 278, 100, 299, 204),]
View(commongenesratio)
citation(package = "rDGIdb")