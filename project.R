setwd("C:/Users/MathurS1/Desktop/Breath_Test/")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("plotrix")
install.packages("plotly")
install.packages("ggpubr")
library(readxl)
library(ggpubr)
library(dplyr)
#reading in the breath test dataset
breathtestdb= read.csv("Breath_test_database_Xiaochen_2018.csv",sep = ',',header = T)

attach(breathtestdb)

breathtestdb[(breathtestdb$CSMRN =="41568146"),]

sub_16 = read_xlsx("new_copy_with_20 values.xlsx")
sub_16[(sub_16$CSMRN == "a41568146"),]

additions = sub_16%>%
  filter(sub_16$CSMRN %in% c("001444656","011532139", "020870855", "030433696", "031249207" ,"031256300","040246587", "041475458" , "070936181","090295645", "100015809" ,"100081364" ,"100094292","100100240", "100259239", "200274243","a41568146"  ))
         
breathtestdb = rbind(breathtestdb,additions)


csmrn_2p = breathtestdb%>%
  filter(breathtestdb$CSMRN %in% c("051310313","011300452"))

a = breathtestdb$CSMRN
write.table(breathtestdb,"model.txt",sep = "\t")

write.csv(breathtestdb,"breathtestDBFINAL.csv",sep = ',')


breathtestdb = read.csv("breathtestDBFINAL.csv",sep = ',',header = T)
# breathtestdb = breathtestdb[order(breathtestdb$CSMRN),]
# View(breathtestdb)
# column1 = breathtestdb$CSMRN

# char = as.data.frame(as.character(breathtestdb$CSMRN))
# # 
# char = as.data.frame(formatC(char$`as.character(breathtestdb$CSMRN)`, width = 9, format = "d", flag = "0"))

#reading in the icd tables and combining both the parts
icdpart1 = read.csv("ICD 7.26.16 part 1.csv",sep = ',',header = T)
icdpart1$CSMRN = formatC(icdpart1$CSMRN,width = 9,format = "d",flag = "0")
icdpart2 = read.csv("ICD 7.26.16 part 2.csv",sep = ',',header = T)
icdpart2$CSMRN = formatC(icdpart2$CSMRN,width = 9,format = "d",flag = "0")
combinedicd = rbind(icdpart1,icdpart2)

#reading in the bmi data
bmi = read.csv("BMI 7.26.16.csv",sep = ',',header = T)
bmi$CSMRN = formatC(bmi$CSMRN,width = 9,format = "d",flag = "0")

#joining bmi and breathtest first
class(breathtestdb$CSMRN)
bmi$CSMRN = as.factor(bmi$CSMRN)
class(bmi$CSMRN)
BTBMI = merge(breathtestdb,bmi, by = "CSMRN",all.x = TRUE)

#checking for non NA values
BTBMI %>%
  dplyr::select(GAP_DAYS)%>%
  filter(!is.na(GAP_DAYS))

#changing format of date for bmi testdate
BTBMI$TESTDATE.y = format(as.POSIXct(BTBMI$TESTDATE.y,format = '%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')

# BTBMI %>%
#   select(TESTDATE.y)%>%
#   filter(!is.na(TESTDATE.y))

BTBMI = BTBMI[order(BTBMI$CSMRN),]

#subtracting the 2 columns
BTBMI$TESTDATE.x = as.Date(BTBMI$TESTDATE.x,"%m/%d/%Y")
BTBMI$BMI_DATE = as.Date(BTBMI$BMI_DATE,"%m/%d/%Y")
BTBMI$Datedifference = abs((BTBMI$TESTDATE.x) - (BTBMI$BMI_DATE))



# not_duplicate = BTBMI[!duplicated(BTBMI$CSMRN),]
# 
# duplicate_values = setdiff(BTBMI,not_duplicate)
# # dupl = BTBMI[duplicated(BTBMI$CSMRN),]
# # attach(dupl)
# # 
# # library(tidyr)
# # 
# duplicate_values = tail(duplicate_values,-6796)
# # 
# # # completeFun <- function(data, desiredCols) {
# # #   completeVec <- complete.cases(data[, desiredCols])
# # #   return(data[completeVec, ])
# # # }
# # # dupl = completeFun(dupl,"CSMRN")
# # dupl$CSMRN = as.numeric(dupl$CSMRN)
# # dupl = dupl[order(dupl$CSMRN),]

#generating the duplicate values and taking the one that is closest to the difference of date
tt = table(BTBMI$CSMRN)
DUPLICATES <- subset(BTBMI, CSMRN %in% names(tt[tt > 1]))
WITHOUT_na_DUPLICATES = tail(DUPLICATES,-6797)
WITHOUT_na_DUPLICATES = WITHOUT_na_DUPLICATES[order(WITHOUT_na_DUPLICATES$Datedifference),]
WITHOUT_na_DUPLICATES = WITHOUT_na_DUPLICATES[!duplicated(WITHOUT_na_DUPLICATES$CSMRN),]


#taking the ones with 1 CSMRNs
singleCSMRNs = subset(BTBMI, CSMRN %in% names(tt[tt== 1]))

#taking the ones with no CSMRNs
noCSMRNs = head(DUPLICATES,6797)


#combining all dataframes
first = rbind(noCSMRNs,WITHOUT_na_DUPLICATES)

COMBINED_INITIAL = rbind(singleCSMRNs,first)

#GETTING THE FINAL TABLE

FINAL_DF = merge(COMBINED_INITIAL,combinedicd,by = "CSMRN",all.x = TRUE)
FINAL_DF_UNIQUE = FINAL_DF[!duplicated(FINAL_DF$CSMRN),]

View(FINAL_DF_UNIQUE[(FINAL_DF_UNIQUE$CSMRN =="001444656"),])

#IBD MASTER FILE

IBD_FILE = read.csv("breath_test_IBD_version2_master.csv",sep = ',',header = T)
# IBD_FILE$MRN = formatC(IBD_FILE$MRN , width = 9,format = "d",flag = "0")
colnames(IBD_FILE)[1] <- "CSMRN"
MASTER = merge(FINAL_DF_UNIQUE,IBD_FILE,by = "CSMRN",all.x = TRUE,all.y = TRUE)

write.csv(MASTER,"MASTER.csv")
write.table(MASTER,"MASTER.txt",sep ="\t")

a = MASTER$CSMRN

b = IBD_FILE$CSMRN
class(b)

d = demo$CSMRN


a= as.vector(MASTER$CSMRN)
b = as.vector(demo$CSMRN)

setdiff(b,a)


table(demo$Diagnosis)
demo = merge(FINAL_DF_UNIQUE,IBD_FILE,by = "CSMRN",all.x = TRUE,all.y = TRUE)



View(head(MASTER))


for(unique_code in unique(MASTER$Diagnosis)){
  
  MASTER[paste("disease", unique_code, sep = ".")] <- ifelse(MASTER$Diagnosis == unique_code, 1, 0)
}

View(head(MASTER,50))





















#Exploratory analysis
install.packages("ggplot2")
library(ggplot2)
#hist for gender
ggplot(MASTER, aes(x = MASTER$Gender.x)) + geom_bar(stat="count")

attach(MASTER)

# aw = MASTER$sugar
# aw = as.list(aw)
# for(x in aw){
#   if (x=="lactulose10g"){
#     rm(x)
#   }
#   else
# }


#methane production
g <- ggplot(MASTER, aes(zero_methane_producer))
g + geom_bar(aes(fill=zero_methane_producer), width = 0.5) + 
  labs(title="Histogram on zero methane production") 


#methanemax
ggplot(data=MASTER, aes(methaneMAX)) + 
  geom_histogram(breaks=seq(20, 50, by =2), 
                 col="red", 
                 aes(fill=..count..))


#FLATLINERS
g <- ggplot(MASTER, aes(zero_methane_producer))
g + geom_bar(aes(fill=zero_methane_producer), width = 0.5) + 
  labs(title="Histogram on zero methane production") 


#FLATLINERS
ggplot(data=MASTER, aes(flatliner)) + 
  geom_bar(col="red", 
                 aes(fill=..count..))


#AGE
ggplot(data=MASTER, aes(age)) + 
  geom_histogram(breaks=seq(0, 100, by =2), 
                 col="blue",
                 aes(fill=..count..))


#groups
ggplot(MASTER, aes(x=as.factor(groups), fill=as.factor(groups) )) + geom_bar( ) +
  scale_fill_hue(c = 40)


#crohn's disease
dd=as.data.frame(table(CD))

ggplot(data = dd,aes(dd$CD)) + geom_bar( col="red",aes(fill=..count..))

View(as.data.frame(MASTER$CSMRN[which(MASTER$age>50)]))

class(MASTER$CSMRN)

write.table(MASTER, "MASTER.txt", sep="\t")

write.table(demo, "MASTER2.txt", sep = "\t")
write.csv(demo, "csvmaster.csv")

write.csv(MASTER,file="yolo.csv")

 # class(MASTER$CSMRN)
# class(FINAL_DF_UNIQUE$CSMRN)
# abc = (MASTER$CSMRN - FINAL_DF_UNIQUE$CSMRN)
# 
# list1= as.list(IBD_FILE$CSMRN)
# list2=as.list(MASTER$CSMRN)
# setdiff(list1,list2)
# a=setdiff(list1,list2)
# class(list2)
# View((which((list1 %in% list2)==TRUE)))

#barplot FOR RACE

r = as.data.frame(table(MASTER$RACE))
class(r$Freq)

ylim <- c(0, 1.2*max(r$Freq))
## Plot, and store x-coordinates of bars in xx
xx <- barplot(r$Freq, xaxt = 'n', xlab = '', width = 0.2, ylim = ylim,
               main = "Race distribution", 
               ylab = "Frequency")
## Add text at top of bars
text(x = xx, y = r$Freq, label = r$Freq, pos = 3, cex = 0.7, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=r$Var1, tick=FALSE, las=2, line=-0.9, cex.axis=0.6)

#piechart for Race
# 3D Exploded Pie Chart
library(plotrix)
slices <- r$Freq 
lbls <- r$Var1
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),radius = 1.5,
    main="Pie Chart of RACE")



library(plotly)






#BMI distribution
library(tidyverse)
MASTER %>% 
  drop_na(BMI) %>%
  ggplot(aes(x = BMI))+
  geom_histogram(stat="bin")+ labs(title="Histogram of BMI distribution") 

#ANEMIA
View(table(PCOS))
MASTER %>% 
  drop_na(ANEMIA) %>%
  ggplot(aes(x = ANEMIA))+
  geom_histogram(stat="count")+ labs(title="Histogram of BMI distribution") 


#PCOS
P= as.data.frame(table(MASTER$PCOS))
P = P[-1,]

p<-ggplot(P, aes(x=P$Var1, y=P$Freq, fill=P$Var1)) +
  geom_bar(stat="identity")+theme_minimal()
P

#Gastroesophreflux
GERd = as.data.frame(table(MASTER$Gastroesoph.reflux))
GERd = GERd[-1,]
row_to_keep = c(TRUE, FALSE, TRUE, TRUE,  FALSE, TRUE,TRUE, TRUE, TRUE, TRUE,  FALSE, TRUE,TRUE,  FALSE,TRUE)
GERd = GERd[row_to_keep,]
ggplot(GERd, aes(x=GERd$Var1, y=GERd$Freq, fill=GERd$Var1)) +
  geom_bar(stat="identity")+theme_minimal()


#CELIACS DISEASE
CELIAC = as.data.frame(table(MASTER$Celiac.disease))
remove = c(F,F,T,T,T,F,F)
CELIAC = CELIAC[remove,]
ggplot(CELIAC, aes(x=CELIAC$Var1, y=CELIAC$Freq, fill=CELIAC$Var1)) +
  geom_bar(stat="identity")+theme_minimal()


#MICROSCOPIC COLLISTIS
MC = as.data.frame(table(MASTER$Microscopic.colitis))
remove = c(F,T,F,T,F)
MC= MC[remove,]
ggplot(MC, aes(x=MC$Var1, y=MC$Freq, fill=MC$Var1)) +
  geom_bar(stat="identity")+theme_minimal()


#Achalasia
achal = as.data.frame(table(MASTER$Achalasia))
remove = c(F,T,F,T,T,F,T,T,T,T,T,F,T,T,F,T)
achal= achal[remove,]
ggplot(achal, aes(x=achal$Var1, y=achal$Freq, fill=achal$Var1)) +
  geom_bar(stat="identity")+theme_minimal() + geom_text(aes(label=achal$Freq), vjust=0.1, color="black",
                                                        position = position_dodge(1.0), size=3.5)

#CD
crohns = as.data.frame(table(MASTER$CD))
ggplot(crohns, aes(x=crohns$Var1, y=crohns$Freq, fill=crohns$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=crohns$Freq), vjust=0.1, color="black",
                                                        position = position_dodge(1.0), size=3.5)


#UC
uc = as.data.frame(table(MASTER$UC))
ggplot(uc, aes(x=uc$Var1, y=uc$Freq, fill=uc$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=uc$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)


#IBDU
ibdu = as.data.frame(table(MASTER$IBDU))
ggplot(ibdu, aes(x=ibdu$Var1, y=ibdu$Freq, fill=ibdu$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=ibdu$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)


#breast cancer 1 5 6 7 8 12
breastcancer = as.data.frame(table(MASTER$Breast.Cancer))
remove = c(F,T,T,T,F,F,F,F,T,T,T,F)
breastcancer= breastcancer[remove,]
ggplot(breastcancer, aes(x=breastcancer$Var1, y=breastcancer$Freq, fill=breastcancer$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=breastcancer$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)


#Ovarian cancer
ovariancancer = as.data.frame(table(MASTER$Ovarian.Cancer))
remove = c(F,T,F,T,F,F)
ovariancancer= ovariancancer[remove,]
ggplot(ovariancancer, aes(x=ovariancancer$Var1, y=ovariancancer$Freq, fill=ovariancancer$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=ovariancancer$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)



#COLON CANCER 1,5,7,8,10,12,13,16,17,18,21
coloncancer = as.data.frame(table(MASTER$Colon.Cancer))
remove = c(F,T,T,T,F,T,F,F,T,F,T,F,F,T,T,F,F,F,T,T,F)
coloncancer = coloncancer[remove,]
ggplot(coloncancer, aes(x=coloncancer$Var1, y=coloncancer$Freq, fill=coloncancer$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=coloncancer$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)


#high bp hypertension 1,2,3,6,7,12,15
hhyperbp = as.data.frame(table(MASTER$High.BP..Hyperten.))
remove = c(F,F,F,T,T,F,F,T,T,T,T,F,T,T,F,T)


ggplot(data=MASTER, aes(MASTER$RACE)) + 
  geom_bar(col="red", 
           aes(fill=..count..))



#high cholestrol
HC = as.data.frame(table(MASTER$High.cholesterol))
HC= HC[-1,]
ggplot(HC, aes(x=HC$Var1, y=HC$Freq, fill=HC$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=HC$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)

#ibs
IBS = as.data.frame(table(MASTER$Irritable.bowel.syndrome))
remove = c(F,F,T,T,T,T,T,F,T,T,F,T,T,F,F)
IBS = IBS[remove,]
ggplot(IBS, aes(x=HC$Var1, y=IBS$Freq, fill=IBS$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=IBS$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)

hmsib = as.data.frame(table(MASTER$H_M_SIBO))
ggplot(hmsib, aes(x=hmsib$Var1, y=hmsib$Freq, fill=hmsib$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=hmsib$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)



hsibio = as.data.frame(table(MASTER$hydrogenSIBO)) 
ggplot(hsibio, aes(x=hsibio$Var1, y=hsibio$Freq, fill=hsibio$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=hsibio$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)



#endomitriosis

Endom = as.data.frame(table(MASTER$Endometriosis))
remove = c(F,F,F,F,T,T,F,T,T)
Endom = Endom[remove,]
ggplot(Endom, aes(x=Endom$Var1, y=Endom$Freq, fill=Endom$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=Endom$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)



btgr = as.data.frame(table(MASTER$groups))
ggplot(btgr, aes(x=btgr$Var1, y=btgr$Freq, fill=btgr$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=btgr$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)



r = as.data.frame(table(master$RACE2))
remove = c(T,T,F,T,T,T,T)
r =r[remove,]

ggplot(r, aes(x=r$Var1, y=r$Freq, fill=r$Var1)) +
  geom_bar(stat="identity")+theme_bw() + geom_text(aes(label=r$Freq), vjust=0.1, color="black",
                                                   position = position_dodge(1.0), size=3.5)

