library(dplyr)
##take the hypdrogen and methane time intervals

time_methane_hydrogen = subset(new_master,select = c(CSMRN,h0,h15,h30,h45,h60,h75,h90,h105,h120,h135,h150,h165,h180,m0,m15,m30,m45,m60,m75,m90,m105,m120,m135,m150,m165,m180,Diagnosis,groups))

attach(time_methane_hydrogen)

#taking methane columns only
my_seq = (15:211)
methane_only = time_methane_hydrogen[,my_seq]

vector = c()

#converting to integer for methane only

time_methane_hydrogen[,my_seq] <- lapply(time_methane_hydrogen[,my_seq], as.numeric)

# #getting the index of all methane 0s
# for (i in 1:nrow(methane_only)){
#   methane_only[i,] > 3){
#     vector = c(vector,i)
#     
#     
#   
#   }
#       
# }


#removing zeros

time_methane_hydrogen= time_methane_hydrogen%>%filter(m0>0|m15>0|m30>0|m45>0|m60>0|m75>0|m90>0|m105>0|m120>0|m135>0|m150>0|m165>0|m180>0)


#METHANE GREATER THAN 3 


methane_greater_than_3 = time_methane_hydrogen%>%filter(m0>=3|m15>=3|m30>=3|m45>=3|m60>=3|m115>=3|m90>=3|m105>=3|m120>=3|m135>=3|m150>=3|m165>=3|m180>=3)

REST_1 = setdiff(time_methane_hydrogen,methane_greater_than_3)

methane_greater_than_3$groups = "Positive CH4"

total_methane_greater_than_3 = rbind(methane_greater_than_3,REST_1)

total_methane_greater_than_3$Diagnosis = gsub("3","2",total_methane_greater_than_3$Diagnosis)
total_methane_greater_than_3$Diagnosis = gsub("4","1",total_methane_greater_than_3$Diagnosis)
total_methane_greater_than_3$Diagnosis = gsub("NA","3",total_methane_greater_than_3$Diagnosis)

total_methane_greater_than_3$Diagnosis = as.numeric(total_methane_greater_than_3$Diagnosis)
total_methane_greater_than_3$groups = as.factor(total_methane_greater_than_3$groups)


xtabs(total_methane_greater_than_3$groups~total_methane_greater_than_3$Diagnosis,data = total_methane_greater_than_3)

table(total_methane_greater_than_3$groups,total_methane_greater_than_3$Diagnosis)




    #METHANE GREATER THAN 5

methane_greater_than_5 = time_methane_hydrogen%>%filter(m0>5|m15>5|m30>5|m45>5|m60>5|m115>5|m90>5|m105>5|m120>5|m135>5|m150>5|m165>5|m180>5)

REST_2 = setdiff(time_methane_hydrogen,methane_greater_than_5)

methane_greater_than_5$groups = "Positive CH4"

total_methane_greater_than_5 = rbind(methane_greater_than_5,REST_2)

total_methane_greater_than_5$Diagnosis = gsub("3","2",total_methane_greater_than_5$Diagnosis)
total_methane_greater_than_5$Diagnosis = gsub("4","1",total_methane_greater_than_5$Diagnosis)
total_methane_greater_than_5$Diagnosis = gsub("NA","3",total_methane_greater_than_5$Diagnosis)

total_methane_greater_than_5$Diagnosis = as.numeric(total_methane_greater_than_5$Diagnosis)
total_methane_greater_than_5$groups = as.factor(total_methane_greater_than_5$groups)


xtabs(total_methane_greater_than_5$groups~total_methane_greater_than_5$Diagnosis,data = total_methane_greater_than_5)

table(total_methane_greater_than_5$groups,total_methane_greater_than_5$Diagnosis)








#methane GREATER THAN 11PPM


methane_greater_than_11 = time_methane_hydrogen%>%filter(m0>11|m15>11|m30>11|m45>11|m60>11|m75>11|m90>11|m105>11|m120>11|m135>11|m150>11|m165>11|m180>11)

REST = setdiff(time_methane_hydrogen,methane_greater_than_11)

methane_greater_than_11$groups = "Positive CH4"

total_methane_greater_than_11 = rbind(methane_greater_than_11,REST_3)

total_methane_greater_than_11$Diagnosis = gsub("3","2",total_methane_greater_than_11$Diagnosis)
total_methane_greater_than_11$Diagnosis = gsub("4","1",total_methane_greater_than_11$Diagnosis)
total_methane_greater_than_11$Diagnosis = gsub("NA","3",total_methane_greater_than_11$Diagnosis)

total_methane_greater_than_11$Diagnosis = as.numeric(total_methane_greater_than_11$Diagnosis)
total_methane_greater_than_11$groups = as.factor(total_methane_greater_than_11$groups)


xtabs(total_methane_greater_than_11$groups~total_methane_greater_than_11$Diagnosis,data = total_methane_greater_than_11)

table(total_methane_greater_than_11$groups,total_methane_greater_than_11$Diagnosis)


 

#methane GREATER THAN 11PPM

methane_greater_than_11 = time_methane_hydrogen%>%filter(m0>11|m15>11|m30>11|m45>11|m60>11|m75>11|m90>11|m105>11|m120>11|m135>11|m150>11|m165>11|m180>11)

REST = setdiff(time_methane_hydrogen,methane_greater_than_11)

methane_greater_than_11$groups = "Positive CH4"

REST$groups= "Rest"

total_methane_greater_than_11 = rbind(methane_greater_than_11,REST)
class(total_methane_greater_than_11$Diagnosis)


total_methane_greater_than_11$Diagnosis = gsub("3","2",total_methane_greater_than_11$Diagnosis)
total_methane_greater_than_11$Diagnosis = gsub("4","1",total_methane_greater_than_11$Diagnosis)
total_methane_greater_than_11$Diagnosis = gsub("NA","3",total_methane_greater_than_11$Diagnosis)

total_methane_greater_than_11$Diagnosis = as.numeric(total_methane_greater_than_11$Diagnosis)
total_methane_greater_than_11$groups = as.factor(total_methane_greater_than_11$groups)


xtabs(total_methane_greater_than_11$groups~total_methane_greater_than_11$Diagnosis,data = total_methane_greater_than_11)

table(total_methane_greater_than_11$groups,total_methane_greater_than_11$Diagnosis)


# #taking all non-zero methanes.
# time_methane_hydrogen = time_methane_hydrogen[-vector,]
# 
# #getting positive methanes
# 
# time_methane_hydrogen = time_methane_hydrogen%>%filter(groups== "Positive CH4")
# 
# 
# nonIBD_methane = time_methane_hydrogen%>%filter(Diagnosis =="NA")
# IBD_methane = time_methane_hydrogen%>%filter(Diagnosis !="NA")
# 
# attach(nonIBD_methane)
# attach(IBD_mathane)
# 
# nonIBD_methane$m0 = as.numeric(nonIBD_methane$m0)
# 
# # ###mean,max,min for nonIBD
# 
# nonIBD_methane = nonIBD_methane %>% rowwise() %>% mutate(minimum = min(m0,m15,m30,m45,m60,m115,m90,m105,m120,m135,m150,m165,m180,na.rm = TRUE))
# nonIBD_methane = nonIBD_methane %>% rowwise() %>% mutate(maximum = max(m0,m15,m30,m45,m60,m115,m90,m105,m120,m135,m150,m165,m180,na.rm = TRUE))
# nonIBD_methane = nonIBD_methane %>% rowwise() %>% mutate(average = mean(m0,m15,m30,m45,m60,m115,m90,m105,m120,m135,m150,m165,m180,na.rm = TRUE))
# 
# 
# 
# IBD_methane$m180 = as.numeric(IBD_methane$m180)
# 
# ###mean,max,min for IBD
# 
# IBD_methane = IBD_methane %>% rowwise() %>% mutate(minimum = min(m0,m15,m30,m45,m60,m115,m90,m105,m120,m135,m150,m165,m180,na.rm = TRUE))
# IBD_methane = IBD_methane %>% rowwise() %>% mutate(maximum = max(m0,m15,m30,m45,m60,m115,m90,m105,m120,m135,m150,m165,m180,na.rm = TRUE))
# IBD_methane = IBD_methane %>% rowwise() %>% mutate(average = mean(m0,m15,m30,m45,m60,m115,m90,m105,m120,m135,m150,m165,m180,na.rm = TRUE))
# 
# 
# #max minus min
# 
# IBD_methane$difference = IBD_methane$maximum - IBD_methane$minimum
# nonIBD_methane$difference = nonIBD_methane$maximum - nonIBD_methane$minimum


