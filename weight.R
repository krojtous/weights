#Name: weight.R
#Date: 31.1.2017
#Author: Matouš Pilnáček - Public Opinion Research Centre, Czech Academy of Science
#E-mail: matous.pilnacek@soc.cas.cz
#Description: Weighting of CVVM survey of crossed age, education and sex

library(foreign)
#functions for basic CVVM model
source("model.R")

#CVVM survey data
cvvm = read.spss(file = "./../../../Data/Nase_spolecnost/1701/Data/NS_1701_DataFile-FINAL.sav",
                 to.data.frame = TRUE,
                 use.value.labels = FALSE,
                 use.missings = FALSE,
                 reencode = 'utf-8')

#Czech Statistical Office Data from census 2011
CSU = read.csv("CSU.csv", sep = ";", dec = ",")

#-----------------Set 0 to system missing----------------------------------
cvvm[cvvm$IDE.2 == 0 | is.na(cvvm$IDE.2), "IDE.2"] = NA
cvvm[cvvm$IDE.8 == 0 | is.na(cvvm$IDE.8), "IDE.8"] = NA
cvvm[cvvm$t_VZD == 0 | is.na(cvvm$t_VZD), "t_VZD"] = NA

#----------------Missing analysis-------------------------------------------
sum(is.na(cvvm$IDE.2)) #number of missing in age
sum(is.na(cvvm$IDE.8)) #number of missing in sex
sum(is.na(cvvm$t_VZD)) #number of missing in edu
sum(is.na(cvvm$t_VZD) | is.na(cvvm$IDE.8) | is.na(cvvm$IDE.2)) #number of missing in all vars

#------------------Recode age to three categories------------------------
# RECODE IDE.2 (15 thru 39=1) (40 thru 59=2) (60 thru Highest=3) INTO t_VEK_3.
cvvm$t_VEK_3 = cvvm$IDE.2
cvvm[cvvm$IDE.2 > 14 & cvvm$IDE.2 < 40, "t_VEK_3"] = 1
cvvm[cvvm$IDE.2 > 39 & cvvm$IDE.2 < 60, "t_VEK_3"] = 2
cvvm[cvvm$IDE.2 > 59, "t_VEK_3"] = 3

#------------------Make relative count of weighting categorie in survey-------
tab = as.data.frame(table(cvvm$IDE.8,cvvm$t_VZD,cvvm$t_VEK_3))
tab$relSurvey = tab$Freq/sum(tab$Freq)
names(tab) = c("IDE.8","t_VZD","t_VEK_3","absSurvey", "relSurvey")

#------------------Compute weights----------------------------------------
tab = merge(tab, CSU)
tab$weight = tab$relCSU / tab$relSurvey

View(tab)




cvvm$IDE.8


cvvm$t_VZD

View(tableRel(cvvmModel(cvvm), labels = TRUE))
