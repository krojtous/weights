#Name: ELECTION MODEL
#Date: 31.1.2017
#Author: Matouš Pilnáček - Public Opinion Research Centre, Czech Academy of Science
#E-mail: matous.pilnacek@soc.cas.cz
#Description: Basic election model

#----------------------Example of use---------------------------
# library(foreign)
# cvvm = read.spss(file = "./../../../Data/Nase_spolecnost/1701/Data/NS_1701_DataFile-FINAL.sav",
#                  to.data.frame = TRUE,
#                  use.value.labels = FALSE,
#                  use.missings = FALSE,
#                  reencode = 'utf-8')
# View(tableRel(cvvmModel(cvvm), labels = TRUE))

#####################################################################
#------------------------CvvmModelAbs--------------------------------
cvvmModel = function(cvvm){
  #Desc: make vector of respondents used in basic election model
  #Input:
     #Data -SPSS data frame from survey Nase spolecnost in native format (variables with dots)
  #Output: vector "model" where is only responses of likely voters
  
  cvvm$model = cvvm[,"PV.4"] #make new variable
  cvvm[cvvm$PV.1 != 1 & cvvm$PV.1 != 2, "model"] = NA # remove all respondests who wouldnt probably vote
  
  #remove other responses from data
  cvvm[ cvvm$model == 15 | #no party/zadna strana 
        cvvm$model == 0  | #without response/bez odpovedi
        cvvm$model == 99 | #dont know/nevi
        cvvm$model == 97 | #refused to answet/odmitl odpovedet
        cvvm$model == 95 | #other reposnes/ostatni vyroky
        is.na(cvvm$model), 
        "model"] = NA
  return(cvvm$model)
}



##################################################################
#------------------------tableRel---------------------------------
tableRel = function(variable, labels = FALSE){
  #Desc:makes from absolute count relative and makes dataframe with three colums ([label], val, abs, rel)
  #Input: 
    #variable - vector/factor varible
    #labels - TRUE/FALSE - if true will extract labels of variable and append them to table
  #Output: data frame with absolute and relative freq of varaible
  table = as.data.frame(table(variable))
  table = cbind(table, round( table$Freq/sum(table$Freq)*100,1 ))
  names(table) = c("val","abs","rel")
  if(labels == TRUE){
    labels = as.data.frame(cbind(attr(variable, "value.labels"), names(attr(variable, "value.labels"))))
    names(labels) = c("val", "label")
    table = merge(labels, table)
  }
  return(table)
}







