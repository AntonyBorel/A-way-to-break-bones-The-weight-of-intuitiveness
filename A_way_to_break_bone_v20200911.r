#########################################################################################
#########################################################################################
################### A WAY TO BREAK BONES? THE WEIGHT OF INTUITIVENESS ###################
# D. Vettese, T. Stavrova, A. Borel, J. Marin, M.-H. Moncel, M. Arzarello, C. Daujeard. #
#########################################################################################
#########################################################################################

## To run this code you will need to install the following packages:
install.packages("xlsx") # Install xlsx package
install.packages("corrplot") # Install corrplot package
install.packages("ggplot2") # Install ggplot2 package
install.packages("FactoMineR") # Install FactoMineR package
install.packages("car") # Install car package


#########################################################################################
# Electronic Supplementary Material 1.

# This code provides the results of the Fisher tests presented in the paper
#########################################################################################

# Make sure to have the "2020_09_07_quality_felt.xlsx" (Supporting Information 19)
# in the same folder as "A_way_to_break_bone.r".

# Construction of the script:
#     - step 1: import data of quality scale
#     - step 2: Fisher test of all Element regarding quality scale
#     - step 3: import data of felt scale
#     - step 4: Chi-square test of all Element regarding felt scale 

########################################################
# Run the necessary libraries and function
library (xlsx)
library(corrplot)

# Function to verify the Cochran's rule:
cochran.verif <- function (datatab) {
  testCoch <- datatab
  testCochtheo<-suppressWarnings(chisq.test(testCoch)$expected)
  addmargins(as.table(testCochtheo)) 
  n.cases.theo<-length(testCochtheo) 
  n.cases.inf.1<-length(which(testCochtheo<1)) 
  n.cases.inf.5<-length(which(testCochtheo<5)) 
  n.cases.entre.1et5<- length(which(testCochtheo<=5 & testCochtheo>=1)) 
  prop.Cochran<- length(which(testCochtheo>5))/n.cases.theo
  if(n.cases.inf.1>0){cat("The Cochran's rule is not respected\nThe following cells have expected frequencies under 1 :\n") ; print(which(testCochtheo<1, arr.ind=TRUE)) ; cat("\n")}
  if(n.cases.inf.1==0 & n.cases.inf.5>0 & prop.Cochran<0.8){cat("The Cochran's rule is not respected\nThe following cells have expected frequency between 1 and 5 :\n") ; print(which(testCochtheo<=5 & testCochtheo>=1, arr.ind=TRUE)) ; cat("And the proportion of expected cell frequencies under 5 is: ") ; cat(prop.Cochran, "\n")}
  if(n.cases.inf.1==0 & n.cases.inf.5>0 & prop.Cochran>=0.8){cat("The Cochran's rule is respected\nThe following cells have expected frequency between 1 and 5 :\n") ; print(which(testCochtheo<=5 & testCochtheo>=1, arr.ind=TRUE)) ; cat("And the proportion of expected cell frequencies above 5 is: ") ; cat(prop.Cochran, "\n")}
  if(n.cases.inf.5==0){cat("The Cochran's rule is respected\nNo cell have expected frequency less than 5\n")}
}
########################################################

###############################################
###### step 1 : import data #########
###############################################
quality <- read.xlsx("2020_09_07_quality_felt.xlsx", sheetName = "Quality", row.names=TRUE) # import the sheet "Quality"
str(quality)# observe the structure and verify that the import is correct

###############################################
###### Step 2: Fisher test of all Element regarding quality scale #########
###############################################
quality1<-quality[,-1] # Remove the column all. 
quality1

# Verify Cochran's rule
cochran.verif(quality1)

# Fisher test results of quality scale
fisher.test(quality1, simulate.p.value=TRUE, B = 2000)

###############################################
###### step 3 : import data #########
###############################################
felt <- read.xlsx("2020_09_07_quality_felt.xlsx", sheetName = "Felt", row.names=TRUE) # import the sheet "Felt"
str(felt)# observe the structure and verify that the import is correct

###############################################
###### step 4: Fisher test of all Element regarding felt scale #########
###############################################
# Remove the column all. 
felt1<-felt [,-1]
felt1

# Verify Cochran's rule
cochran.verif(felt1)

# Fisher test results of felt scale
fisher.test(felt1, simulate.p.value=TRUE, B = 2000)



#########################################################################################
# Electronic Supplementary Material 2.

# This code provides the results of the Spearman's correlation tests presented in the paper
# and used to build the figure of the Supporting Information 9
#########################################################################################

# Make sure to have the "Supplementary_data_15.xlsx" (Supporting Information 15)
# in the same folder as "A_way_to_break_bone.r".

# Packages needed:
# corrplot (Wei, T., Simko, V., Levy, M., Xie, Y., Jin, Y., & Zemla, J. (2017). R Package "corrplot". Statistician, 56(316), e24

# Construction of the script:
#     - step 1: import data 
#     - step 2: plot spearman correlation results (see Vettese et al. 2020).

########################################################
# Run the necessary libraries
library (xlsx)
library(corrplot)
########################################################

###############################################
###### step 1 : import data #########
###############################################
Exp1 <- read.xlsx("Supplementary_data_15.xlsx", sheetIndex=1, row.names=TRUE)
str(Exp1)# observe the structure and verify that the import is correct

# Devide the dataset by bone type
Exphum<-subset(Exp1, Exp1$Element == "Humerus")
Exprau<- subset(Exp1, Exp1$Element == "Radio-ulna")
Expfem<- subset(Exp1, Exp1$Element == "Femur")
Exptib<- subset(Exp1, Exp1$Element == "Tibia")

######################################################
########### step 2: plot spearman correlation results #####################
######################################################
###All 
#Open a new window
x11()  
#Remove the column Individual, Bone.number, Element and Efficiency.index.
Exp1b<-Exp1[,-c(1,2,3,12)] 
# Spearman correlation results on a matrix
resall<-cor(Exp1b,  method = c("spearman")) 
# Plot of Spearman correlation results matrix
corrplot(resall, type="upper",  order="original",addCoef.col=T, tl.col="black", tl.srt=45)

###Humerus 
#Open a new window
x11()  
#Remove the column Individual, Bone.number, Element and Efficiency.index.
Exphum1<-Exphum[,-c(1,2,3,12)] 
# Spearman correlation results on a matrix
reshum<-cor(Exphum1,  method = c("spearman")) 
# Plot of Spearman correlation results matrix
corrplot(reshum, type="upper",  order="original",addCoef.col=T, tl.col="black", tl.srt=45)

###Radio-ulna
#Open a new window
x11()
#Remove the column Individual, Bone.number, Element and Efficiency.index.
Exprau1<-Exprau[,-c(1,2,3,12)]
# Spearman correlation results on a matrix
resrau<-cor(Exprau1,  method = c("spearman"))
# Plot of Spearman correlation results matrix
corrplot(resrau, type="upper",  order="original",addCoef.col=T, tl.col="black", tl.srt=45)

###Femur
#Open a new window
x11()
#Remove the column Individual, Bone.number, Element and Efficiency.index.
Expfem1<-Expfem[,-c(1,2,3,12)]
# Spearman correlation results on a matrix
resfem<-cor(Expfem1,  method = c("spearman"))
# Plot of Spearman correlation results matrix
corrplot(resfem, type="upper",  order="original",addCoef.col=T, tl.col="black", tl.srt=45)

###Tibia
#Open a new window
x11()
#Remove the column Individual, Bone.number, Element and Efficiency.index.
Exptib1<-Exptib[,-c(1,2,3,12)]
# Spearman correlation results on a matrix
restib<-cor(Exptib1,  method = c("spearman"))
# Plot of Spearman correlation results matrix
corrplot(restib, type="upper", order="original",addCoef.col=T, tl.col="black", tl.srt=45)



#########################################################################################
# Electronic Supplementary Material 3.

# This code provides the results of the Mann-Whitney U test/Wilcoxon signed rank test
# presented in the paper and used to build the table of the Supporting Information 8
#########################################################################################

# Make sure to have the "2020_09_07_Try_Blow_EFI_wilcoxon.xlsx" (Supporting Information 20)
# in the same folder as "A_way_to_break_bone.r".

# Construction of the script:
#     - step 1:EFFICIENCY INDEX, 3 FIRST TRIES VS 3 LAST ONES
#     - step 2:EFFICIENCY INDEX, 5 FIRST TRIES VS 5 LAST ONES 
#     - step 3:BLOWS, 3 FIRST TRIES VS 3 LAST ONES 
#     - step 3:BLOWS, 5 FIRST TRIES VS 5 LAST ONES 


########################################################
# Run the necessary libraries
library (xlsx)
library(car)
########################################################

########################################################
##   EFFICIENCY INDEX, 3 FIRST TRIES VS 3 LAST ONES   ##

#####----------------------------------------------#####
#####               Import the data                #####
#####----------------------------------------------#####
data <- read.xlsx("2020_09_07_Try_Blow_EFI_wilcoxon.xlsx", sheetName="3_3")
data

#####----------------------------------------------#####
#####                 Divide by series             #####
#####----------------------------------------------#####

## Select 1st series
Try1 <- data[which(data$Try=="T1"),]
Try1

## Select 2nd series
Try2 <- data[which(data$Try=="T2"),]
Try2

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution:
# If p-value<0.05 then the distribution is not normal
shapiro.test(Try1$EFI) 
x11(); hist(Try1$EFI)
shapiro.test(Try2$EFI)
x11(); hist(Try2$EFI)

x11(); qqPlot(Try1$EFI)
x11(); qqPlot(Try2$EFI)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1$EFI, Try2$EFI, alternative = "two.sided")

#####----------------------------------------------#####
##### Assumptions for t-test are not respected so: #####
#####----------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1$EFI, Try2$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try from the same individual


########################################################
#####----------------------------------------------#####
#####         Divide by series and bone            #####
#####----------------------------------------------#####

## Select bones of 1st series
Try1Femur <- data[which(data$Try=="T1" & data$Bone=="Femur"),]
Try1Humerus <- data[which(data$Try=="T1" & data$Bone=="Humerus"),]
Try1RadioUlna <- data[which(data$Try=="T1" & data$Bone=="Radio-Ulna"),]
Try1Tibia <- data[which(data$Try=="T1" & data$Bone=="Tibia"),]

## Select bones of 2nd series
Try2Femur <- data[which(data$Try=="T2" & data$Bone=="Femur"),]
Try2Humerus <- data[which(data$Try=="T2" & data$Bone=="Humerus"),]
Try2RadioUlna <- data[which(data$Try=="T2" & data$Bone=="Radio-Ulna"),]
Try2Tibia <- data[which(data$Try=="T2" & data$Bone=="Tibia"),]

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution for each bone for each try:
# If significant p-value then the distribution is not normal
shapiro.test(Try1Femur$EFI) 
x11(); hist(Try1Femur$EFI)
shapiro.test(Try1Humerus$EFI) 
x11(); hist(Try1Humerus$EFI)
shapiro.test(Try1RadioUlna$EFI) 
x11(); hist(Try1RadioUlna$EFI)
shapiro.test(Try1Tibia$EFI) 
x11(); hist(Try1Tibia$EFI)

shapiro.test(Try2Femur$EFI)
x11(); hist(Try2Femur$EFI)
shapiro.test(Try2Humerus$EFI)
x11(); hist(Try2Humerus$EFI)
shapiro.test(Try2RadioUlna$EFI)
x11(); hist(Try2RadioUlna$EFI)
shapiro.test(Try2Tibia$EFI)
x11(); hist(Try2Tibia$EFI)

x11(); qqPlot(Try1Femur$EFI)
x11(); qqPlot(Try1Humerus$EFI)
x11(); qqPlot(Try1RadioUlna$EFI)
x11(); qqPlot(Try1Tibia$EFI)

x11(); qqPlot(Try2Femur$EFI)
x11(); qqPlot(Try2Humerus$EFI)
x11(); qqPlot(Try2RadioUlna$EFI)
x11(); qqPlot(Try2Tibia$EFI)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1Femur$EFI, Try2Femur$EFI, alternative = "two.sided")
var.test(Try1Humerus$EFI, Try2Humerus$EFI, alternative = "two.sided")
var.test(Try1RadioUlna$EFI, Try2RadioUlna$EFI, alternative = "two.sided")
var.test(Try1Tibia$EFI, Try2Tibia$EFI, alternative = "two.sided")

#####-----------------------------------------------------#####
##### Assumptions for t-test are not always respected so: #####
#####-----------------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1Femur$EFI, Try2Femur$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Humerus$EFI, Try2Humerus$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1RadioUlna$EFI, Try2RadioUlna$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Tibia$EFI, Try2Tibia$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual



########################################################
##   EFFICIENCY INDEX, 5 FIRST TRIES VS 5 LAST ONES   ##

#####----------------------------------------------#####
#####               Import the data                #####
#####----------------------------------------------#####
data <- read.xlsx("2020_09_07_Try_Blow_EFI_wilcoxon.xlsx", sheetName="5_5")
data

#####----------------------------------------------#####
#####                 Divide by series             #####
#####----------------------------------------------#####

## Select 1st series
Try1 <- data[which(data$Try=="T1"),]
Try1

## Select 2nd series
Try2 <- data[which(data$Try=="T2"),]
Try2

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution:
# If p-value<0.05 then the distribution is not normal
shapiro.test(Try1$EFI) 
x11(); hist(Try1$EFI)
shapiro.test(Try2$EFI)
x11(); hist(Try2$EFI)

x11(); qqPlot(Try1$EFI)
x11(); qqPlot(Try2$EFI)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1$EFI, Try2$EFI, alternative = "two.sided")

#####----------------------------------------------#####
##### Assumptions for t-test are not respected so: #####
#####----------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1$EFI, Try2$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try from the same individual


########################################################
#####----------------------------------------------#####
#####         Divide by series and bone            #####
#####----------------------------------------------#####

## Select bones of 1st series
Try1Femur <- data[which(data$Try=="T1" & data$Bone=="Femur"),]
Try1Humerus <- data[which(data$Try=="T1" & data$Bone=="Humerus"),]
Try1RadioUlna <- data[which(data$Try=="T1" & data$Bone=="Radio-Ulna"),]
Try1Tibia <- data[which(data$Try=="T1" & data$Bone=="Tibia"),]

## Select bones of 2nd series
Try2Femur <- data[which(data$Try=="T2" & data$Bone=="Femur"),]
Try2Humerus <- data[which(data$Try=="T2" & data$Bone=="Humerus"),]
Try2RadioUlna <- data[which(data$Try=="T2" & data$Bone=="Radio-Ulna"),]
Try2Tibia <- data[which(data$Try=="T2" & data$Bone=="Tibia"),]

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution for each bone for each try:
# If significant p-value then the distribution is not normal
shapiro.test(Try1Femur$EFI) 
x11(); hist(Try1Femur$EFI)
shapiro.test(Try1Humerus$EFI) 
x11(); hist(Try1Humerus$EFI)
shapiro.test(Try1RadioUlna$EFI) 
x11(); hist(Try1RadioUlna$EFI)
shapiro.test(Try1Tibia$EFI) 
x11(); hist(Try1Tibia$EFI)

shapiro.test(Try2Femur$EFI)
x11(); hist(Try2Femur$EFI)
shapiro.test(Try2Humerus$EFI)
x11(); hist(Try2Humerus$EFI)
shapiro.test(Try2RadioUlna$EFI)
x11(); hist(Try2RadioUlna$EFI)
shapiro.test(Try2Tibia$EFI)
x11(); hist(Try2Tibia$EFI)

x11(); qqPlot(Try1Femur$EFI)
x11(); qqPlot(Try1Humerus$EFI)
x11(); qqPlot(Try1RadioUlna$EFI)
x11(); qqPlot(Try1Tibia$EFI)

x11(); qqPlot(Try2Femur$EFI)
x11(); qqPlot(Try2Humerus$EFI)
x11(); qqPlot(Try2RadioUlna$EFI)
x11(); qqPlot(Try2Tibia$EFI)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1Femur$EFI, Try2Femur$EFI, alternative = "two.sided")
var.test(Try1Humerus$EFI, Try2Humerus$EFI, alternative = "two.sided")
var.test(Try1RadioUlna$EFI, Try2RadioUlna$EFI, alternative = "two.sided")
var.test(Try1Tibia$EFI, Try2Tibia$EFI, alternative = "two.sided")

#####-----------------------------------------------------#####
##### Assumptions for t-test are not always respected so: #####
#####-----------------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1Femur$EFI, Try2Femur$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Humerus$EFI, Try2Humerus$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1RadioUlna$EFI, Try2RadioUlna$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Tibia$EFI, Try2Tibia$EFI, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual



#######################################################
##   NUMBER OF BLOWS, 3 FIRST TRIES VS 3 LAST ONES   ##

#####----------------------------------------------#####
#####               Import the data                #####
#####----------------------------------------------#####
data <- read.xlsx("2020_09_07_Try_Blow_EFI_wilcoxon.xlsx", sheetName="3_3")
data

#####----------------------------------------------#####
#####                 Divide by series             #####
#####----------------------------------------------#####

## Select 1st series
Try1 <- data[which(data$Try=="T1"),]
Try1

## Select 2nd series
Try2 <- data[which(data$Try=="T2"),]
Try2

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution:
# If p-value<0.05 then the distribution is not normal
shapiro.test(Try1$Blows) 
x11(); hist(Try1$Blows)
shapiro.test(Try2$Blows)
x11(); hist(Try2$Blows)

x11(); qqPlot(Try1$Blows)
x11(); qqPlot(Try2$Blows)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1$Blows, Try2$Blows, alternative = "two.sided")

#####----------------------------------------------#####
##### Assumptions for t-test are not respected so: #####
#####----------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1$Blows, Try2$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try from the same individual


########################################################
#####----------------------------------------------#####
#####         Divide by series and bone            #####
#####----------------------------------------------#####

## Select bones of 1st series
Try1Femur <- data[which(data$Try=="T1" & data$Bone=="Femur"),]
Try1Humerus <- data[which(data$Try=="T1" & data$Bone=="Humerus"),]
Try1RadioUlna <- data[which(data$Try=="T1" & data$Bone=="Radio-Ulna"),]
Try1Tibia <- data[which(data$Try=="T1" & data$Bone=="Tibia"),]

## Select bones of 2nd series
Try2Femur <- data[which(data$Try=="T2" & data$Bone=="Femur"),]
Try2Humerus <- data[which(data$Try=="T2" & data$Bone=="Humerus"),]
Try2RadioUlna <- data[which(data$Try=="T2" & data$Bone=="Radio-Ulna"),]
Try2Tibia <- data[which(data$Try=="T2" & data$Bone=="Tibia"),]

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution for each bone for each try:
# If significant p-value then the distribution is not normal
shapiro.test(Try1Femur$Blows) 
x11(); hist(Try1Femur$Blows)
shapiro.test(Try1Humerus$Blows) 
x11(); hist(Try1Humerus$Blows)
shapiro.test(Try1RadioUlna$Blows) 
x11(); hist(Try1RadioUlna$Blows)
shapiro.test(Try1Tibia$Blows) 
x11(); hist(Try1Tibia$Blows)

shapiro.test(Try2Femur$Blows)
x11(); hist(Try2Femur$Blows)
shapiro.test(Try2Humerus$Blows)
x11(); hist(Try2Humerus$Blows)
shapiro.test(Try2RadioUlna$Blows)
x11(); hist(Try2RadioUlna$Blows)
shapiro.test(Try2Tibia$Blows)
x11(); hist(Try2Tibia$Blows)

x11(); qqPlot(Try1Femur$Blows)
x11(); qqPlot(Try1Humerus$Blows)
x11(); qqPlot(Try1RadioUlna$Blows)
x11(); qqPlot(Try1Tibia$Blows)

x11(); qqPlot(Try2Femur$Blows)
x11(); qqPlot(Try2Humerus$Blows)
x11(); qqPlot(Try2RadioUlna$Blows)
x11(); qqPlot(Try2Tibia$Blows)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1Femur$Blows, Try2Femur$Blows, alternative = "two.sided")
var.test(Try1Humerus$Blows, Try2Humerus$Blows, alternative = "two.sided")
var.test(Try1RadioUlna$Blows, Try2RadioUlna$Blows, alternative = "two.sided")
var.test(Try1Tibia$Blows, Try2Tibia$Blows, alternative = "two.sided")

#####-----------------------------------------------------#####
##### Assumptions for t-test are not always respected so: #####
#####-----------------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1Femur$Blows, Try2Femur$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Humerus$Blows, Try2Humerus$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1RadioUlna$Blows, Try2RadioUlna$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Tibia$Blows, Try2Tibia$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual



#######################################################
##   NUMBER OF BLOWS, 5 FIRST TRIES VS 5 LAST ONES   ##

#####----------------------------------------------#####
#####               Import the data                #####
#####----------------------------------------------#####
data <- read.xlsx("2020_09_07_Try_Blow_EFI_wilcoxon.xlsx", sheetName="5_5")
data

#####----------------------------------------------#####
#####                 Divide by series             #####
#####----------------------------------------------#####

## Select 1st series
Try1 <- data[which(data$Try=="T1"),]
Try1

## Select 2nd series
Try2 <- data[which(data$Try=="T2"),]
Try2

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution:
# If p-value<0.05 then the distribution is not normal
shapiro.test(Try1$Blows) 
x11(); hist(Try1$Blows)
shapiro.test(Try2$Blows)
x11(); hist(Try2$Blows)

x11(); qqPlot(Try1$Blows)
x11(); qqPlot(Try2$Blows)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1$Blows, Try2$Blows, alternative = "two.sided")

#####----------------------------------------------#####
##### Assumptions for t-test are not respected so: #####
#####----------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1$Blows, Try2$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try from the same individual


########################################################
#####----------------------------------------------#####
#####         Divide by series and bone            #####
#####----------------------------------------------#####

## Select bones of 1st series
Try1Femur <- data[which(data$Try=="T1" & data$Bone=="Femur"),]
Try1Humerus <- data[which(data$Try=="T1" & data$Bone=="Humerus"),]
Try1RadioUlna <- data[which(data$Try=="T1" & data$Bone=="Radio-Ulna"),]
Try1Tibia <- data[which(data$Try=="T1" & data$Bone=="Tibia"),]

## Select bones of 2nd series
Try2Femur <- data[which(data$Try=="T2" & data$Bone=="Femur"),]
Try2Humerus <- data[which(data$Try=="T2" & data$Bone=="Humerus"),]
Try2RadioUlna <- data[which(data$Try=="T2" & data$Bone=="Radio-Ulna"),]
Try2Tibia <- data[which(data$Try=="T2" & data$Bone=="Tibia"),]

#####----------------------------------------------#####
#####          Test assumptions for t-test         #####
#####----------------------------------------------#####
# test the assumption of normality of distribution for each bone for each try:
# If significant p-value then the distribution is not normal
shapiro.test(Try1Femur$Blows) 
x11(); hist(Try1Femur$Blows)
shapiro.test(Try1Humerus$Blows) 
x11(); hist(Try1Humerus$Blows)
shapiro.test(Try1RadioUlna$Blows) 
x11(); hist(Try1RadioUlna$Blows)
shapiro.test(Try1Tibia$Blows) 
x11(); hist(Try1Tibia$Blows)

shapiro.test(Try2Femur$Blows)
x11(); hist(Try2Femur$Blows)
shapiro.test(Try2Humerus$Blows)
x11(); hist(Try2Humerus$Blows)
shapiro.test(Try2RadioUlna$Blows)
x11(); hist(Try2RadioUlna$Blows)
shapiro.test(Try2Tibia$Blows)
x11(); hist(Try2Tibia$Blows)

x11(); qqPlot(Try1Femur$Blows)
x11(); qqPlot(Try1Humerus$Blows)
x11(); qqPlot(Try1RadioUlna$Blows)
x11(); qqPlot(Try1Tibia$Blows)

x11(); qqPlot(Try2Femur$Blows)
x11(); qqPlot(Try2Humerus$Blows)
x11(); qqPlot(Try2RadioUlna$Blows)
x11(); qqPlot(Try2Tibia$Blows)

# test the assumption of equality of variance:
# if p-value>0.05 => there is no significant difference between the two variances.
var.test(Try1Femur$Blows, Try2Femur$Blows, alternative = "two.sided")
var.test(Try1Humerus$Blows, Try2Humerus$Blows, alternative = "two.sided")
var.test(Try1RadioUlna$Blows, Try2RadioUlna$Blows, alternative = "two.sided")
var.test(Try1Tibia$Blows, Try2Tibia$Blows, alternative = "two.sided")

#####-----------------------------------------------------#####
##### Assumptions for t-test are not always respected so: #####
#####-----------------------------------------------------#####
# independent 2-group non parametric Mann-Whitney U Test
wilcox.test(Try1Femur$Blows, Try2Femur$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Humerus$Blows, Try2Humerus$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1RadioUlna$Blows, Try2RadioUlna$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual
wilcox.test(Try1Tibia$Blows, Try2Tibia$Blows, alternative = "two.sided", paired = TRUE) # paired = TRUE because we are testing paired data, first try and second try be the same individual



#########################################################################################
# Electronic Supplementary Material 4.

# This code provides the results of the PCA presented in the figure 3 of the paper
#########################################################################################

# Make sure to have the "Supplementary_data_15.xlsx" (Supporting Information 15)
# in the same folder as "A_way_to_break_bone.r".

# Packages needed:
# FactoMineR: Lê, S., Josse, J., & Husson, F. (2008). FactoMineR: an R package for multivariate analysis. Journal of statistical software, 25(1), 1-18.
# Factoshiny, Vaissie, P., Monge, A., & Husson, F. (2015). Factoshiny: perform factorial analysis from FactoMineR with a shiny application. R package version, 1.

# Construction of the script:
#     - step 1: import data 
#     - step 2: compute PCA
#     - step 3: Plot PCA

###############################################
# Run the necessary libraries
library(xlsx)
library(FactoMineR)
###############################################

###############################################
##           step 1 : import data            ##
###############################################
data <- read.xlsx("Supplementary_data_15.xlsx", sheetIndex=1)
str(data)# observe the structure and verify that the import is correct

#Keep the variable of interest
data.acp <- data[,c(3, 4, 5, 9, 13)]
str(data.acp)

###############################################
##            step 2: Compute PCA            ##
###############################################
res.PCA<-PCA(data.acp, quali.sup=c(2), graph=F)

###############################################
##             step 3: Plot PCA              ##
###############################################
# Graph of variables
x11(); plot(res.PCA, choix="var")

# Graph of individuals
x11(); plotellipses(res.PCA, label="quali", level = 0.95, means = TRUE)



#########################################################################################
# Electronic Supplementary Material 5.

# This code provides the results of the boxplots presented in the figure 2 of the paper
#########################################################################################

# Make sure to have the "2019_07_23_EFI pour box plot.xlsx" (Supporting Information 18)
# in the same folder as "A_way_to_break_bone.r".

# Packages needed:
# ggplot2: Wickham, H. (2011). ggplot2. Wiley Interdisciplinary Reviews: Computational Statistics, 3(2), 180-185.

# Construction of the script:
#     - step 1: import data 
#     - step 2: build boxplots for all the series and then for the first 7 and the first 3 series
 
########################################################
# Run the necessary libraries and function
library (xlsx)
library (ggplot2)

###############################################
###### step 1: import data #########
###############################################
EFI <- read.xlsx("2019_07_23_EFI pour box plot.xlsx", sheetName = "EFIall") # import the sheet "EFIall"
str(EFI)# observe the structure and verify that the import is correct

EFI7 <- read.xlsx("2019_07_23_EFI pour box plot.xlsx", sheetName = "EFI7") # import the sheet "EFI7"
str(EFI7)# observe the structure and verify that the import is correct

EFI3 <- read.xlsx("2019_07_23_EFI pour box plot.xlsx", sheetName = "EFI3") # import the sheet "EFI3"
str(EFI3)# observe the structure and verify that the import is correct

###############################################
###### step 2: boxplot #########
###############################################
# For all series number 
names <- c(EFI$Individual)
value <- c(EFI$EFIall)
data <- data.frame(names, value, EFI$Element)
# Open a new window
x11()
ggplot( data, aes(x=as.factor(names) , y=value , fill=EFI$Element))+
  geom_boxplot()+
  labs(x="NIndividual", y = "EFIall")+
  scale_fill_discrete(name="Element")+
  theme_minimal()+
  scale_x_discrete(limits=c("1", "7", "11","2","9","12","3","5","8","4","6","10"))

# For the last seven series number 
names <- c(EFI7$Individual)
value <- c(EFI7$EFI7)
data <- data.frame(names,value,EFI7$Element )
# Open a new window
x11()
ggplot( data, aes(x=as.factor(names) , y=value , fill=EFI7$Element))+
  geom_boxplot()+
  labs(x="NIndividual", y = "EFI7")+
  scale_fill_discrete(name="Element")+
  theme_minimal()+
  scale_x_discrete(limits=c("1", "7", "11","2","9","12","3","5","8","4","6","10"))

# For the three first series number 
names <- c(EFI3$Individual)
value <- c(EFI3$EFI3)
data <- data.frame(names,value,EFI3$Element )
# Open a new window
x11()
ggplot( data, aes(x=as.factor(names) , y=value , fill=EFI3$Element))+
  geom_boxplot()+labs(x="NIndividual", y = "EFI3")+
  scale_fill_discrete(name="Element")+
  theme_minimal()+
  scale_x_discrete(limits=c("1", "7", "11","2","9","12","3","5","8","4","6","10"))

