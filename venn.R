# Load library
library(VennDiagram)

# Generate 3 sets of 200 words
set1 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set2 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set3 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set4 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")


library(venn)
n12<- 5
n13 <- 5
n14 <- 3
n23 <- 2
n24 <- 1
n34 <- 2
set_list <- list(top15_importance_df[,1],top15_importance_df[,3],top15_importance_df[,5],
                 top15_importance_df[,7])
venn(set_list, ilab=TRUE, zcolor = "style")
