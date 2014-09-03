# STAT428 Project Codes

### Import data and Create the typearr(Response). 
# The response is 0 or 1 where 0 represents healthy person and 1 
# represents a patient with a cancer.
breast <- source("brda.rda")$value
head(breast)
dim(breast) # 3266x22
cols <- colnames(breast)
typearr <- rep(0, dim(breast)[2])
for(i in 1:length(cols)){
    if ( grepl("Sporadic", cols[i]) == 1){
        typearr[i] = 0
    } else {
        typearr[i] = 1
    }
}
typearr
which(typearr == 0) # 11 12 13 14 15 16 17
### Rearrange the columns of the original matrix here
typearr2 <- c(rep(0, 7), rep(1, 15))
breast2 <- matrix(rep(0, ncol(breast)*nrow(breast)), ncol=ncol(breast))
breast2 <- cbind(breast[, 11:17], breast[, 1:6], breast[, 18], 
    breast[, 7:10], breast[, 19:22])
colnames(breast2)[14] <- colnames(breast)[18]
head(breast2)

### a) Find the breast-cancer related genetic markers.
source("http://bioconductor.org/biocLite.R")
biocLite("multtest")
library(multtest)
teststat2 <- mt.teststat(as.matrix(breast2), typearr2)
qqnorm(teststat2)
qqline(teststat2)
# Transform the data from data.frame to matrix format.
breast2 <- as.matrix(breast2)
genenum <- dim(breast2)[1]
parr <- rep(0, genenum)
# Do F-test first to check variances before performing T-tests
for (i in 1:3226){
    tmp <- var.test(breast2[i, 1:7], breast2[i, 8:22])$p.value
    if(tmp < 0.01){
        parr[i] <- t.test(breast2[i, 1:7], breast2[i, 8:22], var.equal=F, paired=F)$p.value
    } else {
        parr[i] <- t.test(breast2[i, 1:7], breast2[i, 8:22], var.equal=T, paired=F)$p.value
    }
}
# Use 0.01 as the Significance level here.
length(parr[parr < 0.01]) # 60 significant genes
selection <- breast2[which(parr<0.01), ]
rownames(selection) # Names of the 60 significant genes
library("gplots")
heatmap.2(selection, col=redgreen(75), scale="row",
           key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=0.5)

### c) Use your selected marker to predict the breast cancer and obtain 
### the prediction error.
set.seed(1235)
# Randomly separate data to get train/test.
index <- as.matrix(rbinom(22, 1, 0.7))
colnames(index) <- "Train"
selected <- t(selection) # 22x60
response <- as.matrix(typearr2)
colnames(response) <- "Response"
selected <- cbind(selected, response, index) # 22x62
train_type <- selected[index==1, "Response"]
train <- selected[index==1, 1:60]
test_type <- selected[index==0, "Response"]
test <- selected[index==0, 1:60]
# Use LDA to train the training data.
my.lda <- lda(train, train_type)
summary(my.lda)
# Predict on the testing data.
pred <- predict(my.lda, test)
sort(abs(my.lda$scaling[, 1]))
sum(pred$class==test_type) 
sum(pred$class!=test_type) / length(test_type)
ldahist(data=pred$x[, 1], g=test_type)

######## CELL CYCLE #########
# NORMAL CELLS: M1-G-M2-D    (NCBI)
# CANCER CELLS: M1-G-M2
