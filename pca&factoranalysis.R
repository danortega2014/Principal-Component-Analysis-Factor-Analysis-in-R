# Principal Component Analysis and Factor Analysis in R
library(readxl)
genedata <- read_excel("C:/Users/danor/Desktop/genedata pca.xltx")
View(genedata)
attach

X <- cbind(genedata[,4:86])

# descriptive stats
summary(X)
cor(X)
#Imputing averages for all continuous variables
for(i in 1:ncol(X)){
  X[is.na(X[,i]), i] <- mean(X[,i], na.rm = TRUE)
}
#check if all NAS are gone
summary(X)
# Principal component analysis
pca1 <- princomp(X, scores=TRUE, cor=TRUE)
summary(pca1)

# loadings of principal componenets
loadings(pca1)


# scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")


# Scores of the components
pca1$scores[1:10,]

# Rotation
varimax(pca1$loadings[,1:3])
promax(pca1$loadings[,1:3])


# Factor analysis -------------------
fa1 <- factanal(X, factor=3, rotation="none")
fa1

fa2 <- factanal(X, factor=3, rotation="varimax")
fa2

fa3 <- factanal(X, factors=3, rotation="varimax", scores="regression")
fa3
fa3$scores

fa4 <- factanal(X, factor=3, rotation="promax")
fa4


