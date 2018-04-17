source("Cleaning Functions.R")
source("Data for Regression.R")

external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

# combine suitability & project proposal
external_regression_data$ProposalCombined <- round((external_regression_data$ScientificRelevance+
                                                      external_regression_data$Suitability)/2, 0)


# Simple first regression: Overall Grade 4-6 >- "Good", 1-3 -> bad
external_regression_data$SimplifiedOverallGrade <- ifelse(external_regression_data$OverallGrade < 4, 0, 1)
external_regression_data$SimplifiedProposal <- ifelse(external_regression_data$ProposalCombined < 4, 0, 1)
external_regression_data$SimplifiedApplicant <- ifelse(external_regression_data$ApplicantTrack < 4, 0, 1)


# Janine regression 

janine_reg_proposal <- glm(SimplifiedProposal ~ PercentFemale+Gender+Division+
                             IsContinuation+PreviousRequest+InstType+AmountRequested+
                            Gender:PercentFemale-ProjectID, 
                           data=external_regression_data, family="binomial")

summary(janine_reg_proposal)

janine_reg_applicant <- glm(SimplifiedApplicant ~ PercentFemale+Age+Gender+Division+
                              IsContinuation+PreviousRequest+InstType+AmountRequested+
                              Gender:Division + Gender:PercentFemale-ProjectID, 
                            data=external_regression_data, family="binomial")

summary(janine_reg_applicant)

# Diagnostics

Model<-janine_reg_proposal 

# Are covariates correlated
vif(Model)  

# Model matrix, to spread factors into dummy variables
X<-model.matrix(Model)
X<-X[,-1]   # Eliminate the intercept
Xc<-cor(X)   # Compute correlation matrix
corrplot(Xc)

# Plot the correlation matrix  

# correlation test
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(X,method="spearman")
head(p.mat[, 1:5])

corrplot(Xc, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01,insig = "blank")


# Model Diagnostic


fvl <- predict(Model, type="link")
fpr <- predict(Model, type="response")
SimplifiedProposal<-external_regression_data$SimplifiedProposal

# Residual plot
plot(fvl, SimplifiedProposal, type="n", xlab="linear predictor",
     ylab="Approval Result", xlim=c(-2,6))
points(fvl[SimplifiedProposal==0], SimplifiedProposal[SimplifiedProposal==0])
points(fvl[SimplifiedProposal==1], SimplifiedProposal[SimplifiedProposal==1], col="red")
lines(sort(fvl), sort(fpr), lty=3)
title("Result vs. Linear Predictor")   # Confirms what Leslie found

# there are some outliers
#id<-which(fvl<(-5))

# Ignore outliers to better visualize the residual plot
#It looks fine to me, what do you think
xx <- fvl
yy <- residuals(Model, type="deviance")

plot(xx, yy, pch=20, main="Tukey-Anscombe Plot",xlim=c(-2,6))
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")

# Analysing outliers
obs<-external_regression_data[id,]
obs<-obs$ProjectID

#This are observations for which residuals are greater than 2 and need a closer look
dat<-subset(apps, ProjectID%in%obs)
dat<-merge(dat,external_reviews, by="ProjectID")

# Tables 
structable(~ Gender+Division+SimplifiedProposal, data = dat) # all rejected
structable(~ InstType+PreviousRequest+IsContinuation,data=dat) # all Previous Request
cotabplot(~Gender+GradeFinal, data=dat) # 


#Inference
#from the script "Applied Statistical Regression - AS 2017" from Dr. Marcel Dettling
#coeficient of determination
1-Model$dev/Model$null # ?? This does not look nice
#or
(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))

#optimize model
drop1(Model, test="Chisq")
#we can consider eliminating some variables



# Residuals: see if there are Pearson residuals greater than 2 (need a closer look)
R<-residuals(Model, type = "pearson")
idR<-which(abs(R)>2)
R[idR]
obs<-external_regression_data[idR,]
obs<-obs$ProjectID

#This are observations for which residuals are greater than 2 and need a closer look
dat<-subset(apps, ProjectID%in%obs)
dat<-merge(dat,external_reviews, by="ProjectID")


structable(~ Gender+Division+SimplifiedProposal, data = dat) 
structable(~ InstType+PreviousRequest+IsContinuation,data=dat) 
cotabplot(~Gender+GradeFinal, data=dat) # All B adn B+

# I do not see anithing special      

