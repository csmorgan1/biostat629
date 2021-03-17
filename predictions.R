require(caret)
require(e1071)
require(kernlab)
require(tidyverse)
require(forcats)
require(table1)
require(pROC)

setwd("//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/csmorgan")
set.seed(123)

dat <- read_csv("//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/csmorgan/dat.csv")

dat <- dat %>% 
  select(-X1) %>% 
  filter(HRV_mean<205) %>%
  mutate(Anxiety=factor(Anxiety,
                        labels = c("No","Yes")),
         Age = scale(AgeAtEnrollment,center=T,scale=T),
         Gender = factor(GenderName),
         Race = fct_collapse(RaceName,
                      White = c("Caucasian"),
                      Black = c("African American"),
                      Asian = c("Asian"),
                      Other = c("Other","American Indian or Alaska Native","Native Hawaiian and Other Pacific Islander","Patient Refused","Unknown")),
         Race=factor(Race),
         HRV_scale = scale(HRV_mean, center=T,scale=T)) %>%
  select(-GenderName,-RaceName,-HRV_mean,-AgeAtEnrollment) %>%
  na.omit()

table1(~AgeAtEnrollment+Gender+Race+HRV_mean|Anxiety,data=dat,render.missing=function(x,...)character(0),caption="Demographics Stratified by Anxiety")

inTrain <- createDataPartition(y=dat$Anxiety,p=0.8,list = F)

training <- dat[inTrain,]
testing <- dat[-inTrain,]
True_Anxiety <- testing$Anxiety

glm.fits <- glm(Anxiety~.,
                data = training %>% select(-1),
                family = binomial)

forestmodel::forest_model(glm.fits, format_options = forestmodel::forest_model_format_options(color="FFCB05",shape=19))

glm.probs <- predict.glm(glm.fits,
                     type="response")

training %>%
  add_column(Prob=glm.probs) %>%
  ggplot(aes(x=Prob,y=Anxiety,color=Anxiety)) +
  geom_point()

get_logistic_pred <- function(mod, data, res="y",pos=1,neg=0,cut=0.5){
  probs <- predict(mod,newdata=data,type="response")
  ifelse(probs>cut,pos,neg)
}

test.pred.05 <- get_logistic_pred(glm.fits,data=testing,res="default",pos="Yes",neg="No",cut=0.05)
mean(test.pred.05==True_Anxiety)
test.pred.10 <- get_logistic_pred(glm.fits,data=testing,res="default",pos="Yes",neg="No",cut=0.1)
mean(test.pred.10==True_Anxiety)
test.pred.15 <- get_logistic_pred(glm.fits,data=testing,res="default",pos="Yes",neg="No",cut=0.15)
mean(test.pred.15==True_Anxiety)

test.tab.05 <- table(predicted=test.pred.05,actual=True_Anxiety)
test.tab.10 <- table(predicted=test.pred.10,actual=True_Anxiety)
test.tab.15 <- table(predicted=test.pred.15,actual=True_Anxiety)

test.conmat.05 <- caret::confusionMatrix(test.tab.05,positive="Yes")
test.conmat.10 <- caret::confusionMatrix(test.tab.10,positive="Yes")
test.conmat.15 <- caret::confusionMatrix(test.tab.15,positive="Yes")

metrics <- rbind(
  c(test.conmat.05$overall["Accuracy"],
    test.conmat.05$byClass["Sensitivity"],
    test.conmat.05$byClass["Specificity"]),
  c(test.conmat.10$overall["Accuracy"],
    test.conmat.10$byClass["Sensitivity"],
    test.conmat.10$byClass["Specificity"]),
  c(test.conmat.15$overall["Accuracy"],
    test.conmat.15$byClass["Sensitivity"],
    test.conmat.15$byClass["Specificity"])
)
rownames(metrics) <- c("c = 0.05","c = 0.10","c = 0.15")

kableExtra::kable(as.data.frame(metrics)) %>%
  kableExtra::kable_classic()

test.prob <- predict(glm.fits,newdata = testing,type="response")

test.roc <- roc(True_Anxiety~test.prob,plot=T,print.auc=T)

ggroc(test.roc) +
  geom_segment(aes(x=1,xend=0,y=0,yend=1)) +
  theme_bw() + 
  geom_label(aes(x=0.5,y=0.5,label="AUC 0.772")) +
  geom_segment(aes(x=0.05,xend=0.05,y=0,yend=0.8201384,color="5DE2E0")) +
  geom_segment(aes(x=0.1,xend=0.1,y=0,yend=0.9461952,color="6D5DE2")) +
  geom_segment(aes(x=0.15,xend=0.15,y=0,yend=0.9677171, color = "E25DD6")) +
  geom_segment(aes(x=1,xend=0.05,y=0.8104375,yend=0.8201384,color="5DE2E0")) +
  geom_segment(aes(x=1,xend=0.1,y=0.9316961,yend=0.9461952,color="6D5DE2")) +
  geom_segment(aes(x=1,xend=0.15,y=0.961627,yend=0.9677171, color = "E25DD6")) +
  theme(legend.position = "none")
 
