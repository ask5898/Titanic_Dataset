# Title     : TODO
# Objective : TODO
# Created by: ali
# Created on: 31/8/18

df_train <- read.csv(file="train.csv",header=TRUE,sep=",")

vec_sex <- vector()

for(sex in df_train$Sex) {
    ifelse(sex == "male",vec_sex <- append(vec_sex,0),vec_sex <- append(vec_sex,1))
}


df_train$Sex <- vec_sex
drop <- c("Cabin","Name","Ticket")
df_train_final <- df_train[,!(names(df_train) %in% drop)]

mean_age <- colMeans(df_train_final["Age"],na.rm=TRUE)
vec_age <- which(is.na(df_train_final["Age"]))

df_train_final$Age[vec_age] <- round(mean_age)
vec_temp <- vector()
input_matrix <- matrix(nrow=11,ncol=891,byrow=FALSE)

for(row in 1:nrow(df_train_final)) {

    vec_temp <- as.numeric(round(df_train_final[row,c("PassengerId","Survived","Pclass","Sex","Age","SibSp","Parch","Fare")]))
    if(df_train_final[row,c("Embarked")] == "C") {
        vec_temp <- append(vec_temp,1)
        vec_temp <- append(vec_temp,0)
        vec_temp <- append(vec_temp,0)
    }
    else if(df_train_final[row,c("Embarked")] == "Q")     {
        vec_temp <- append(vec_temp,0)
        vec_temp <- append(vec_temp,1)
        vec_temp <- append(vec_temp,0)
    }
    else if(df_train_final[row,c("Embarked")] == "S")     { 
         vec_temp <- append(vec_temp,0)
         vec_temp <- append(vec_temp,0)
         vec_temp <- append(vec_temp,1)
    }
    else {
           vec_temp <- append(vec_temp,0)
           vec_temp <- append(vec_temp,0)
           vec_temp <- append(vec_temp,0)
    }
    #print(vec_temp)
    input_matrix <- cbind(vec_temp,input_matrix)
}
input_matrix <- input_matrix[, colSums(is.na(input_matrix)) != nrow(input_matrix)]
print(input_matrix)
