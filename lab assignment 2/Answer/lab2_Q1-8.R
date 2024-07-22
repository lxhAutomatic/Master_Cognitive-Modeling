library(ggplot2)
library(reshape2)
# data preprocessing
original_data <- read.table('C:/Users/75581/Desktop/CM/Lab assignment 2/NeuralResponses')
subjects <- c()
for(i in 1:12){
  ns <- rnorm(nrow(original_data)*ncol(original_data), mean = 0, sd = 1)
  noise <- matrix(ns,ncol=100,byrow=TRUE)
  subject <- original_data+noise
  subjects[[i]] <- subject
}

#Question 2
correlations <- c()
melted <- c()
for (i in 1:12){
  correlations[[i]] <- 1-cor(t(subjects[[i]]))
  melted[[i]] <- melt(correlations[[i]])
}
correlations_original <- 1-cor(t(original_data))
melted_original <- melt(correlations_original)
p <- ggplot(data = melted_original, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
p +labs(title="RDM Original Data", x="",y="")
p <- ggplot(data = melted[[4]], aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
p +labs(title="RDM Subject 4 Data", x="",y="")

#Question 3
average_subject <- Reduce("+", correlations) / length(correlations)
ggplot(data = melt(average_subject), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + labs(title="Average RDM All Subject", x="", y="")

# data input
category_vectors <- read.table('C:/Users/75581/Desktop/CM/Lab assignment 2/CategoryVectors')
category_labels <- read.table('C:/Users/75581/Desktop/CM/Lab assignment 2/CategoryLabels')

#Question 4
melt_matrix <- acast(melted_original, Var2~Var1, value.var="value")
matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 1] == category_vectors[j, 1]){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}
preprocess_matrix = function(matrix)
{
  matrix[lower.tri(matrix, diag = TRUE)] <- NA
  matrix <- c(matrix)
  matrix <- matrix[!is.na(matrix)]
  matrix <- matrix[ matrix != 0]
}
diss_same <- preprocess_matrix(matrix*melt_matrix)
matrix <- +(!matrix)

diss_different <- preprocess_matrix(matrix*melt_matrix)
t.test(diss_same,diss_different)

# t-test for original 
# data:  diss_same and diss_different
# t = -230.35, df = 2740.1, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.3043685 -0.2992304
# sample estimates:
#  mean of x mean of y 
# 0.8504482 1.1522476 

#Question 5
#Average subject
matrix <- +(!matrix)
diss_same <- preprocess_matrix(matrix*average_subject)
matrix <- +(!matrix)
diss_different <- preprocess_matrix(matrix*average_subject)
t.test(diss_same,diss_different)

# t-test for average
# data:  diss_same and diss_different
# t = -145.57, df = 3703.9, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.1511312 -0.1471144
# sample estimates:
#  mean of x mean of y 
# 0.9258305 1.0749533

#Example subject 4
melt_matrix_2 <- acast(melted[[4]], Var2~Var1, value.var="value")
matrix <- +(!matrix)
diss_same <- preprocess_matrix(matrix*melt_matrix_2)
matrix <- +(!matrix)
diss_different <- preprocess_matrix(matrix*melt_matrix_2)

t.test(diss_same,diss_different)

# t-test for subject 4
# data:  diss_same and diss_different
# t = -52.892, df = 4179.8, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.1495936 -0.1389000
# sample estimates:
#  mean of x mean of y 
# 0.9304236 1.0746704 

#Question 6
matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if(category_vectors[i, 6] == category_vectors[j, 6]){
      matrix[i, j] = 1
    }
    else{
      matrix[i, j] = 0
    }
  }
}
diss_same <- preprocess_matrix(matrix*melt_matrix)
matrix <- +(!matrix)
diss_different <- preprocess_matrix(matrix*melt_matrix)

t.test(diss_same,diss_different)


# data:  diss_same and diss_different
# t = -18.693, df = 3819, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.09572642 -0.07755278
# sample estimates:
#  mean of x mean of y 
# 0.9689395 1.0555791 

#Question 7
matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 6] == category_vectors[j, 6] & category_vectors[i, 1] == 1 & category_vectors[j, 1] == 1){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}

diss_same_face_an <- preprocess_matrix(matrix*melt_matrix)

matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 6] != category_vectors[j, 6] & category_vectors[i, 1] == 1 & category_vectors[j, 1] == 1){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}

diss_different_face_an <- preprocess_matrix(matrix*melt_matrix)

t.test(diss_same_face_an,diss_different_face_an)

# data:  diss_same_face_an and diss_different_face_an
# t = -22.586, df = 592.1, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.08414440 -0.07068147
# sample estimates:
#  mean of x mean of y 
# 0.793767  0.871180 

#Question 8
matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 3] == category_vectors[j, 3]){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}

diss_same_human <- preprocess_matrix(matrix*melt_matrix)
matrix <- +(!matrix)

diss_different_human <- preprocess_matrix(matrix*melt_matrix)
t.test(diss_same_human,diss_different_human)

# human data
# data:  diss_same_human and diss_different_human
# t = -13.12, df = 3298.8, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.07447192 -0.05510768
# sample estimates:
#  mean of x mean of y 
# 0.9774581 1.0422479 

matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 3] == category_vectors[j, 3] & category_vectors[i, 1] == 1 & category_vectors[j, 1] == 1){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}

diss_same_human_an <- preprocess_matrix(matrix*melt_matrix)

matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 3] != category_vectors[j, 3] & category_vectors[i, 1] == 1 & category_vectors[j, 1] == 1){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}

diss_different_human_an <- preprocess_matrix(matrix*melt_matrix)
t.test(diss_same_human_an,diss_different_human_an)

# human and animate data
# data:  diss_same_human_an and diss_different_human_an
# t = -0.45633, df = 1120, p-value = 0.6482
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.009863570  0.006141256
# sample estimates:
#  mean of x mean of y 
# 0.8323467 0.8342078 
