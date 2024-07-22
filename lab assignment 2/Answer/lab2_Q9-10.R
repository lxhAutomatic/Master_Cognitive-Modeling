library(reshape2)
original_data <- read.table('C:/Users/75581/Desktop/CM/Lab assignment 2/NeuralResponses')
category_vectors <- read.table('C:/Users/75581/Desktop/CM/Lab assignment 2/CategoryVectors')
category_labels <- read.table('C:/Users/75581/Desktop/CM/Lab assignment 2/CategoryLabels')
preprocess_matrix_two = function(matrix){
  matrix[lower.tri(matrix, diag=TRUE)] <- NA
  matrix <- c(matrix)
  matrix <- matrix[!is.na(matrix)]
  #matrix <- matrix[ matrix != 0 ]
}

melt <- melt(1 - cor(t(original_data)))
melt_matrix <- acast(melt, Var2~Var1, value.var="value")

matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
list_diff = c()
list_same = c()
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 1] == 1 & category_vectors[j, 1] == 1){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}

diss_same <- preprocess_matrix_two(matrix*melt_matrix)
matrix <- +(!matrix)
diss_different <- preprocess_matrix_two(matrix*melt_matrix)

matrix = matrix(NA, nrow = length((category_vectors[,1])), ncol = length(category_vectors[,1]))
for (i in 1:92){
  for (j in 1:92){
    if (category_vectors[i, 6] == 1 & category_vectors[j, 6] == 1 & category_vectors[i, 1] == 1 & category_vectors[j, 1] == 1){
      matrix[i, j] = 1
    }else
      matrix[i, j] = 0
  }
}

diss_same_face_an <- preprocess_matrix_two(matrix*melt_matrix)
matrix <- +(!matrix)
diss_different_face_an <- preprocess_matrix_two(matrix*melt_matrix)

my_data <- cbind(diss_same_face_an, diss_same)
my_df <- as.data.frame(my_data)
my_df <- my_df[rowSums(my_df[])>0,]

res.aov2 <- aov(diss_same_face_an ~ diss_same+diss_different, data = my_df)
summary(res.aov2)
res.aov2 <- aov(diss_different_face_an ~ diss_same+diss_different, data = my_df)
summary(res.aov2)

#library(sjstats)
#new_melt_matrix <- preprocess_matrix_two(melt_matrix)

#my_data <- cbind(diss_same_face_an,diss_different_face_an, diss_same, diss_different)
#my_df <- as.data.frame(my_data)
#my_df <- my_df[rowSums(my_df[])>0,]

#res.aov2 <- aov(new_melt_matrix ~ diss_same+diss_different, data = my_df)
#summary(res.aov2)
#effectsize::eta_squared(res.aov2)

#res.aov2 <- aov(new_melt_matrix ~ diss_same_face_an+diss_different_face_an, data = my_df)
#summary(res.aov2)
#effectsize::eta_squared(res.aov2)
