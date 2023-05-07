#ZADANIE 1

population_size <- 1000000
probability_sick <- 0.1
prob_sick_pos <- 0.92
prob_healthy_neg <- 0.94
population <- rbinom(population_size, 1, probability_sick)


experiment_size <- 1000000

pos_test <- 0

for (i in 1:experiment_size){
  
  person <- sample(c(0,1), prob = c(0.9, 0.1), 1)
  if(person == 0){
      pos_test = pos_test + sample(c(0,1), 
                                   prob = c(prob_healthy_neg, 1 - prob_healthy_neg), 
                                   1)
  }
  else{
    pos_test = pos_test + sample(c(0,1), 
                                 prob = c(1 - prob_sick_pos, prob_healthy_neg), 
                                 1)
  }
}

pos_test / experiment_size

tests <- rep(0, population_size)
for (i in 1:population_size){
  person <- population[i]
  if (person == 0){
    tests[i] <- sample(c(0,1), prob = c(prob_healthy_neg, 1 - prob_healthy_neg), 
                       1)
  }
  if (person == 1){
    tests[i] <- sample(c(0,1), 
                       prob = c(1 - prob_sick_pos, prob_healthy_neg), 
                       1)
  }
}

a <- 0

for (i in 1:population_size){
  if(tests[i] == 1 & population[i] == 1){
    a <- a + 1
  }
}

a / sum(tests)

# ZADANIE 2

experiment_2_size <- 1000000

children1 <- rbinom(experiment_2_size, 1, 0.5)
children2 <- rbinom(experiment_2_size, 1, 0.5)

# 1

a <- 0
b_1 <- 0

for (i in 1:experiment_2_size){
  if (children1[i] == 1 | children2[i] == 1){
    a <- a + 1
  }
  if (children1[i] == 1 & children2[i] == 1){
    b_1 <- b_1 + 1
  }
}

b_1 / a

# 2

b_2 <- 0

for (i in 1:experiment_2_size){
  if (children1[i] == 1 & children2[i] == 1){
    b_2 <- b_2 + 1
  }
}

b_2 / sum(children1)

# 3

days_of_birth_1 <- sample(1:7, experiment_2_size, replace = TRUE)
days_of_birth_2 <- sample(1:7, experiment_2_size, replace = TRUE)

a_3 <- 0
b_3 <- 0

for (i in 1:experiment_2_size){
  if ((children1[i] == 1 & days_of_birth_1[i] == 2) | (children2[i] == 1 & days_of_birth_2[i] == 2)){
    a_3 <- a_3 + 1
    if(children1[i] == 1 & children2[i] == 1){
      b_3 <- b_3 + 1
    }
  }
}

b_3 / a_3

# 4 

a_4 <- 0
b_4 <- 0 

for (i in 1:experiment_2_size){
  if ((children1[i] == 1 & days_of_birth_1[i] == 2) | 
      (children1[i] == 1 & days_of_birth_1[i] == 2) |
      (children2[i] == 1 & days_of_birth_2[i] == 5) |
      (children2[i] == 1 & days_of_birth_2[i] == 5)){
    a_4 <- a_4 + 1
    if(children1[i] == 1 & children2[i] == 1){
      b_4 <- b_4 + 1
    }
  }
}

b_4 / a_4


