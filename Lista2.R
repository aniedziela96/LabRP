colors <- rep(1:4, 13)
figures <- rep(2:14, 4)
cards <- data.frame(colors, figures)


sort_card <- function(v){
  sorted_v <- v[order(v$figures, v$colors),]
  return (sorted_v)
}

is_one_colour <- function(hand){
  if(hand[,1][1] == hand[,1][2] & 
     hand[,1][2] == hand[,1][3] & 
     hand[,1][3] == hand[,1][4] & 
     hand[,1][4] == hand[,1][5]){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_Royal_flush <- function(v){
  hand <- sort_card(v)
  if(is_one_colour(hand) & hand[,2][1] == 10){
    return(TRUE)
  } else {
      return(FALSE)
    }
}

is_flush <- function(v){
  hand <- sort_card(v)
  if(is_one_colour(hand) == FALSE){
    return(FALSE)
  } else if(is_Royal_flush(hand)){
    return(FALSE)
  } else {
    for (i in 1:4){
      if(hand[,2][i] + 1 != hand[,2][i+1]){
        return(FALSE)
      }
    }
    return(TRUE)
  }
}

is_colour <- function(v){
  if(is_one_colour(v)){
    if(is_flush(v) == FALSE){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

is_straight <- function(v){
  hand <- sort_card(v)
  for (i in 1:4){
    if(hand[,2][i] + 1 != hand[,2][i+1]){
      return(FALSE)
    }
  }
  if(is_one_colour(v)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

is_four <- function(v){
  hand <- sort_card(v)
  if(hand[,2][1] == hand[,2][4] |
     hand[,2][2] == hand[,2][5]){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_full <- function(v){
  hand <- sort_card(v)
  if((hand[,2][1] == hand[,2][3] & hand[,2][4] == hand[,2][5]) |
     (hand[,2][1] == hand[,2][2] & hand[,2][3] == hand[,2][5])){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_three <- function(v){
  hand <- sort_card(v)
  if(hand[,2][1] == hand[,2][3] | hand[,2][3] == hand[,2][5]){
    if(is_full(v) | is_four(v)){
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  } 
}

is_two_pairs <- function(v){
  hand <- sort_card(v)
  if(is_three(v) | is_full(v) | is_four(v)){
    return(FALSE)
  } else if((hand[,2][1] == hand[,2][2] &
             hand[,2][3] == hand[,2][4]) |
            (hand[,2][2] == hand[,2][3] &
             hand[,2][4] == hand[,2][5])){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_pair <- function(v){
  hand <- sort_card(v)
  if(is_three(v) | is_full(v) | is_four(v) | is_two_pairs(v)){
    return(FALSE)
  } else if(hand[,2][1] == hand[,2][2] |
            hand[,2][2] == hand[,2][3] |
            hand[,2][3] == hand[,2][4] |
            hand[,2][4] == hand[,2][5]){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_high_card <- function(v){
  if(is_three(v) | 
     is_full(v) | 
     is_four(v) | 
     is_two_pairs(v) |
     is_pair(v) |
     is_one_colour(v) |
     is_straight(v) |
     is_flush(v) |
     is_Royal_flush(v)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

a<-0

for(i in 1:10000){
  v <- cards[sample(1:52, 5),]
  if(is_high_card(v)){
    a <- a + 1
  }
}

a


