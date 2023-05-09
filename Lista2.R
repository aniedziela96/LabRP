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
  return(TRUE)
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

hands_ranking <- c('High card', 'Pair', 'Two Pair', 
           'Three of a kind', 'Straight', 'Flush', 
           'Full House', 'Four of a kind', 
           'Straight Flush', 'Royal Flush')

poker <- function(v){
  hand <- sort_card(v)
  # checks if there is a pair - if there is there's no possibility for straight, colour etc.
  if(hand[,2][1] == hand[,2][2] |
     hand[,2][2] == hand[,2][3] |
     hand[,2][3] == hand[,2][4] |
     hand[,2][4] == hand[,2][5]){
    # checks if there are two pairs
    if((hand[,2][1] == hand[,2][2] & 
        hand[,2][3] == hand[,2][4]) |
       (hand[,2][2] == hand[,2][3] &
        hand[,2][4] == hand[,2][5])){
      # checks if there is a three
      if(hand[,2][1] == hand[,2][3] | hand[,2][3] == hand[,2][5]){
        # two possible options: full or four
        if(hand[,2][1] == hand[,2][4] |
           hand[,2][2] == hand[,2][5]){
          # checks for four, if it is not, then we have a full
          return(3) # Four
        } else {
          return(4) # Full
        }
      }
      return(8) # Two Pair, there's no Three, so it only can be Two Pair
    } else if(hand[,2][1] == hand[,2][3] | hand[,2][3] == hand[,2][5]){
      # Now check for Three, but now without the Two Pair
      return(7) # Three of a kind
    }
    # if there is no Three or Two Pairs, there only can be a One Pair
    return(9)
    
  } else if(is_one_colour(hand)){ #check for straight, flush
    if(is_straight(hand)){
      if(hand[,2][1] == 10){
        return(1) # Royal flush
      } else {
        return(2) # Straight flush
      }
    } else {
      return(5) # flush
    }
  } else if(is_straight(hand)){
    return(6) # straight 
    } else {
      return(10)
  }
}

u <- numeric(1000000)

for(i in 1:1000000){
  v <- cards[sample(1:52, 5),]
  u[i] <- poker(v)
}

probs <- numeric(0)

for(i in 1:10){
  probs[i] <- length(which(u == i)) / 10000
}

probs


