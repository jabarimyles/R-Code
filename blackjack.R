library(dplyr)

VI_policy <- 'ES'
epsilon <- 0.01

set.seed(123)

N <- 50^6
States <- length(seq(2, 11)) * length(seq(12, 21)) * 2
Actions <- 2
dealer_card <- length(seq(1, 10))
my_cards <- length(seq(2, 22))
usable_ace <- 2
actions <- 2

G_avg <- vector()
policy <- rep(0, States)
Policy <- list()

for (i in seq(1, dealer_card)) {
  for (j in seq(2, my_cards + 1)) {
    for (k in seq(usable_ace)) {
      Policy[[paste(i, j, k, sep = "_")]] <- 0
    }
  }
}

NewPolicy <- list()

for (i in seq(1, dealer_card)) {
  for (j in seq(2, my_cards + 1)) {
    for (k in seq(usable_ace)) {
      NewPolicy[[paste(i, j, k, sep = "_")]] <- 0
    }
  }
}

support <- c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)
support_A <- c(1, 0)
probabilities <- c(16/52, 4/52, 4/52, 4/52, 4/52, 4/52, 4/52, 4/52, 4/52, 4/52)
iterations <- 20

for (iter in seq(iterations)) {
  if (VI_policy == 'ES') {
    Policy <- NewPolicy
  }}
  
  U <- list()
  for (i in seq(1, dealer_card)) {
    for (j in seq(2, my_cards + 1)) {
      for (k in seq(usable_ace)) {
        for (l in seq(2)) {
          U[[paste(i, j, k, l, sep = "_")]] <- 0
        }
      }
    }
  }
  
  Q <- list()
  for (i in seq(1, dealer_card)) {
    for (j in seq(2, my_cards + 1)) {
      for (k in seq(usable_ace)) {
        for (l in seq(2)) {
          Q[[paste(i, j, k, l, sep = "_")]] <- 0
        }
      }
    }
  }
  
  G_list <- vector()
  
  for (_ in seq(N)) {
    S_list <- list()
    R_list <- vector()
    A_list <- vector()
    
    for (i in seq(1)) {
      c_values <- sample(support, 4, prob = probabilities)
      P_1 <- c_values[1]
      P_2 <- c_values[2]
      D_1 <- c_values[3]
      D_2 <- c_values[4]
      A_0 <- sample(support_A, 1)
      
      if ((P_1 == 1 || P_2 == 1) && (P_1 + P_2 + 10 <= 21)) {
        S_p <- P_1 + P_2 + 10
        A_p <- 1
      } else {
        S_p <- P_1 + P_2
        A_p <- 0
      }
      
      if ((D_1 == 1 || D_2 == 1) && (D_1 + D_2 + 10 <= 21)) {
        S_d <- D_1 + D_2 + 10
        A_d <- 1
      } else {
        S_d <- D_1 + D_2
        A_d <- 0
      }
      
      if (S_p < 12) {
        A_0 <- 1
      }
      
      S_list[[1]] <- c(D_1, S_p, A_p)
      if (S_p == 21) {
        A_list <- c(A_list, 0)
      }
      
      while (S_p < 21) {
        if (length(A_list) == 0) {
          action <- A_0
        } else {
          action <- Policy[[paste(S_list[[length(S_list)]], collapse = "_")]]
        }
        
        if (action == 1) {
          C <- sample(support, 1, prob = probabilities)
          
          if (A_p == 1 && S_p > 21) {
            S_p <- S_p - 10
            A_p <- 0
          }
          
          if (A_p == 0 && C == 1 && S_p + 10 <= 21) {
            S_p <- S_p + 10
            A_p <- 1
          }
          
          S_p <- S_p + C
          
          if (S_p < 22) {
            S_list[[length(S_list) + 1]] <- c(D_1, S_p, A_p)
          }
        } else {
          A_list <- c(A_list, action)
          break
        }
        
        A_list <- c(A_list, action)
      }
      
      while ((S_d < 21 || S_d < S_p) && S_p < 21) {
        C <- sample(support, 1, prob = probabilities)
        
        if (A_d == 1 && S_d > 21) {
          S_d <- S_d - 10
          A_d <- 0
        }
        
        if (A_d == 0 && C == 1 && S_d + 10 <= 21) {
          S_d <- S_d + 10
          A_d <- 1
        }
        
        S_d <- S_d + C
      }
      
      if ((S_p == 21) && (S_d != 21)) {
        R_list <- c(R_list, 1)
      } else if (S_p > 21) {
        R_list <- c(R_list, -1)
      } else if (S_d > 21) {
        R_list <- c(R_list, 1)
      } else if (S_p > S_d) {
        R_list <- c(R_list, 1)
      } else if (S_p < S_d) {
        R_list <- c(R_list, -1)
      } else {
        R_list <- c(R_list, 0)
      }
    }
    
    if (length(S_list) > length(A_list)) {
      A_list <- c(A_list, 0)
    } else if (length(S_list) < length(A_list)) {
      S_list <- c(S_list, 0)
    }
    
    G <- 0
    len_S <- length(S_list)
    
    for (j in rev(seq(len_S))) {
      G <- G + R_list[1]
      U_look <- c(S_list[[j]], A_list[[j]])
      U[[paste(U_look, collapse = "_")]] <- c(U[[paste(U_look, collapse
                                                       