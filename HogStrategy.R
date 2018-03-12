library(dplyr)
library(rgl)

#Generate probabilities between 2-6, assign probabilities for each
one_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),1))),rep((1/6),length(rowSums(expand.grid(rep(list(2:6),1)))))); colnames(one_dice) = c('values','prob')
two_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),2))),rep((1/6)^2,length(rowSums(expand.grid(rep(list(2:6),2)))))); colnames(two_dice) = c('values','prob')
three_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),3))),rep((1/6)^3,length(rowSums(expand.grid(rep(list(2:6),3)))))); colnames(three_dice) = c('values','prob')
four_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),4))),rep((1/6)^4,length(rowSums(expand.grid(rep(list(2:6),4)))))); colnames(four_dice) = c('values','prob')
five_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),5))),rep((1/6)^5,length(rowSums(expand.grid(rep(list(2:6),5)))))); colnames(five_dice) = c('values','prob')
six_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),6))),rep((1/6)^6,length(rowSums(expand.grid(rep(list(2:6),6)))))); colnames(six_dice) = c('values','prob')
seven_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),7))),rep((1/6)^7,length(rowSums(expand.grid(rep(list(2:6),7)))))); colnames(seven_dice) = c('values','prob')
eight_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),8))),rep((1/6)^8,length(rowSums(expand.grid(rep(list(2:6),8)))))); colnames(eight_dice) = c('values','prob')
nine_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),9))),rep((1/6)^9,length(rowSums(expand.grid(rep(list(2:6),9)))))); colnames(nine_dice) = c('values','prob')
ten_dice = cbind.data.frame(rowSums(expand.grid(rep(list(2:6),10))),rep((1/6)^10,length(rowSums(expand.grid(rep(list(2:6),10)))))); colnames(ten_dice) = c('values','prob')

#Maybe enumerate 2 * n, n * 6, then generate length of 1/6, then generate table(_dice, assign probabiltiies off that?)


#Store the associated probability ranges for the problem because lookups take fairly long
one_p = 1- (5/6)^1
two_p = 1- (5/6)^2
three_p = 1- (5/6)^3
four_p = 1- (5/6)^4
five_p = 1- (5/6)^5
six_p = 1- (5/6)^6
seven_p = 1- (5/6)^7
eight_p = 1- (5/6)^8
nine_p = 1- (5/6)^9
ten_p = 1- (5/6)^10

one_r = unname(unlist((one_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
two_r = unname(unlist((two_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
three_r = unname(unlist((three_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
four_r = unname(unlist((four_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
five_r = unname(unlist((five_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
six_r = unname(unlist((six_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
seven_r = unname(unlist((seven_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
eight_r = unname(unlist((eight_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
nine_r = unname(unlist((nine_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))
ten_r = unname(unlist((ten_dice %>% group_by(values) %>% summarise(sum(prob)))[2]))

#Store min, max because lookups take fairly long
l1 = 2    ;h1 = 6
l2 = 2 * 2;h2 = 6 * 2
l3 = 2 * 3;h3 = 6 * 3
l4 = 2 * 4;h4 = 6 * 4
l5 = 2 * 5;h5 = 6 * 5
l6 = 2 * 6;h6 = 6 * 6
l7 = 2 * 7;h7 = 6 * 7
l8 = 2 * 8;h8 = 6 * 8
l9 = 2 * 9;h9 = 6 * 9
l10 = 2 * 10;h10 = 6 * 10	

#V is the probability of winning the game
V=array(0,c(100+(6*10),100+(6*10),100 + (6*10)))
#U is the optimal decision to make
U=array(0,c(100+(6*10),100+(6*10),100 + (6*10)))

#loop for all the possible sUms
for (ij in 199:0) {
  #run yoUr possible ValUes of i
  for (i in min(ij, (99 + (6*10))):max(0,ij-(99 + (6*10)))){
    j = ij - i
    #Run possible ValUes it takes to win 
    for (k in ((99 + (6*10))-i):0){  
      #j = ij - i
      if (i + k >= 100){ #Win
        V[i+1,j+1,k+1] = 1
        U[i+1,j+1,k+1] = 2
      }
      else{
        if (j >= 100){ # Lose
          V[i+1,j+1,k+1] = 0
          U[i+1,j+1,k+1] = 1 
        }
        
        else{ # Calculate probability of holding and rolling different numbers of dice
          V_hold = (1 - V[j+1,i+1+max(k,1),1])
          
          V_roll = (1 - V[j+1,i+1+1,1]) * one_p
          for (n in (l1:h1)){ V_roll = V_roll + V[i+1,j+1,k+1+n] * one_r[n-1]}
          
          V_roll2 = (1 - V[j+1,i+1+1,1]) * two_p
          for (n in (l2:h2)){ V_roll2 = V_roll2 + V[i+1,j+1,k+1+n] * two_r[n-3]}
          
          V_roll3 = (1 - V[j+1,i+1+1,1]) * three_p
          for (n in (l3:h3)){ V_roll3 = V_roll3 + V[i+1,j+1,k+1+n] * three_r[n-5]}
          
          V_roll4 = (1 - V[j+1,i+1+1,1]) * four_p
          for (n in (l4:h4)){ V_roll4 = V_roll4 + V[i+1,j+1,k+1+n] * four_r[n-7]}
          
          V_roll5 = (1 - V[j+1,i+1+1,1]) * five_p
          for (n in (l5:h5)){ V_roll5 = V_roll5 + V[i+1,j+1,k+1+n] * five_r[n-9]}
          
          V_roll6 = (1 - V[j+1,i+1+1,1]) * six_p
          for (n in (l6:h6)){ V_roll6 = V_roll6 + V[i+1,j+1,k+1+n] * six_r[n-11]}
          
          V_roll7 = (1 - V[j+1,i+1+1,1]) * seven_p
          for (n in (l7:h7)){ V_roll7 = V_roll7 + V[i+1,j+1,k+1+n] * seven_r[n-13]}
          
          V_roll8 = (1 - V[j+1,i+1+1,1]) * eight_p
          for (n in (l8:h8)){ V_roll8 = V_roll8 + V[i+1,j+1,k+1+n] * eight_r[n-15]}
          
          V_roll9 = (1 - V[j+1,i+1+1,1]) * nine_p
          for (n in (l9:h9)){ V_roll9 = V_roll9 + V[i+1,j+1,k+1+n] * nine_r[n-17]}
          
          V_roll10 = (1 - V[j+1,i+1+1,1]) * ten_p
          for (n in (l10:h10)){ V_roll10 = V_roll10 + V[i+1,j+1,k+1+n] * ten_r[n-19]}
          
          V[i+1,j+1,k+1]=max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold)
          
          if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) == V_roll10){ U[i+1,j+1,k+1] = 10}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll9){ U[i+1,j+1,k+1] = 9}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll8){ U[i+1,j+1,k+1] = 8}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll7){ U[i+1,j+1,k+1] = 7}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll6){ U[i+1,j+1,k+1] = 6}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll5){ U[i+1,j+1,k+1] = 5}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll4){ U[i+1,j+1,k+1] = 4}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll3){ U[i+1,j+1,k+1] = 3}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll2){ U[i+1,j+1,k+1] = 2}
          else if (max(V_roll,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_hold) ==V_roll){ U[i+1,j+1,k+1] = 1}
          else{U[i+1,j+1,k+1] = 0}
        }
      }
    }      
  }
}

plot3d = as.data.frame(V)

