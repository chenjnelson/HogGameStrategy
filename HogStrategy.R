library(dplyr)
library(Rmpfr) #Increase precision 
library(reshape2)
library(rgl)

#Choose r from n with precision
choose_large <- function(n, r){
  return( mpfr(factorialMpfr(n),1000) / (mpfr(factorialMpfr(r),1000) * mpfr(factorialMpfr(n-r),1000)))
}

#Calculate probability distribution of rolling a value in n dice, with no dice being a 1, influenced by http://mathworld.wolfram.com/Dice.html
calc_prob_dist <- function(n)
{
  s = 5
  counter = 1
  a = 0
  prob = c()
  for (p in (n:(n*s))){  
    for (k in 0:floor(((p-n)/s))){
      a = a + (-1) ^ k * choose_large(n,k) * choose_large((p-s*k-1),(n-1))
    }
    prob[counter] = as.numeric((a * ((1/6)^n)))
    a = 0
    counter = counter + 1
  }
  return (prob)
}

#Generate probability distributions
one_r = calc_prob_dist(1)
two_r = calc_prob_dist(2)
three_r = calc_prob_dist(3)
four_r = calc_prob_dist(4)
five_r = calc_prob_dist(5)
six_r = calc_prob_dist(6)
seven_r = calc_prob_dist(7)
eight_r = calc_prob_dist(8)
nine_r = calc_prob_dist(9)
ten_r = calc_prob_dist(10)
elevent_r = calc_prob_dist(11)
twelve_r = calc_prob_dist(12)
thirteen_r = calc_prob_dist(13)
fourteen_r = calc_prob_dist(14)
fifteen_r = calc_prob_dist(15)
sixteen_r = calc_prob_dist(16)
seventeen_r = calc_prob_dist(17)
eighteen_r = calc_prob_dist(18)
nineteen_r = calc_prob_dist(19)
twenty_r = calc_prob_dist(20)
twentyone_r = calc_prob_dist(21)
twentytwo_r = calc_prob_dist(22)
twentythree_r = calc_prob_dist(23)
twentyfour_r = calc_prob_dist(24)
twentyfive_r = calc_prob_dist(25)
twentysix_r = calc_prob_dist(26)
twentyseven_r = calc_prob_dist(27)
twentyeight_r = calc_prob_dist(28)
twentynine_r = calc_prob_dist(29)
thirty_r = calc_prob_dist(30)
thirtyone_r = calc_prob_dist(31)
thirtytwo_r = calc_prob_dist(32)
thirtythree_r = calc_prob_dist(33)
thirtyfour_r = calc_prob_dist(34)
thirtyfive_r = calc_prob_dist(35)
thirtysix_r = calc_prob_dist(36)
thirtyseven_r = calc_prob_dist(37)
thirtyeight_r = calc_prob_dist(38)
thirtynine_r = calc_prob_dist(39)
fourty_r = calc_prob_dist(40)
fourtyone_r = calc_prob_dist(41)
fourtytwo_r = calc_prob_dist(42)
fourtythree_r = calc_prob_dist(43)
fourtyfour_r = calc_prob_dist(44)
fourtyfive_r = calc_prob_dist(45)
fourtysix_r = calc_prob_dist(46)
fourtyseven_r = calc_prob_dist(47)
fourtyeight_r = calc_prob_dist(48)
fourtynine_r = calc_prob_dist(49)
fifty_r = calc_prob_dist(50)

#Generate probability distributions of not getting a 1
one_p = 1 - (5/6) ^1
two_p = 1 - (5/6) ^2
three_p = 1 - (5/6) ^3
four_p = 1 - (5/6) ^4
five_p = 1 - (5/6) ^5
six_p = 1 - (5/6) ^6
seven_p = 1 - (5/6) ^7
eight_p = 1 - (5/6) ^8
nine_p = 1 - (5/6) ^9
ten_p = 1 - (5/6) ^10
elevent_p = 1 - (5/6) ^11
twelve_p = 1 - (5/6) ^12
thirteen_p = 1 - (5/6) ^13
fourteen_p = 1 - (5/6) ^14
fifteen_p = 1 - (5/6) ^15
sixteen_p = 1 - (5/6) ^16
seventeen_p = 1 - (5/6) ^17
eighteen_p = 1 - (5/6) ^18
nineteen_p = 1 - (5/6) ^19
twenty_p = 1 - (5/6) ^20
twentyone_p = 1 - (5/6) ^21
twentytwo_p = 1 - (5/6) ^22
twentythree_p = 1 - (5/6) ^23
twentyfour_p = 1 - (5/6) ^24
twentyfive_p = 1 - (5/6) ^25
twentysix_p = 1 - (5/6) ^26
twentyseven_p = 1 - (5/6) ^27
twentyeight_p = 1 - (5/6) ^28
twentynine_p = 1 - (5/6) ^29
thirty_p = 1 - (5/6) ^30
thirtyone_p = 1 - (5/6) ^31
thirtytwo_p = 1 - (5/6) ^32
thirtythree_p = 1 - (5/6) ^33
thirtyfour_p = 1 - (5/6) ^34
thirtyfive_p = 1 - (5/6) ^35
thirtysix_p = 1 - (5/6) ^36
thirtyseven_p = 1 - (5/6) ^37
thirtyeight_p = 1 - (5/6) ^38
thirtynine_p = 1 - (5/6) ^39
fourty_p = 1 - (5/6) ^40
fourtyone_p = 1 - (5/6) ^41
fourtytwo_p = 1 - (5/6) ^42
fourtythree_p = 1 - (5/6) ^43
fourtyfour_p = 1 - (5/6) ^44
fourtyfive_p = 1 - (5/6) ^45
fourtysix_p = 1 - (5/6) ^46
fourtyseven_p = 1 - (5/6) ^47
fourtyeight_p = 1 - (5/6) ^48
fourtynine_p = 1 - (5/6) ^49
fifty_p = 1 - (5/6) ^50

#Store min, max because lookups take fairly long
l1 = 2 *1; h1 =1 * 6
l2 = 2 *2; h2 =2 * 6
l3 = 2 *3; h3 =3 * 6
l4 = 2 *4; h4 =4 * 6
l5 = 2 *5; h5 =5 * 6
l6 = 2 *6; h6 =6 * 6
l7 = 2 *7; h7 =7 * 6
l8 = 2 *8; h8 =8 * 6
l9 = 2 *9; h9 =9 * 6
l10 = 2 *10; h10 =10 * 6
l11 = 2 *11; h11 =11 * 6
l12 = 2 *12; h12 =12 * 6
l13 = 2 *13; h13 =13 * 6
l14 = 2 *14; h14 =14 * 6
l15 = 2 *15; h15 =15 * 6
l16 = 2 *16; h16 =16 * 6
l17 = 2 *17; h17 =17 * 6
l18 = 2 *18; h18 =18 * 6
l19 = 2 *19; h19 =19 * 6
l20 = 2 *20; h20 =20 * 6
l21 = 2 *21; h21 =21 * 6
l22 = 2 *22; h22 =22 * 6
l23 = 2 *23; h23 =23 * 6
l24 = 2 *24; h24 =24 * 6
l25 = 2 *25; h25 =25 * 6
l26 = 2 *26; h26 =26 * 6
l27 = 2 *27; h27 =27 * 6
l28 = 2 *28; h28 =28 * 6
l29 = 2 *29; h29 =29 * 6
l30 = 2 *30; h30 =30 * 6
l31 = 2 *31; h31 =31 * 6
l32 = 2 *32; h32 =32 * 6
l33 = 2 *33; h33 =33 * 6
l34 = 2 *34; h34 =34 * 6
l35 = 2 *35; h35 =35 * 6
l36 = 2 *36; h36 =36 * 6
l37 = 2 *37; h37 =37 * 6
l38 = 2 *38; h38 =38 * 6
l39 = 2 *39; h39 =39 * 6
l40 = 2 *40; h40 =40 * 6
l41 = 2 *41; h41 =41 * 6
l42 = 2 *42; h42 =42 * 6
l43 = 2 *43; h43 =43 * 6
l44 = 2 *44; h44 =44 * 6
l45 = 2 *45; h45 =45 * 6
l46 = 2 *46; h46 =46 * 6
l47 = 2 *47; h47 =47 * 6
l48 = 2 *48; h48 =48 * 6
l49 = 2 *49; h49 =49 * 6
l50 = 2 *50; h50 =50 * 6

#V is the probability of winning the game
V=array(0,c(100+(6*50),100+(6*50),100 + (6*50)))
#U is the optimal decision to make
U=array(0,c(100+(6*50),100+(6*50),100 + (6*50)))

#loop for all the possible sUms
for (ij in 199:0) {
  #run yoUr possible ValUes of i
  for (i in min(ij, (99 + (6*50))):max(0,ij-(99 + (6*50)))){
    j = ij - i
    #Run possible ValUes it takes to win 
    for (k in ((99 + (6*50))-i):0){  

      #I might need to refix this part
      if (i + k >= 100){ #Win
        V[i+1,j+1,k+1] = 1
        U[i+1,j+1,k+1] = 0 #roll
      }
      else{
        if (j >= 100){ # Lose
          V[i+1,j+1,k+1] = 0
          U[i+1,j+1,k+1] = 0 #nothing
        }
        
        else{ # Calculate probability of holding and rolling different numbers of dice
          V_hold = (1 - V[j+1,i+1+max(k,1),1])
          
          V_roll1 = (1 - V[j+1,i+1+1,1]) * one_p
          V_roll2 = (1 - V[j+1,i+1+1,1]) * two_p
          V_roll3 = (1 - V[j+1,i+1+1,1]) * three_p
          V_roll4 = (1 - V[j+1,i+1+1,1]) * four_p
          V_roll5 = (1 - V[j+1,i+1+1,1]) * five_p
          V_roll6 = (1 - V[j+1,i+1+1,1]) * six_p
          V_roll7 = (1 - V[j+1,i+1+1,1]) * seven_p
          V_roll8 = (1 - V[j+1,i+1+1,1]) * eight_p
          V_roll9 = (1 - V[j+1,i+1+1,1]) * nine_p
          V_roll10 = (1 - V[j+1,i+1+1,1]) * ten_p
          V_roll11 = (1 - V[j+1,i+1+1,1]) * elevent_p
          V_roll12 = (1 - V[j+1,i+1+1,1]) * twelve_p
          V_roll13 = (1 - V[j+1,i+1+1,1]) * thirteen_p
          V_roll14 = (1 - V[j+1,i+1+1,1]) * fourteen_p
          V_roll15 = (1 - V[j+1,i+1+1,1]) * fifteen_p
          V_roll16 = (1 - V[j+1,i+1+1,1]) * sixteen_p
          V_roll17 = (1 - V[j+1,i+1+1,1]) * seventeen_p
          V_roll18 = (1 - V[j+1,i+1+1,1]) * eighteen_p
          V_roll19 = (1 - V[j+1,i+1+1,1]) * nineteen_p
          V_roll20 = (1 - V[j+1,i+1+1,1]) * twenty_p
          V_roll21 = (1 - V[j+1,i+1+1,1]) * twentyone_p
          V_roll22 = (1 - V[j+1,i+1+1,1]) * twentytwo_p
          V_roll23 = (1 - V[j+1,i+1+1,1]) * twentythree_p
          V_roll24 = (1 - V[j+1,i+1+1,1]) * twentyfour_p
          V_roll25 = (1 - V[j+1,i+1+1,1]) * twentyfive_p
          V_roll26 = (1 - V[j+1,i+1+1,1]) * twentysix_p
          V_roll27 = (1 - V[j+1,i+1+1,1]) * twentyseven_p
          V_roll28 = (1 - V[j+1,i+1+1,1]) * twentyeight_p
          V_roll29 = (1 - V[j+1,i+1+1,1]) * twentynine_p
          V_roll30 = (1 - V[j+1,i+1+1,1]) * thirty_p
          V_roll31 = (1 - V[j+1,i+1+1,1]) * thirtyone_p
          V_roll32 = (1 - V[j+1,i+1+1,1]) * thirtytwo_p
          V_roll33 = (1 - V[j+1,i+1+1,1]) * thirtythree_p
          V_roll34 = (1 - V[j+1,i+1+1,1]) * thirtyfour_p
          V_roll35 = (1 - V[j+1,i+1+1,1]) * thirtyfive_p
          V_roll36 = (1 - V[j+1,i+1+1,1]) * thirtysix_p
          V_roll37 = (1 - V[j+1,i+1+1,1]) * thirtyseven_p
          V_roll38 = (1 - V[j+1,i+1+1,1]) * thirtyeight_p
          V_roll39 = (1 - V[j+1,i+1+1,1]) * thirtynine_p
          V_roll40 = (1 - V[j+1,i+1+1,1]) * fourty_p
          V_roll41 = (1 - V[j+1,i+1+1,1]) * fourtyone_p
          V_roll42 = (1 - V[j+1,i+1+1,1]) * fourtytwo_p
          V_roll43 = (1 - V[j+1,i+1+1,1]) * fourtythree_p
          V_roll44 = (1 - V[j+1,i+1+1,1]) * fourtyfour_p
          V_roll45 = (1 - V[j+1,i+1+1,1]) * fourtyfive_p
          V_roll46 = (1 - V[j+1,i+1+1,1]) * fourtysix_p
          V_roll47 = (1 - V[j+1,i+1+1,1]) * fourtyseven_p
          V_roll48 = (1 - V[j+1,i+1+1,1]) * fourtyeight_p
          V_roll49 = (1 - V[j+1,i+1+1,1]) * fourtynine_p
          V_roll50 = (1 - V[j+1,i+1+1,1]) * fifty_p
          
          for (n in (l1:h1)){V_roll1 = V_roll1 + V[i+1,j+1,k+1+n] * one_r[n-1]}
          for (n in (l2:h2)){V_roll2 = V_roll2 + V[i+1,j+1,k+1+n] * two_r[n-3]}
          for (n in (l3:h3)){V_roll3 = V_roll3 + V[i+1,j+1,k+1+n] * three_r[n-5]}
          for (n in (l4:h4)){V_roll4 = V_roll4 + V[i+1,j+1,k+1+n] * four_r[n-7]}
          for (n in (l5:h5)){V_roll5 = V_roll5 + V[i+1,j+1,k+1+n] * five_r[n-9]}
          for (n in (l6:h6)){V_roll6 = V_roll6 + V[i+1,j+1,k+1+n] * six_r[n-11]}
          for (n in (l7:h7)){V_roll7 = V_roll7 + V[i+1,j+1,k+1+n] * seven_r[n-13]}
          for (n in (l8:h8)){V_roll8 = V_roll8 + V[i+1,j+1,k+1+n] * eight_r[n-15]}
          for (n in (l9:h9)){V_roll9 = V_roll9 + V[i+1,j+1,k+1+n] * nine_r[n-17]}
          for (n in (l10:h10)){V_roll10 = V_roll10 + V[i+1,j+1,k+1+n] * ten_r[n-19]}
          for (n in (l11:h11)){V_roll11 = V_roll11 + V[i+1,j+1,k+1+n] * elevent_r[n-21]}
          for (n in (l12:h12)){V_roll12 = V_roll12 + V[i+1,j+1,k+1+n] * twelve_r[n-23]}
          for (n in (l13:h13)){V_roll13 = V_roll13 + V[i+1,j+1,k+1+n] * thirteen_r[n-25]}
          for (n in (l14:h14)){V_roll14 = V_roll14 + V[i+1,j+1,k+1+n] * fourteen_r[n-27]}
          for (n in (l15:h15)){V_roll15 = V_roll15 + V[i+1,j+1,k+1+n] * fifteen_r[n-29]}
          for (n in (l16:h16)){V_roll16 = V_roll16 + V[i+1,j+1,k+1+n] * sixteen_r[n-31]}
          for (n in (l17:h17)){V_roll17 = V_roll17 + V[i+1,j+1,k+1+n] * seventeen_r[n-33]}
          for (n in (l18:h18)){V_roll18 = V_roll18 + V[i+1,j+1,k+1+n] * eighteen_r[n-35]}
          for (n in (l19:h19)){V_roll19 = V_roll19 + V[i+1,j+1,k+1+n] * nineteen_r[n-37]}
          for (n in (l20:h20)){V_roll20 = V_roll20 + V[i+1,j+1,k+1+n] * twenty_r[n-39]}
          for (n in (l21:h21)){V_roll21 = V_roll21 + V[i+1,j+1,k+1+n] * twentyone_r[n-41]}
          for (n in (l22:h22)){V_roll22 = V_roll22 + V[i+1,j+1,k+1+n] * twentytwo_r[n-43]}
          for (n in (l23:h23)){V_roll23 = V_roll23 + V[i+1,j+1,k+1+n] * twentythree_r[n-45]}
          for (n in (l24:h24)){V_roll24 = V_roll24 + V[i+1,j+1,k+1+n] * twentyfour_r[n-47]}
          for (n in (l25:h25)){V_roll25 = V_roll25 + V[i+1,j+1,k+1+n] * twentyfive_r[n-49]}
          for (n in (l26:h26)){V_roll26 = V_roll26 + V[i+1,j+1,k+1+n] * twentysix_r[n-51]}
          for (n in (l27:h27)){V_roll27 = V_roll27 + V[i+1,j+1,k+1+n] * twentyseven_r[n-53]}
          for (n in (l28:h28)){V_roll28 = V_roll28 + V[i+1,j+1,k+1+n] * twentyeight_r[n-55]}
          for (n in (l29:h29)){V_roll29 = V_roll29 + V[i+1,j+1,k+1+n] * twentynine_r[n-57]}
          for (n in (l30:h30)){V_roll30 = V_roll30 + V[i+1,j+1,k+1+n] * thirty_r[n-59]}
          for (n in (l31:h31)){V_roll31 = V_roll31 + V[i+1,j+1,k+1+n] * thirtyone_r[n-61]}
          for (n in (l32:h32)){V_roll32 = V_roll32 + V[i+1,j+1,k+1+n] * thirtytwo_r[n-63]}
          for (n in (l33:h33)){V_roll33 = V_roll33 + V[i+1,j+1,k+1+n] * thirtythree_r[n-65]}
          for (n in (l34:h34)){V_roll34 = V_roll34 + V[i+1,j+1,k+1+n] * thirtyfour_r[n-67]}
          for (n in (l35:h35)){V_roll35 = V_roll35 + V[i+1,j+1,k+1+n] * thirtyfive_r[n-69]}
          for (n in (l36:h36)){V_roll36 = V_roll36 + V[i+1,j+1,k+1+n] * thirtysix_r[n-71]}
          for (n in (l37:h37)){V_roll37 = V_roll37 + V[i+1,j+1,k+1+n] * thirtyseven_r[n-73]}
          for (n in (l38:h38)){V_roll38 = V_roll38 + V[i+1,j+1,k+1+n] * thirtyeight_r[n-75]}
          for (n in (l39:h39)){V_roll39 = V_roll39 + V[i+1,j+1,k+1+n] * thirtynine_r[n-77]}
          for (n in (l40:h40)){V_roll40 = V_roll40 + V[i+1,j+1,k+1+n] * fourty_r[n-79]}
          for (n in (l41:h41)){V_roll41 = V_roll41 + V[i+1,j+1,k+1+n] * fourtyone_r[n-81]}
          for (n in (l42:h42)){V_roll42 = V_roll42 + V[i+1,j+1,k+1+n] * fourtytwo_r[n-83]}
          for (n in (l43:h43)){V_roll43 = V_roll43 + V[i+1,j+1,k+1+n] * fourtythree_r[n-85]}
          for (n in (l44:h44)){V_roll44 = V_roll44 + V[i+1,j+1,k+1+n] * fourtyfour_r[n-87]}
          for (n in (l45:h45)){V_roll45 = V_roll45 + V[i+1,j+1,k+1+n] * fourtyfive_r[n-89]}
          for (n in (l46:h46)){V_roll46 = V_roll46 + V[i+1,j+1,k+1+n] * fourtysix_r[n-91]}
          for (n in (l47:h47)){V_roll47 = V_roll47 + V[i+1,j+1,k+1+n] * fourtyseven_r[n-93]}
          for (n in (l48:h48)){V_roll48 = V_roll48 + V[i+1,j+1,k+1+n] * fourtyeight_r[n-95]}
          for (n in (l49:h49)){V_roll49 = V_roll49 + V[i+1,j+1,k+1+n] * fourtynine_r[n-97]}
          for (n in (l50:h50)){V_roll50 = V_roll50 + V[i+1,j+1,k+1+n] * fifty_r[n-99]}
          
          max_prob = max(V_roll1,V_roll2,V_roll3,V_roll4,V_roll5,V_roll6,V_roll7,V_roll8,V_roll9,V_roll10,V_roll11,V_roll12,V_roll13,V_roll14,V_roll15,V_roll16,V_roll17,V_roll18,V_roll19,V_roll20,V_roll21,V_roll22,V_roll23,V_roll24,V_roll25,V_roll26,V_roll27,V_roll28,V_roll29,V_roll30,V_roll31,V_roll32,V_roll33,V_roll34,V_roll35,V_roll36,V_roll37,V_roll38,V_roll39,V_roll40,V_roll41,V_roll42,V_roll43,V_roll44,V_roll45,V_roll46,V_roll47,V_roll48,V_roll49,V_roll50,V_hold)
          V[i+1,j+1,k+1]=max_prob
          
          if (max_prob == V_roll50){ U[i+1,j+1,k+1] = 50}
          else if (max_prob ==V_roll49){ U[i+1,j+1,k+1] = 49}
          else if (max_prob ==V_roll48){ U[i+1,j+1,k+1] = 48}
          else if (max_prob ==V_roll47){ U[i+1,j+1,k+1] = 47}
          else if (max_prob ==V_roll46){ U[i+1,j+1,k+1] = 46}
          else if (max_prob ==V_roll45){ U[i+1,j+1,k+1] = 45}
          else if (max_prob ==V_roll44){ U[i+1,j+1,k+1] = 44}
          else if (max_prob ==V_roll43){ U[i+1,j+1,k+1] = 43}
          else if (max_prob ==V_roll42){ U[i+1,j+1,k+1] = 42}
          else if (max_prob ==V_roll41){ U[i+1,j+1,k+1] = 41}
          else if (max_prob ==V_roll40){ U[i+1,j+1,k+1] = 40}
          else if (max_prob ==V_roll39){ U[i+1,j+1,k+1] = 39}
          else if (max_prob ==V_roll38){ U[i+1,j+1,k+1] = 38}
          else if (max_prob ==V_roll37){ U[i+1,j+1,k+1] = 37}
          else if (max_prob ==V_roll36){ U[i+1,j+1,k+1] = 36}
          else if (max_prob ==V_roll35){ U[i+1,j+1,k+1] = 35}
          else if (max_prob ==V_roll34){ U[i+1,j+1,k+1] = 34}
          else if (max_prob ==V_roll33){ U[i+1,j+1,k+1] = 33}
          else if (max_prob ==V_roll32){ U[i+1,j+1,k+1] = 32}
          else if (max_prob ==V_roll31){ U[i+1,j+1,k+1] = 31}   
          else if (max_prob ==V_roll30){ U[i+1,j+1,k+1] = 30}       
          else if (max_prob ==V_roll29){ U[i+1,j+1,k+1] = 29}
          else if (max_prob ==V_roll28){ U[i+1,j+1,k+1] = 28}
          else if (max_prob ==V_roll27){ U[i+1,j+1,k+1] = 27}
          else if (max_prob ==V_roll26){ U[i+1,j+1,k+1] = 26}
          else if (max_prob ==V_roll25){ U[i+1,j+1,k+1] = 25}
          else if (max_prob ==V_roll24){ U[i+1,j+1,k+1] = 24}
          else if (max_prob ==V_roll23){ U[i+1,j+1,k+1] = 23}
          else if (max_prob ==V_roll22){ U[i+1,j+1,k+1] = 22}
          else if (max_prob ==V_roll21){ U[i+1,j+1,k+1] = 21} 
          else if (max_prob ==V_roll20){ U[i+1,j+1,k+1] = 20}
          else if (max_prob ==V_roll19){ U[i+1,j+1,k+1] = 19}
          else if (max_prob ==V_roll18){ U[i+1,j+1,k+1] = 18}
          else if (max_prob ==V_roll17){ U[i+1,j+1,k+1] = 17}
          else if (max_prob ==V_roll16){ U[i+1,j+1,k+1] = 16}
          else if (max_prob ==V_roll15){ U[i+1,j+1,k+1] = 15}
          else if (max_prob ==V_roll14){ U[i+1,j+1,k+1] = 14}
          else if (max_prob ==V_roll13){ U[i+1,j+1,k+1] = 13}
          else if (max_prob ==V_roll12){ U[i+1,j+1,k+1] = 12}
          else if (max_prob ==V_roll11){ U[i+1,j+1,k+1] = 11}
          else if (max_prob ==V_roll10){ U[i+1,j+1,k+1] = 10}
          else if (max_prob ==V_roll9){ U[i+1,j+1,k+1] = 9}
          else if (max_prob ==V_roll8){ U[i+1,j+1,k+1] = 8}
          else if (max_prob ==V_roll7){ U[i+1,j+1,k+1] = 7}
          else if (max_prob ==V_roll6){ U[i+1,j+1,k+1] = 6}
          else if (max_prob ==V_roll5){ U[i+1,j+1,k+1] = 5}
          else if (max_prob ==V_roll4){ U[i+1,j+1,k+1] = 4}
          else if (max_prob ==V_roll3){ U[i+1,j+1,k+1] = 3}
          else if (max_prob ==V_roll2){ U[i+1,j+1,k+1] = 2}
          else if (max_prob ==V_roll1){ U[i+1,j+1,k+1] = 1}
          else{U[i+1,j+1,k+1] = 0} #hodl
        }
      }
    }      
  }
}


save(list = c('V','U'),file = 'VUfile.Rdata')  
load("VUfile.RData")

optimal_roll = melt(U)
optimal_prob = melt(V)
optimal_roll[optimal_roll['value'] == 33,] #You are going to win anyways with at 99
unique(optimal_roll['value']) #Most you will ever gamble is 16 otherwise

optimal_prob[optimal_roll['value'] == 16,]

