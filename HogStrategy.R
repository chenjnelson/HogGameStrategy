library(dplyr)
library(Rmpfr) #Increase precision 
library(reshape2)

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

#P is the probability of winning the game
P=array(0,c(100+(6*50),100+(6*50),100 + (6*50)))
#D is the optimal decision to make
D=array(0,c(100+(6*50),100+(6*50),100 + (6*50)))

#loop for all the possible sUms
for (ij in 199:0) {
  #run yoUr possible ValUes of i
  for (i in min(ij, (99 + (6*50))):max(0,ij-(99 + (6*50)))){
    j = ij - i
    #Run possible ValUes it takes to win 
    for (k in ((99 + (6*50))-i):0){  

      #I might need to refix this part
      if (i + k >= 100){ #Win
        P[i+1,j+1,k+1] = 1
        D[i+1,j+1,k+1] = 0 #hold
      }
      else{
        if (j >= 100){ # Lose
          P[i+1,j+1,k+1] = 0
          D[i+1,j+1,k+1] = 99 #nothing since you lost
        }
        
        else{ # Calculate probability of holding and rolling different numbers of dice
          P_hold = (1 - P[j+1,i+1+max(k,1),1])
          
          P_roll1 = (1 - P[j+1,i+1+1,1]) * one_p
          P_roll2 = (1 - P[j+1,i+1+1,1]) * two_p
          P_roll3 = (1 - P[j+1,i+1+1,1]) * three_p
          P_roll4 = (1 - P[j+1,i+1+1,1]) * four_p
          P_roll5 = (1 - P[j+1,i+1+1,1]) * five_p
          P_roll6 = (1 - P[j+1,i+1+1,1]) * six_p
          P_roll7 = (1 - P[j+1,i+1+1,1]) * seven_p
          P_roll8 = (1 - P[j+1,i+1+1,1]) * eight_p
          P_roll9 = (1 - P[j+1,i+1+1,1]) * nine_p
          P_roll10 = (1 - P[j+1,i+1+1,1]) * ten_p
          P_roll11 = (1 - P[j+1,i+1+1,1]) * elevent_p
          P_roll12 = (1 - P[j+1,i+1+1,1]) * twelve_p
          P_roll13 = (1 - P[j+1,i+1+1,1]) * thirteen_p
          P_roll14 = (1 - P[j+1,i+1+1,1]) * fourteen_p
          P_roll15 = (1 - P[j+1,i+1+1,1]) * fifteen_p
          P_roll16 = (1 - P[j+1,i+1+1,1]) * sixteen_p
          P_roll17 = (1 - P[j+1,i+1+1,1]) * seventeen_p
          P_roll18 = (1 - P[j+1,i+1+1,1]) * eighteen_p
          P_roll19 = (1 - P[j+1,i+1+1,1]) * nineteen_p
          P_roll20 = (1 - P[j+1,i+1+1,1]) * twenty_p
          P_roll21 = (1 - P[j+1,i+1+1,1]) * twentyone_p
          P_roll22 = (1 - P[j+1,i+1+1,1]) * twentytwo_p
          P_roll23 = (1 - P[j+1,i+1+1,1]) * twentythree_p
          P_roll24 = (1 - P[j+1,i+1+1,1]) * twentyfour_p
          P_roll25 = (1 - P[j+1,i+1+1,1]) * twentyfive_p
          P_roll26 = (1 - P[j+1,i+1+1,1]) * twentysix_p
          P_roll27 = (1 - P[j+1,i+1+1,1]) * twentyseven_p
          P_roll28 = (1 - P[j+1,i+1+1,1]) * twentyeight_p
          P_roll29 = (1 - P[j+1,i+1+1,1]) * twentynine_p
          P_roll30 = (1 - P[j+1,i+1+1,1]) * thirty_p
          P_roll31 = (1 - P[j+1,i+1+1,1]) * thirtyone_p
          P_roll32 = (1 - P[j+1,i+1+1,1]) * thirtytwo_p
          P_roll33 = (1 - P[j+1,i+1+1,1]) * thirtythree_p
          P_roll34 = (1 - P[j+1,i+1+1,1]) * thirtyfour_p
          P_roll35 = (1 - P[j+1,i+1+1,1]) * thirtyfive_p
          P_roll36 = (1 - P[j+1,i+1+1,1]) * thirtysix_p
          P_roll37 = (1 - P[j+1,i+1+1,1]) * thirtyseven_p
          P_roll38 = (1 - P[j+1,i+1+1,1]) * thirtyeight_p
          P_roll39 = (1 - P[j+1,i+1+1,1]) * thirtynine_p
          P_roll40 = (1 - P[j+1,i+1+1,1]) * fourty_p
          P_roll41 = (1 - P[j+1,i+1+1,1]) * fourtyone_p
          P_roll42 = (1 - P[j+1,i+1+1,1]) * fourtytwo_p
          P_roll43 = (1 - P[j+1,i+1+1,1]) * fourtythree_p
          P_roll44 = (1 - P[j+1,i+1+1,1]) * fourtyfour_p
          P_roll45 = (1 - P[j+1,i+1+1,1]) * fourtyfive_p
          P_roll46 = (1 - P[j+1,i+1+1,1]) * fourtysix_p
          P_roll47 = (1 - P[j+1,i+1+1,1]) * fourtyseven_p
          P_roll48 = (1 - P[j+1,i+1+1,1]) * fourtyeight_p
          P_roll49 = (1 - P[j+1,i+1+1,1]) * fourtynine_p
          P_roll50 = (1 - P[j+1,i+1+1,1]) * fifty_p
          
          for (n in (l1:h1)){P_roll1 = P_roll1 + P[i+1,j+1,k+1+n] * one_r[n-1]}
          for (n in (l2:h2)){P_roll2 = P_roll2 + P[i+1,j+1,k+1+n] * two_r[n-3]}
          for (n in (l3:h3)){P_roll3 = P_roll3 + P[i+1,j+1,k+1+n] * three_r[n-5]}
          for (n in (l4:h4)){P_roll4 = P_roll4 + P[i+1,j+1,k+1+n] * four_r[n-7]}
          for (n in (l5:h5)){P_roll5 = P_roll5 + P[i+1,j+1,k+1+n] * five_r[n-9]}
          for (n in (l6:h6)){P_roll6 = P_roll6 + P[i+1,j+1,k+1+n] * six_r[n-11]}
          for (n in (l7:h7)){P_roll7 = P_roll7 + P[i+1,j+1,k+1+n] * seven_r[n-13]}
          for (n in (l8:h8)){P_roll8 = P_roll8 + P[i+1,j+1,k+1+n] * eight_r[n-15]}
          for (n in (l9:h9)){P_roll9 = P_roll9 + P[i+1,j+1,k+1+n] * nine_r[n-17]}
          for (n in (l10:h10)){P_roll10 = P_roll10 + P[i+1,j+1,k+1+n] * ten_r[n-19]}
          for (n in (l11:h11)){P_roll11 = P_roll11 + P[i+1,j+1,k+1+n] * elevent_r[n-21]}
          for (n in (l12:h12)){P_roll12 = P_roll12 + P[i+1,j+1,k+1+n] * twelve_r[n-23]}
          for (n in (l13:h13)){P_roll13 = P_roll13 + P[i+1,j+1,k+1+n] * thirteen_r[n-25]}
          for (n in (l14:h14)){P_roll14 = P_roll14 + P[i+1,j+1,k+1+n] * fourteen_r[n-27]}
          for (n in (l15:h15)){P_roll15 = P_roll15 + P[i+1,j+1,k+1+n] * fifteen_r[n-29]}
          for (n in (l16:h16)){P_roll16 = P_roll16 + P[i+1,j+1,k+1+n] * sixteen_r[n-31]}
          for (n in (l17:h17)){P_roll17 = P_roll17 + P[i+1,j+1,k+1+n] * seventeen_r[n-33]}
          for (n in (l18:h18)){P_roll18 = P_roll18 + P[i+1,j+1,k+1+n] * eighteen_r[n-35]}
          for (n in (l19:h19)){P_roll19 = P_roll19 + P[i+1,j+1,k+1+n] * nineteen_r[n-37]}
          for (n in (l20:h20)){P_roll20 = P_roll20 + P[i+1,j+1,k+1+n] * twenty_r[n-39]}
          for (n in (l21:h21)){P_roll21 = P_roll21 + P[i+1,j+1,k+1+n] * twentyone_r[n-41]}
          for (n in (l22:h22)){P_roll22 = P_roll22 + P[i+1,j+1,k+1+n] * twentytwo_r[n-43]}
          for (n in (l23:h23)){P_roll23 = P_roll23 + P[i+1,j+1,k+1+n] * twentythree_r[n-45]}
          for (n in (l24:h24)){P_roll24 = P_roll24 + P[i+1,j+1,k+1+n] * twentyfour_r[n-47]}
          for (n in (l25:h25)){P_roll25 = P_roll25 + P[i+1,j+1,k+1+n] * twentyfive_r[n-49]}
          for (n in (l26:h26)){P_roll26 = P_roll26 + P[i+1,j+1,k+1+n] * twentysix_r[n-51]}
          for (n in (l27:h27)){P_roll27 = P_roll27 + P[i+1,j+1,k+1+n] * twentyseven_r[n-53]}
          for (n in (l28:h28)){P_roll28 = P_roll28 + P[i+1,j+1,k+1+n] * twentyeight_r[n-55]}
          for (n in (l29:h29)){P_roll29 = P_roll29 + P[i+1,j+1,k+1+n] * twentynine_r[n-57]}
          for (n in (l30:h30)){P_roll30 = P_roll30 + P[i+1,j+1,k+1+n] * thirty_r[n-59]}
          for (n in (l31:h31)){P_roll31 = P_roll31 + P[i+1,j+1,k+1+n] * thirtyone_r[n-61]}
          for (n in (l32:h32)){P_roll32 = P_roll32 + P[i+1,j+1,k+1+n] * thirtytwo_r[n-63]}
          for (n in (l33:h33)){P_roll33 = P_roll33 + P[i+1,j+1,k+1+n] * thirtythree_r[n-65]}
          for (n in (l34:h34)){P_roll34 = P_roll34 + P[i+1,j+1,k+1+n] * thirtyfour_r[n-67]}
          for (n in (l35:h35)){P_roll35 = P_roll35 + P[i+1,j+1,k+1+n] * thirtyfive_r[n-69]}
          for (n in (l36:h36)){P_roll36 = P_roll36 + P[i+1,j+1,k+1+n] * thirtysix_r[n-71]}
          for (n in (l37:h37)){P_roll37 = P_roll37 + P[i+1,j+1,k+1+n] * thirtyseven_r[n-73]}
          for (n in (l38:h38)){P_roll38 = P_roll38 + P[i+1,j+1,k+1+n] * thirtyeight_r[n-75]}
          for (n in (l39:h39)){P_roll39 = P_roll39 + P[i+1,j+1,k+1+n] * thirtynine_r[n-77]}
          for (n in (l40:h40)){P_roll40 = P_roll40 + P[i+1,j+1,k+1+n] * fourty_r[n-79]}
          for (n in (l41:h41)){P_roll41 = P_roll41 + P[i+1,j+1,k+1+n] * fourtyone_r[n-81]}
          for (n in (l42:h42)){P_roll42 = P_roll42 + P[i+1,j+1,k+1+n] * fourtytwo_r[n-83]}
          for (n in (l43:h43)){P_roll43 = P_roll43 + P[i+1,j+1,k+1+n] * fourtythree_r[n-85]}
          for (n in (l44:h44)){P_roll44 = P_roll44 + P[i+1,j+1,k+1+n] * fourtyfour_r[n-87]}
          for (n in (l45:h45)){P_roll45 = P_roll45 + P[i+1,j+1,k+1+n] * fourtyfive_r[n-89]}
          for (n in (l46:h46)){P_roll46 = P_roll46 + P[i+1,j+1,k+1+n] * fourtysix_r[n-91]}
          for (n in (l47:h47)){P_roll47 = P_roll47 + P[i+1,j+1,k+1+n] * fourtyseven_r[n-93]}
          for (n in (l48:h48)){P_roll48 = P_roll48 + P[i+1,j+1,k+1+n] * fourtyeight_r[n-95]}
          for (n in (l49:h49)){P_roll49 = P_roll49 + P[i+1,j+1,k+1+n] * fourtynine_r[n-97]}
          for (n in (l50:h50)){P_roll50 = P_roll50 + P[i+1,j+1,k+1+n] * fifty_r[n-99]}
          
          max_prob = max(P_roll1,P_roll2,P_roll3,P_roll4,P_roll5,P_roll6,P_roll7,P_roll8,P_roll9,P_roll10,P_roll11,P_roll12,P_roll13,P_roll14,P_roll15,P_roll16,P_roll17,P_roll18,P_roll19,P_roll20,P_roll21,P_roll22,P_roll23,P_roll24,P_roll25,P_roll26,P_roll27,P_roll28,P_roll29,P_roll30,P_roll31,P_roll32,P_roll33,P_roll34,P_roll35,P_roll36,P_roll37,P_roll38,P_roll39,P_roll40,P_roll41,P_roll42,P_roll43,P_roll44,P_roll45,P_roll46,P_roll47,P_roll48,P_roll49,P_roll50,P_hold)
          P[i+1,j+1,k+1]=max_prob
          
          if (max_prob == P_roll50){ D[i+1,j+1,k+1] = 50}
          else if (max_prob ==P_roll49){ D[i+1,j+1,k+1] = 49}
          else if (max_prob ==P_roll48){ D[i+1,j+1,k+1] = 48}
          else if (max_prob ==P_roll47){ D[i+1,j+1,k+1] = 47}
          else if (max_prob ==P_roll46){ D[i+1,j+1,k+1] = 46}
          else if (max_prob ==P_roll45){ D[i+1,j+1,k+1] = 45}
          else if (max_prob ==P_roll44){ D[i+1,j+1,k+1] = 44}
          else if (max_prob ==P_roll43){ D[i+1,j+1,k+1] = 43}
          else if (max_prob ==P_roll42){ D[i+1,j+1,k+1] = 42}
          else if (max_prob ==P_roll41){ D[i+1,j+1,k+1] = 41}
          else if (max_prob ==P_roll40){ D[i+1,j+1,k+1] = 40}
          else if (max_prob ==P_roll39){ D[i+1,j+1,k+1] = 39}
          else if (max_prob ==P_roll38){ D[i+1,j+1,k+1] = 38}
          else if (max_prob ==P_roll37){ D[i+1,j+1,k+1] = 37}
          else if (max_prob ==P_roll36){ D[i+1,j+1,k+1] = 36}
          else if (max_prob ==P_roll35){ D[i+1,j+1,k+1] = 35}
          else if (max_prob ==P_roll34){ D[i+1,j+1,k+1] = 34}
          else if (max_prob ==P_roll33){ D[i+1,j+1,k+1] = 33}
          else if (max_prob ==P_roll32){ D[i+1,j+1,k+1] = 32}
          else if (max_prob ==P_roll31){ D[i+1,j+1,k+1] = 31}   
          else if (max_prob ==P_roll30){ D[i+1,j+1,k+1] = 30}       
          else if (max_prob ==P_roll29){ D[i+1,j+1,k+1] = 29}
          else if (max_prob ==P_roll28){ D[i+1,j+1,k+1] = 28}
          else if (max_prob ==P_roll27){ D[i+1,j+1,k+1] = 27}
          else if (max_prob ==P_roll26){ D[i+1,j+1,k+1] = 26}
          else if (max_prob ==P_roll25){ D[i+1,j+1,k+1] = 25}
          else if (max_prob ==P_roll24){ D[i+1,j+1,k+1] = 24}
          else if (max_prob ==P_roll23){ D[i+1,j+1,k+1] = 23}
          else if (max_prob ==P_roll22){ D[i+1,j+1,k+1] = 22}
          else if (max_prob ==P_roll21){ D[i+1,j+1,k+1] = 21} 
          else if (max_prob ==P_roll20){ D[i+1,j+1,k+1] = 20}
          else if (max_prob ==P_roll19){ D[i+1,j+1,k+1] = 19}
          else if (max_prob ==P_roll18){ D[i+1,j+1,k+1] = 18}
          else if (max_prob ==P_roll17){ D[i+1,j+1,k+1] = 17}
          else if (max_prob ==P_roll16){ D[i+1,j+1,k+1] = 16}
          else if (max_prob ==P_roll15){ D[i+1,j+1,k+1] = 15}
          else if (max_prob ==P_roll14){ D[i+1,j+1,k+1] = 14}
          else if (max_prob ==P_roll13){ D[i+1,j+1,k+1] = 13}
          else if (max_prob ==P_roll12){ D[i+1,j+1,k+1] = 12}
          else if (max_prob ==P_roll11){ D[i+1,j+1,k+1] = 11}
          else if (max_prob ==P_roll10){ D[i+1,j+1,k+1] = 10}
          else if (max_prob ==P_roll9){ D[i+1,j+1,k+1] = 9}
          else if (max_prob ==P_roll8){ D[i+1,j+1,k+1] = 8}
          else if (max_prob ==P_roll7){ D[i+1,j+1,k+1] = 7}
          else if (max_prob ==P_roll6){ D[i+1,j+1,k+1] = 6}
          else if (max_prob ==P_roll5){ D[i+1,j+1,k+1] = 5}
          else if (max_prob ==P_roll4){ D[i+1,j+1,k+1] = 4}
          else if (max_prob ==P_roll3){ D[i+1,j+1,k+1] = 3}
          else if (max_prob ==P_roll2){ D[i+1,j+1,k+1] = 2}
          else if (max_prob ==P_roll1){ D[i+1,j+1,k+1] = 1}
          else{D[i+1,j+1,k+1] = 0} #hodl
        }
      }
    }      
  }
}

save(list = c('P','D'),file = 'PDfile.Rdata') 