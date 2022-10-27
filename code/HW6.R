# ZOO 955 HW 6 Hart, Adler, Butts: Individual Based Modeling

require(dplyr)


zoop = 200
macinv = 200
yp = 1

df<- data.frame(matrix(ncol=4, nrow=101))
colnames(df)<- c('day', 'zoop','macinv', 'biomass')

df$day = seq(0,100)
df$zoop[1] = zoop
df$macinv[1] = macinv
df$biomass = 0

# outer for loop

current_pop = c(rep(colnames(df[2]), times = df[1,2]),
                rep(colnames(df[3]), times = df[1,3]))

for (i in 1: nrow(df)){
  bm = 0
  
  # inner for loop
  
  for (j in 1:50){
    
    food = sample(current_pop, size = 1)
    
    current_pop = current_pop[-match(food, current_pop)]
    
    bm = bm + ifelse(food == 'macinv', 10, 1)
    
    if  (bm >= 50 ) break
    
  }
  
  # update current population to data frame
  
  df$biomass[i] = bm
  
  df$zoop[i] = length(current_pop[current_pop == 'zoop'])
  df$macinv[i] = length(current_pop[current_pop == 'macinv'])
  
  
  

}


