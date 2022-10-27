# ZOO 955 HW 6 Hart, Adler, Butts: Individual Based Modeling

require(dplyr); require(ggplot2)

zoop = 500
macinv = 500
yp = 1

df<- data.frame(matrix(ncol=6, nrow=101))
colnames(df)<- c('day', 'zoop','macinv', 'biomass', 'mac_eat', 'zoop_eat')

df$day = seq(0,100)
df$zoop[1] = zoop
df$macinv[1] = macinv
df$biomass = 0
df$mac_eat = 0
df$zoop_eat = 0

# outer for loop

current_pop = c(rep(colnames(df[2]), times = df[1,2]),
                rep(colnames(df[3]), times = df[1,3]))

for (i in 2: nrow(df)){
  bm = 0
  
  # inner for loop
  
  for (j in 1:50){
    
    food = sample(current_pop, size = 1)
    
    if (food == 'macinv'){
      df$mac_eat[i] = df$mac_eat[i] + 1
    } else {
      df$zoop_eat[i] = df$zoop_eat[i] + 1
    }
    
    current_pop = current_pop[-match(food, current_pop)]
    
    bm = bm + ifelse(food == 'macinv', 10, 1)
    
    if  (bm >= 50 ) break
    
  }
  
  # update current population to data frame
  
  df$biomass[i] = bm
  
  new_zoop = round(length(current_pop[current_pop == 'zoop'])*1.01)
  new_macinv = round(length(current_pop[current_pop == 'macinv'])*1.01)
  
  current_pop = c(rep('zoop', times = new_zoop),
                  rep('macinv', times = new_macinv))
  
  df$zoop[i] = length(current_pop[current_pop == 'zoop'])
  df$macinv[i] = length(current_pop[current_pop == 'macinv'])
  
  
}

ggplot(df, aes(day, macinv))+
  geom_point()
