# ZOO 955 HW 6 Hart, Adler, Butts: Individual Based Modeling

require(dplyr); require(ggplot2)

# initial populations
zoop = 100000
macinv = 700
yp = 1

# blank data frame
df<- data.frame(matrix(ncol=6, nrow=101))
colnames(df)<- c('day', 'zoop','macinv', 'biomass', 'mac_eat', 'zoop_eat')

# populating values in data frame
df$day = seq(0,100)
df$zoop[1] = zoop
df$macinv[1] = macinv
df$biomass = 0
df$mac_eat = 0
df$zoop_eat = 0

# setting the start population that will be updated through each iteration
current_pop = c(rep(colnames(df[2]), times = df[1,2]),
                rep(colnames(df[3]), times = df[1,3]))

for (i in 2: nrow(df)){
  # daily biomass consumption counter is reset each loop
  bm = 0
  
  # inner for loop
  
  for (j in 1:200){
    
    # sampling one individual from the populations of zoops & macinvs
    food = sample(current_pop, size = 1)
    
    # adding counts of what was eaten each day to the data frame
    if (food == 'macinv'){
      df$mac_eat[i] = df$mac_eat[i] + 1
    } else {
      df$zoop_eat[i] = df$zoop_eat[i] + 1
    }
    
    # removing the appropriate individual from the current population
    current_pop = current_pop[-match(food, current_pop)]
    
    # adding to the biomass counter with the assumption that macinvs are larger than zoops and will satiate the yellow perch quicker
    bm = bm + ifelse(food == 'macinv', 20, 1)
    
    # when yellow perch consume X bm, they are done for the day and the inner loop ends
    if  (bm >= 300 ) break
    
  }
  
  # update final metrics to data frame
  
  # total biomass consumed on that day
  df$biomass[i] = bm
  
  # accounting for population growth
  new_zoop = ceiling(length(current_pop[current_pop == 'zoop'])*1.001)
  new_macinv = ceiling(length(current_pop[current_pop == 'macinv'])*1.0001)
  
  # new current population after predation and growth
  current_pop = c(rep('zoop', times = new_zoop),
                  rep('macinv', times = new_macinv))
  
  # tracking populations in the data frame
  df$zoop[i] = length(current_pop[current_pop == 'zoop'])
  df$macinv[i] = length(current_pop[current_pop == 'macinv'])
  
  
}

ggplot(df, aes(day, zoop))+
  geom_point()

ggplot(df, aes(day, macinv))+
  geom_point()
