# ZOO 955 HW 6 Hart, Adler, Butts: Individual Based Modeling

require(dplyr)


zoop = 200
macinv = 200
yp = 1

df<- data.frame(matrix(ncol=4, nrow=101))
colnames(df)<- c('day', 'zoop','macinv', 'biomass')

df$day[1] = 1
df$zoop[1] = zoop
df$macinv[1] = macinv
df$biomass = 0

# outer for loop
current_pop = c(rep(colnames(df[2]), times = df[1,2]),
                rep(colnames(df[3]), times = df[1,3]))

biomass = 0

# inner for loop
food = sample(current_pop, size = 1)
current_pop = ifelse(food == 'zoop', current_pop[current_pop == 'zoop'][- (1:1)],
                     current_pop[current_pop == 'macinv'][- (1:1)] )

bm = ifelse(food == 'macinv', 10, 1)

df$biomass[1] = df$biomass[1] + bm

if  (df$biomass[1] => 50 ) break

# update current population to data frame

