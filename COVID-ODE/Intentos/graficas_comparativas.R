getwd()
dir()
data1<- read.csv("sim_Lety.csv"  )
data2<- read.csv("siim_foo.csv"  )

names(data1)
plot(data1$s1, col='red', type='l')
lines(data2$s_Sistema1, type = 'l', col='green')

plot(data$s_Sistema2, col='red', type='l')
lines(data$s2, type = 'l', col='green')

plot(data$s_Sistema32, col='red', type='l')
lines(data$s32, type = 'l', col='green')


plot(data$e1, col='red', type='l')
lines(data$e_Sistema1, type = 'l', col='green')


z <- X_theta_4(theta )
