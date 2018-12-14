l = [1..10]
filter3 x = x < 3
filter5 x = x < 5

mapped3 = [x | x<-l, filter5 x, filter3 x]