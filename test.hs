elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

l = [x | x<-[1..10], p x]

p x = x < 6

test = elem' 3 l
test2 = elem' 7 l