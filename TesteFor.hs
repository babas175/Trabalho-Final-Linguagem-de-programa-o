somaAte :: Int -> Int
somaAte n = somaAte' n 0

somaAte' :: Int -> Int -> Int
somaAte' 0 acc = acc
somaAte' n acc = somaAte' (n - 1) (acc + n)
