import Data.Bits

generate_new_value f v = rem (f * v) 2147483647
gen_a = drop 1 $ filter (\x -> rem x 4 == 0) $ iterate (generate_new_value 16807) 873
gen_b = drop 1 $ filter (\x -> rem x 8 == 0) $ iterate (generate_new_value 48271) 583

compare_matches n = foldr f 0 $ zip (take n gen_a) (take n gen_b)
  where f :: (Integer,Integer) -> Integer -> Integer
        f (a,b) c = if (a .&. 0xFFFF) == (b .&. 0xFFFF) then c+1 else c
