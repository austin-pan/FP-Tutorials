data State = Ok | Nok deriving(Show)

atm :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> [Int] -> [((Int, Int, Int, Int, Int, Int, Int, Int, Int), Int, State)]
atm _ [] = []
atm (p1,p2,p5,p10,p20,p50,po1,po2,total) (x:xs)
    | po2<50 && x>=200 && (total+x)<=5000 = atm (p1,p2,p5,p10,p20,p50,po1,po2+1,total) ((x-200):xs)
    | po1<50 && x>=100 && (total+x)<=5000 = atm (p1,p2,p5,p10,p20,p50,po1+1,po2,total) ((x-100):xs)
    | p50<50 && x>=50 && (total+x)<=5000 = atm (p1,p2,p5,p10,p20,p50+1,po1,po2,total) ((x-50):xs)
    | p20<50 && x>=20 && (total+x)<=5000 = atm (p1,p2,p5,p10,p20+1,p50,po1,po2,total) ((x-20):xs)
    | p10<50 && x>=10 && (total+x)<=5000 = atm (p1,p2,p5,p10+1,p20,p50,po1,po2,total) ((x-10):xs)
    | p5<50 && x>=5 && (total+x)<=5000 = atm (p1,p2,p5+1,p10,p20,p50,po1,po2,total) ((x-5):xs)
    | p2<50 && x>=2 && (total+x)<=5000 = atm (p1,p2+1,p5,p10,p20,p50,po1,po2,total) ((x-2):xs)
    | p1<50 && x>=1 && (total+x)<=5000 = atm (p1+1,p2,p5,p10,p20,p50,po1,po2,total) ((x-1):xs)
    | x<=0 = ((p1,p2,p5,p10,p20,p50,po1,po2,total), 0, Ok) : atm (p1,p2,p5,p10,p20,p50,po1,po2,total) xs
    | otherwise = ((p1,p2,p5,p10,p20,p50,po1,po2,total), x, Nok) : atm (p1,p2,p5,p10,p20,p50,po1,po2,total) xs
        where total = p1 + 2*p2 + 5*p5 + 10*p10 + 20*p20 + 50*p50 + 100*po1 + 200*po2