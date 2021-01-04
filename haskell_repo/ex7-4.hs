import System.Environment

main = do args <- getArgs
          let year = read $ args !! 0
          let month = read $ args !! 1
          let day = read $ args !! 2
          putStrLn $ yearStr year month (day + 10)
          putStrLn $ yearStr year month (day + 100)
          putStrLn $ yearStr year month (day + 1000)
          putStrLn $ yearStr year month (day + 10000)

yearStr:: Int->Int->Int->String
yearStr year month day = do let pre_year = year
                            let now_year = year + (dnyear year day 0)
                            let dif_day = difdays pre_year now_year day
                            let now_month = (getmonth now_year month dif_day month) `mod` 12
                            let now_days =  getdays now_year month dif_day
                            show (now_year) ++ "/" ++ show (now_month) ++ "/" ++ show (now_days)

leap::Int->Bool
leap y | y `mod` 400 == 0 = True
       | y `mod` 100 == 0 = False
       | y `mod` 4   == 0 = True
       | otherwise = False

monthDay::Int -> Int -> Int
monthDay _ 1 = 31
monthDay y 2 = if leap y then 29 else 28
monthDay _ 3 = 31
monthDay _ 4 = 30
monthDay _ 5 = 31
monthDay _ 6 = 30
monthDay _ 7 = 31
monthDay _ 8 = 31
monthDay _ 9 = 30
monthDay _ 10 = 31
monthDay _ 11 = 30
monthDay _ 12 = 31

yearDay year = if leap year then 366 else 365


dnyear year days n = if days < 0 then n - 1
                                 else if days == 0 then n
                                                   else dnyear (year + 1) (days - (yearDay year)) (n + 1)

difdays pre_year now_year days = days - sum[yearDay y | y <- [pre_year..(now_year-1)]]

getmonth year pre_month days n =  if days < 0  then  n - 1
                                               else if days == 0 then n
                                                                 else getmonth year pre_month (days - (monthDay year pre_month)) n+1

getdays now_year month dif_day = if dif_day <= (monthDay now_year month) then dif_day
                                                                  else getdays now_year (month + 1) (dif_day - (monthDay now_year month))

