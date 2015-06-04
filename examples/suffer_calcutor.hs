-- | Calculates Suffer Score on Recent Activity
-- Be sure to set your token. This code isn't 
-- exactly type safe. 

{-# LANGUAGE OverloadedStrings #-}
import Strive hiding (map, points)

import Data.Text (unpack)

token = "Fill me in"

type Zone = (Int, (Int, Int))

-- Our Heart Rate Zones
zones :: [Zone]
zones = [(1, (0,126)), (2, (127,157)), (3,(158,189)), (4, (189,1000))]

main = do
  client <- buildClient (Just token)
  (Right acts) <- getCurrentActivities client $ with [] 
  let hr_acts = filter hasHeartRate acts
  let hr_acts_id    = map activitySummary_id hr_acts
  let hr_acts_title = map activitySummary_name hr_acts
  let hr_acts_date  = map activitySummary_startDate hr_acts
  hr_act_suffer <- mapM (calc_suffer_for_activity client) hr_acts_id
  let table = zip3 hr_acts_title hr_acts_date hr_act_suffer
  mapM_ (\(title, day, suffer) -> putStrLn $ 
    (show suffer) ++ "\t" ++ (show day) ++ "\t" ++ (unpack title))
    table

hasHeartRate :: ActivitySummary -> Bool
hasHeartRate activity = (activitySummary_averageHeartrate activity) /= Nothing 


calc_suffer_for_activity :: Client -> ActivityId -> IO Int
calc_suffer_for_activity client act_id = do
  (Right act_stream) <- getActivityStreams client act_id
    [ HeartrateStream, MovingStream ] $ with [ set seriesType Time ]
  let (Just hr)     = heartrateStream act_stream
  let (Just time)   = timeStream act_stream
  let (Just moving) = movingStream act_stream
  return $ calc_suffer hr time moving


-- Drop first point (usually in your privacy zone)
-- Calculate Elapsed Time
-- Filter rows where moving is False
calc_suffer :: [Int] -> [Int] -> [Bool] -> Int
calc_suffer hr time moving = 
  let zipped = zip3 (elapsed time) (tail hr) (tail moving)
      filter_by_moving = filter (\(_,_,is_moving) -> is_moving) zipped
      heart_rates = map (\(a,b,_) -> (a,b)) filter_by_moving
  in points zones heart_rates

elapsed :: [Int] -> [Int]
elapsed (x:y:xs) = (y - x) : elapsed (y:xs)
elapsed (x:[])   = []

type HeartRate = (Int, Int)
type HeartRates = [HeartRate] 


in_zone :: Zone -> HeartRate -> Int 
in_zone (pts, (low, high)) (elapse, hr) = 
  if(low <= hr && hr <= high) then elapse else 0

-- time is in seconds
time_in_zone :: Zone -> HeartRates -> Int
time_in_zone zone = sum . (map (in_zone zone))

time_in_zones :: [Zone] -> HeartRates -> [Int]
time_in_zones zones hr 
  = map (`time_in_zone` hr) zones

points_in_zone :: Zone -> HeartRates -> Int
points_in_zone z@(points, _) hr = ((time_in_zone z hr) * points) `div` 60

points_in_zones :: [Zone] -> HeartRates -> [Int]
points_in_zones zones hr = 
  map (`points_in_zone` hr) zones

points :: [Zone] -> HeartRates -> Int
points zone hr = sum $ points_in_zones zone hr
