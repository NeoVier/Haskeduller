import Data.Time.Calendar

day :: Day
day = fromGregorian 2019 10 29

task :: Task
task = Task "Name" day "Description"

main :: IO ()
main = putStrLn "Test suite not yet implemented"
