import Program

main :: IO ()
main = do
  let input = ["http://www.google.com/"]
  companyNames <- program input
  mapM_ putStrLn companyNames
