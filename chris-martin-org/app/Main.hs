import ChrisMartinOrg.CommandLineOpts

main :: IO ()
main =
  getOpts >>= print
