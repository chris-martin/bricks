import ChrisMartinOrg.CommandLineOpts

main :: IO ()
main =
  do
    opts@(Opts verbosity command) <- getOpts
    case verbosity of
      VerbosityHigh -> print opts
      _ -> pure ()
