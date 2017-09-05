{ code, file-string, scss }:

let
  scala = code { language = "scala"; };

in {
  title = "Correctness.scala";
  date  = "2013 Oct 22";
  slug  = "correctness-scala";

  css = scss ./correctness.scss;

  abstract = scala ''
    // Let’s talk about programs.
    type P
  '';

  body = scala (file-string ./correctness.scala);
}
