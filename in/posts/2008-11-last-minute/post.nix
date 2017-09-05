{ html-tags }:

let
  inherit (html-tags) p;

in {
  title = "Last-minute math";
  date  = "2008 Nov 3";
  slug  = "last-minute-math";

  abstract = ''
    As I was leaving for AVS this morning, I happened upon a take-home
    test for Graph Theory which I should have done this weekend …
  '';

  body = [

    (p ''
      As I was leaving for AVS this morning, I happened upon a take-home
      test for Graph Theory which I should have done this weekend, and was
      now due in roughly 2 hours. I froze, panicked for about 5 seconds,
      then sat down to do some math. About an hour later, I’m done. And my
      proof is decently eloquent. Victory.
    '')

    (p "I think people complain too much about classes being hard.")

  ];
}
