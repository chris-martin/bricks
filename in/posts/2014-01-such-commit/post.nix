{ file-path, file-string, code }:

{
  title = "such commit—wow";
  date  = "2014 Jan 8";
  slug  = "such-commit-wow";

  thumbnail = file-path ./doge.png;

  abstract = code { language = "bash"; } ''
    alias such=git
    alias very=git
    alias wow='git status'
  '';

  body = html (file-string ./tweet.html);
}
