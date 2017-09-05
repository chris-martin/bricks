{ file-path, file-string, html }:

{
  title = "Dynamic typing";
  date  = "2015 Aug 9";
  slug  = "dynamic-typing";

  thumbnail = file-path ./duck.jpg;

  abstract = ''
    "Dynamic typing" The belief that you can’t explain to a computer
    why your code works, but you can keep track of it all in your head.
  '';

  body = html (file-string ./tweet.html);
}
