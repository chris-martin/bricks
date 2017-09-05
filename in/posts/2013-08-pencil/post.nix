{ file-path, file-string, html }:

{
  title = "Pencil effects";
  date  = "2013 Aug 03";
  slug  = "pencil-effects";

  thumbnail = file-path ./pencil.jpg;

  abstract = ''
    Side effects are great, but …
  '';

  body = html (file-string ./post.html);
}
