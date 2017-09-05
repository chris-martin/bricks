{ html, html-tags, markdown, file-string }:

let
  inherit (html-tags) p;
  link = "http://www.maa.org/external_archive/devlin/devlin_03_08.html";
  quote x = html-tags.blockquote (markdown (file-string x));

in {
  title = "A Mathematician’s Lament";
  date  = "2009 June 19";
  slug  = "a-mathematicians-lament";

  abstract = ''
    The big question everyone had in middle-to-high school mathematics
    classes was "When am I ever going to use this?"
  '';

  body = [

    (p (html ''
      Slashdot recently featured
      <a href="${link}">"A Mathematician’s Lament"</a>
      by Paul Lockhart.
    ''))

    (p ''
      The big question everyone had in middle-to-high school mathematics
      classes was "When am I ever going to use this?" Those few who had
      interest in the subject tried to justify the existence of math class.
      But we were completely wrong. You asked the right quesion, and the
      answer was: Never. Your time is being wasted, and you are being
      frustrated needlessly, because they are teaching all the wrong things.
    '')

    (p ''
      I do recommend everyone read this, although it is a bit long.
      Below are my favorite excerpts.
    '')

    hr

    (quote ./quote1.md)
    (quote ./quote2.md)
    (quote ./quote3.md)
    (quote ./quote4.md)
  ];
}
