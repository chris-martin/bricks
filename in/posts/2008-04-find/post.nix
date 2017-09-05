{ file-path, file-string, html-tags, markdown, code }:

let
  p = x: html-tags.p (markdown x);

in {
  title = "Fun with find";
  date  = "2008 Apr 24";
  slug  = "fun-with-find";

  thumbnail = file-path ./thumbnail.png;

  abstract = ''
    `find` is one of many simple yet powerful command-line tools, and I haven’t
    until recently had a chance to learn how to use it. Oh, the things you can
    do.
  '';

  body = [

    (p ''
      `find` is one of many simple yet powerful command-line tools, and I
      haven’t until recently had a chance to learn how to use it. Oh, the
      things you can do. Here’s a fun example Matt Luongo and I worked out
      a few days ago, plus a bit I added on just now:
    '')

    (code { language = "bash"; } (file-string ./command.sh))

    (p ''
      `find` recursively enumerates all of the files within the current
      directory which match the name *.java.
    '')

    (p ''
      The result of that gets piped to `xargs`, which takes each line (a file
      path) and sends them as arguments to `wc` (word count). The `-l` flag
      tells `wc` to count the number of lines in each file, and also give a
      total count.
    '')

    (p ''
      This is a pretty big list of files, and the most interesting results are
      the largest ones. So, the next pipe goes to `sort`. `-n` tells it sort by
      number, not alphabetically, and `-r` sorts in reverse other to put the
      biggest numbers on top.
    '')

    (p ''
      It’s sorted, but I still only care about the first few, so I pipe that
      into `head`, whose `-n` parameter lets me specify that I only want the
      first 5 lines.
    '')

    (p ''
      Finally, the line length of the output is pretty long, because it’s
      showing the entire relative path to the files, which are somewhat
      deeply nested. I only want to see the filenames. Fortunately, this is
      nothing that a simple regex can’t handle, and `sed` is up to the task
      with a search-and-replace which yields exactly what I’m looking for.
    '')

    (p ''
      I ran this from the SVN directory for my software engineering project
      to see how much code we’d produced. The result:
    '')

    (code {} (file-string ./result.txt))

    (p "Fascinating.")

    (p ''
      Instead of writing this, I really should have spent the last half hour
      sleeping. I do only have one more day to debug 18208 lines of Java for
      our final demo.
    '')

  ];
}
