{ file-path, html, markdown }:

[

  (h1 "Chris Martin")

  (html ''<img src="${file-path ./photo.jpg}" class="photo">'')

  (markdown ''
    [Twitter](https://twitter.com/chris__martin) &middot;
    [GitHub](https://github.com/chris-martin) &middot;
    [Stack Overflow](http://stackoverflow.com/users/402884) &middot;
    [Facebook](https://www.facebook.com/ch.martin) &middot;
    [Email](mailto:ch.martin@gmail.com)
  '')

]
