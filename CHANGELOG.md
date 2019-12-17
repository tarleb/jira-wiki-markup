# Changelog

`jira-wiki-markup` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

1.0.0
=====

Released 2019-12-17.

* Add `Doc` datatype representing a full document; `parse` now returns
  this type.
* Improve parsing:
  - double-backslash is recognized as linebreak;
  - emoticons are parsed as `Emoji`;
  - special sequences of dashes are translated into their unicode
    representation;
  - naked URLs are parsed as `AutoLink`;
  - blocks of colored text are parsed as `Color`;
  - interpretation of special characters as markup can be forced by
    surrounding them with curly braces.
* A parser `plainText` was added available to read markup-less text.
* *Inline*-parser `symbol` was renamed to `specialChar`.
* Add printer module to render the document AST as Jira markup.
* Markup datatype changes:
  - new *Block* elements `Color` and `HorizontalRule`.
  - new *Inline* elements `Emoji`, and `Styled`.
  - *Inline* constructors `Subscript`, `Superscript`, `Emph`, `Strong`,
    `Inserted`, and `Deleted` have been remove. Use `Styled` instead.
  - Constructor `Image` now takes a list of parameters as an additional
    argument.
* CI runs also test GHC 8.8.

0.1.1
=====

* Ensure proper parsing of backslash-escaped characters.

0.1.0
=====

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/tarleb/jira-wiki-markup/releases
