[
  "import"
  "let"
  "macro"
] @keyword

[
  "="
  "<"
  ">"
  "+"
  "-"
  "*"
  "/"
  "%"
  "<="
  ">="
  "=="
  "!="
  "|"
  "&"
  "^"
  "~"
] @operator

(num) @constant
(str) @string


(comment) @comment
(call_expr func: (expr (ident) @function))

