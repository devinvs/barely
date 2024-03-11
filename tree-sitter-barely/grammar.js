module.exports = grammar({
  name: 'barely',
  extras: $ => [
    /\s|\\\r?\n/,
    $.comment
  ],
  rules: {
    source_file: $ => repeat($.item),
    comment: $ => seq('//', /(\\+(.|\r?\n)|[^\\\n])*/),

    item : $ => choice(
      $.import,
      $.macro,
      $.stmt,
    ),

    import: $ => seq("import", $.ident),

    macro: $ => seq(
      "macro",
      $.ident,
      '(',
      optional(csv($.ident)),
      ')',
      '=',
      $.expr
    ),

    stmt: $ => choice(
      seq('let', $.ident, '=', $.expr),
      $.expr
    ),

    expr: $ => choice(
      prec.left(1, seq($.expr, "||", $.expr)),
      prec.left(2, seq($.expr, "&&", $.expr)),
      prec.left(3, seq($.expr, "|", $.expr)),
      prec.left(4, seq($.expr, "^", $.expr)),
      prec.left(5, seq($.expr, "&", $.expr)),
      prec.left(6, seq($.expr, "==", $.expr)),
      prec.left(6, seq($.expr, "!=", $.expr)),
      prec.left(7, seq($.expr, "<", $.expr)),
      prec.left(7, seq($.expr, ">", $.expr)),
      prec.left(7, seq($.expr, "<=", $.expr)),
      prec.left(7, seq($.expr, ">=", $.expr)),
      prec.left(8, seq($.expr, "::", $.expr)),
      prec.left(9, seq($.expr, "+", $.expr)),
      prec.left(9, seq($.expr, "-", $.expr)),
      prec.left(10, seq($.expr, "*", $.expr)),
      prec.left(10, seq($.expr, "/", $.expr)),
      prec.left(10, seq($.expr, "%", $.expr)),
      prec.left(11, seq($.expr, "**", $.expr)),
      prec.left(12, seq("!", $.expr)),
      prec.left(12, seq("~", $.expr)),
      prec.left(12, seq("-", $.expr)),
      $.call_expr,

      $.ident,
      $.num,
      $.str
    ),

    call_expr: $ => prec.right(13, seq(field("func", $.expr), "(", optional(csv($.expr)), ")")),
    ident: $ => /[a-zA-Z][a-zA-Z0-9_']*/,
    num: $ => /[0-9]+/,
    str: $ => /"[^"\n\r]*"/,
  }
})

function csv(expr) {
  return seq(expr, repeat(seq(",", expr)), optional(","));
}
