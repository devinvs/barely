use barely::compiler::compile;
use barely::lexer::Lexer;
use barely::parser;
use barely::ssa::prog_to_ssa;

use std::collections::HashMap;
use std::env::args;
use std::error::Error;
use std::io::Write;

fn main() {
    let mut args = args();
    args.next();

    let path = args.next().unwrap();

    let s = std::fs::read_to_string(path).unwrap();

    let mut l = Lexer::default();
    let mut ts = l.lex(&s).unwrap();
    let mut lex_f = std::fs::File::create("./build/out.lex").unwrap();
    writeln!(lex_f, "{:#?}", ts).unwrap();

    let prog = parser::program(&mut ts).unwrap();
    let mut parse_f = std::fs::File::create("./build/out.parse").unwrap();
    writeln!(parse_f, "{:#?}", prog).unwrap();

    let ssa = prog_to_ssa(prog);

    let mut ssa_f = std::fs::File::create("./build/out.ssa").unwrap();
    writeln!(ssa_f, "{:#?}", ssa).unwrap();

    let out = std::fs::File::create("./build/out.asm").unwrap();
    let text = compile(ssa.body);

    write(Box::new(out), text, ssa.strings, ssa.imports).unwrap();
}

fn write(
    mut w: Box<dyn Write>,
    text: Vec<String>,
    strings: HashMap<String, u64>,
    imports: Vec<String>,
) -> Result<(), Box<dyn Error>> {
    writeln!(w, "default rel")?;
    writeln!(w, "bits 64\n")?;

    if imports.len() > 0 {
        for i in imports {
            writeln!(w, "extern {}", i)?;
        }
        writeln!(w)?;
    }

    writeln!(w, "section .text")?;
    writeln!(w, "global _start")?;
    writeln!(w, "_start:")?;

    for line in text {
        writeln!(w, "\t{}", line)?;
    }

    for (s, id) in strings.into_iter() {
        writeln!(w, "_s{id}: db `{s}`")?;
    }

    Ok(())
}
