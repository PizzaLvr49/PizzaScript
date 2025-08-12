use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{alpha1, char, multispace0, multispace1},
    combinator::{map, opt},
    multi::many0,
    sequence::delimited,
};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Type {
    Number,
    String,
    Bool,
    Unknown,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(String),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub mutable: bool,
    pub var_type: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let { var: Variable, value: Expr },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn parse_identifier(input: &str) -> IResult<&str, String> {
    let (input, first) = alt((alpha1, tag("_")))(input)?;
    let (input, rest) = take_while(is_ident_char)(input)?;
    Ok((input, format!("{}{}", first, rest)))
}

fn parse_mut_keyword(input: &str) -> IResult<&str, bool> {
    let (input, _) = multispace0(input)?;
    let (input, is_mut) = opt(tag("mut"))(input)?;
    Ok((input, is_mut.is_some()))
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    let (input, _) = multispace0(input)?;
    let (input, ty_str) = alt((tag("number"), tag("string"), tag("bool")))(input)?;
    let ty = match ty_str {
        "number" => Type::Number,
        "string" => Type::String,
        "bool" => Type::Bool,
        _ => Type::Unknown,
    };
    Ok((input, ty))
}

fn parse_number_literal(input: &str) -> IResult<&str, Literal> {
    let (input, num_str) = take_while1(|c: char| c.is_digit(10))(input)?;
    let number = num_str.parse::<i64>().unwrap();
    Ok((input, Literal::Number(number)))
}

fn parse_string_literal(input: &str) -> IResult<&str, Literal> {
    let (input, content) = delimited(char('"'), take_while(|c| c != '"'), char('"'))(input)?;
    Ok((input, Literal::String(content.to_string())))
}

fn parse_bool_literal(input: &str) -> IResult<&str, Literal> {
    let (input, val) = alt((tag("true"), tag("false")))(input)?;
    Ok((input, Literal::Bool(val == "true")))
}

fn parse_literal(input: &str) -> IResult<&str, Literal> {
    alt((
        parse_number_literal,
        parse_string_literal,
        parse_bool_literal,
    ))(input)
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    alt((
        map(parse_literal, Expr::Literal),
        map(parse_identifier, Expr::Var),
    ))(input)
}

fn parse_let_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("let")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, mutable) = parse_mut_keyword(input)?;
    let (input, _) = multispace0(input)?;
    let (input, var_name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, var_type) = parse_type(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, value) = parse_expr(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((
        input,
        Statement::Let {
            var: Variable {
                name: var_name,
                mutable,
                var_type,
            },
            value,
        },
    ))
}

fn parse_expr_statement(input: &str) -> IResult<&str, Statement> {
    let (input, expr) = parse_expr(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(";")(input)?;
    Ok((input, Statement::Expr(expr)))
}

fn parse_statement(input: &str) -> IResult<&str, Statement> {
    alt((parse_let_statement, parse_expr_statement))(input)
}

fn parse_program(input: &str) -> IResult<&str, Program> {
    let (input, statements) = many0(parse_statement)(input)?;
    Ok((input, Program { statements }))
}

fn main() {
    let source = r#"
        let mut counter: number = 5;
        let message: string = "Hello, world!";
        let done: bool = false;
    "#;

    match parse_program(source) {
        Ok((_rem, program)) => {
            println!("Parsed program:");
            for stmt in program.statements {
                match stmt {
                    Statement::Let { var, value } => {
                        println!(
                            "Let {}{}: {} = {:?}",
                            if var.mutable { "mut " } else { "" },
                            var.name,
                            var.var_type,
                            value
                        );
                    }
                    Statement::Expr(expr) => {
                        println!("Expr: {:?}", expr);
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
        }
    }
}
