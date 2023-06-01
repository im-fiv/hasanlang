use pest::iterators::{Pair, Pairs};
use crate::tokenizer::Rule;

use std::iter::Peekable;

pub struct ASTParser<'p> {
	pairs: Pairs<'p, Rule>
}

#[allow(dead_code)] // ! MUST REMOVE LATER
#[derive(Debug)]
pub enum Statement {
	FunctionDefinition,
	TypeDefinition,
	ClassDefinition,
	ClassDeclaration,
	VariableDefinition(String, Expression),
	FunctionCall(String, Vec<Expression>),
	Return,

	// special statements that are not intended to be used traditionally
	Unimplemented
}

#[allow(dead_code)] // ! MUST REMOVE LATER
#[derive(Debug)]
pub enum Expression {
	Number(i32),
	String(String),

	Unary(Operator, Box<Expression>),
	Binary(Box<Expression>, Operator, Box<Expression>),

	FunctionCall(Box<Expression>, Vec<Expression>),
	ArrayAccess(Box<Expression>, Box<Expression>),
	DotAccess(Box<Expression>, Box<Expression>),
	ArrowAccess(Box<Expression>, Box<Expression>),
	Array,
	Identifier(String),

	Unimplemented
}

#[allow(dead_code)] // ! MUST REMOVE LATER
#[derive(Debug)]
pub enum Operator {
	Plus,
	Minus,
	Divide,
	Multiply,
	Modulo
}

#[allow(unused_variables)] // ! MUST REMOVE LATER
impl<'p> ASTParser<'p> {
	pub fn new(pairs: Pairs<'p, Rule>) -> ASTParser<'p> {
		ASTParser { pairs }
	}

	pub fn parse(&self) -> Vec<Statement> {
		let mut statements: Option<Vec<Statement>> = None;

		for pair in self.pairs.clone() {
			match pair.as_rule() {
				Rule::COMMENT |
				Rule::WHITESPACE |
				Rule::line_comment |
				Rule::block_comment |
				Rule::number_literal |
				Rule::string_literal |
				Rule::EXPRESSION_access |
				Rule::EXPRESSION_array_access |
				Rule::EXPRESSION_arrow_access |
				Rule::EXPRESSION_dot_access |
				Rule::EXPRESSION_function_call => (),

				Rule::program => statements = Some(self.parse_program(pair.into_inner())),

				rule => panic!("Expected \"{:?}\" as first rule, got \"{:?}\"", Rule::program, rule)
			}
		}

		statements.expect("No statements have been parsed")
	}

	fn parse_program(&self, pairs: Pairs<'p, Rule>) -> Vec<Statement> {
		let mut statements: Vec<Statement> = Vec::new();

		for pair in pairs {
			if pair.as_rule() == Rule::EOI {
				continue;
			}

			let statement = match pair.as_rule() {
				Rule::function_definition_stmt => self.parse_function_definition(pair.into_inner()),
				Rule::type_definition_stmt => self.parse_type_definition(pair.into_inner()),
				Rule::class_definition => self.parse_class_definition(pair.into_inner()),
				Rule::class_declaration => self.parse_class_declaration(pair.into_inner()),
				Rule::variable_definition_stmt => self.parse_variable_definition(pair.into_inner()),
				Rule::function_call_stmt => self.parse_function_call(pair.into_inner()),
				Rule::return_stmt => self.parse_return(pair.into_inner()),

				rule => panic!("Got invalid statement rule: \"{:?}\"", rule)
			};

			statements.push(statement);
		}

		statements
	}

	fn parse_operator(&self, pair: &Pair<'p, Rule>) -> Operator {
		match pair.as_str() {
			"+" => Operator::Plus,
			"-" => Operator::Minus,
			"/" => Operator::Divide,
			"*" => Operator::Multiply,
			"%" => Operator::Modulo,

			string => panic!("Unexpected operator \"{}\"", string)
		}
	}

	fn parse_expression(&self, pairs: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs.peekable();
		self.parse_expression_with_precedence(&mut pairs, 0)
	}

	fn parse_expression_with_precedence(&self, pairs: &mut Peekable<Pairs<'p, Rule>>, precedence: u8) -> Expression {
		let mut left = self.parse_term(pairs);
	
		while let Some(pair) = pairs.peek() {
			if pair.as_rule() == Rule::operator {
				let operator_precedence = self.get_operator_precedence(pair);
				
				if operator_precedence < precedence {
					break;
				}
	
				let operator = self.parse_operator(pair);
				pairs.next(); // Consume the operator
				
				let right = self.parse_expression_with_precedence(pairs, operator_precedence + 1);
	
				left = Expression::Binary(
					Box::new(left),
					operator,
					Box::new(right)
				);
			} else {
				break;
			}
		}
	
		left
	}

	fn get_operator_precedence(&self, pair: &Pair<'p, Rule>) -> u8 {
		match pair.as_str() {
			"+" | "-" => 1,
			"*" | "/" | "%" => 2,
			
			_ => panic!("Unexpected operator \"{}\"", pair.as_str()),
		}
	}

	fn parse_term(&self, pairs: &mut Peekable<Pairs<'p, Rule>>) -> Expression {
		if let Some(pair) = pairs.next() {
			match pair.as_rule() {
				Rule::unary_expression => {
					let operator = self.parse_operator(&pair);
					let operand = self.parse_term(pairs);

					Expression::Unary(operator, Box::new(operand))
				}

				Rule::binary_expression | Rule::expression => self.parse_expression(pair.into_inner()),

				Rule::number_literal => {
					let string = pair.as_str().to_owned();
					let literal = string.parse::<i32>()
						.expect(format!("Failed to parse number \"{}\"", string).as_str());
	
					Expression::Number(literal)
				},

				Rule::string_literal => {
					let literal = pair.as_str().to_owned();
					Expression::String(literal)
				},

				rule => panic!("Got invalid expression rule: \"{:?}\"", rule),
			}
		} else {
			panic!("Unexpected end of input");
		}
	}

	fn parse_function_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented
	}

	fn parse_type_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented
	}

	fn parse_class_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented
	}

	fn parse_class_declaration(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented
	}

	fn parse_variable_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut name: Option<String> = None;
		let mut expression: Option<Expression> = None;

		for pair in pairs {
			match pair.as_rule() {
				Rule::identifier => name = Some(pair.as_str().to_owned()),
				Rule::expression => expression = Some(self.parse_expression(pair.into_inner())),

				rule => unreachable!("Got unexpected rule \"{:?}\" in variable definition", rule)
			}
		}

		Statement::VariableDefinition(
			name.expect("Identifier has not been provided in variable definition"),
			expression.expect("Expression has not been provided in variable definition")
		)
	}

	fn parse_function_call(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented
	}

	fn parse_return(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented
	}
}