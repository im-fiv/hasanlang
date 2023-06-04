use crate::tokenizer::Rule;
use crate::compiler::Type;

use pest::iterators::{Pair, Pairs};
use std::iter::Peekable;

pub type NumberType = i32;

pub struct ASTParser<'p> {
	pairs: Pairs<'p, Rule>
}

#[derive(Debug)]
pub enum Statement {
	FunctionDefinition {
		name: String,
		arguments: Vec<FunctionArgument>,
		return_type: Expression, //* Expression::Type
		statements: Vec<Statement>
	},

	TypeDefinition,
	ClassDefinition,
	ClassDeclaration,
	VariableDefinition(String, Expression),
	FunctionCall(Box<Expression>, Vec<Expression>),
	Return(Box<Expression>),

	// special statements that are not intended to be used traditionally
	Unimplemented(Option<Rule>)
}

#[derive(Debug)]
pub struct FunctionArgument {
	name: Expression, //* Expression::Identifier
	type_expression: Expression //* Expression::Type
}

impl FunctionArgument {
	pub fn new(name: Expression, type_expression: Expression) -> FunctionArgument {
		match type_expression {
			Expression::Type { .. } => FunctionArgument { name, type_expression },
			_ => panic!("Failed to create a new FunctionArgument: Expression::Type expected in type_expression, got {:?}", type_expression)
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expression {
	Number(i32),
	String(String),

	Unary(Operator, Box<Expression>),
	Binary(Box<Expression>, Operator, Box<Expression>),

	FunctionCall(Box<Expression>, Vec<Expression>),
	ArrayAccess(Box<Expression>, Box<Expression>),
	DotAccess(Box<Expression>, Box<Expression>),
	ArrowAccess(Box<Expression>, Box<Expression>),
	Array(Vec<Box<Expression>>),
	Identifier(String),

	Type {
		base: ParserTypeUtility,
		generics: Box<Vec<Expression>>,
		array_type: bool
	},

	Empty,
	Unimplemented
}

// NOTE: this is just a utility type
#[derive(Debug, Clone)]
pub enum ParserTypeUtility {
	Expression(Box<Expression>)
}

impl ParserTypeUtility {
	pub fn void_type() -> ParserTypeUtility {
		ParserTypeUtility::from_parser_type(Type::Void)
	}

	pub fn from_parser_type(parser_type: Type) -> ParserTypeUtility {
		ParserTypeUtility::Expression(
			Box::new(
				Expression::Identifier(
					parser_type.as_str_parser().to_owned()
				)
			)
		)
	}

	pub fn from_string(name: String) -> ParserTypeUtility {
		ParserTypeUtility::Expression(
			Box::new(
				Expression::Identifier(name)
			)
		)
	}
}

#[derive(Debug, Clone)]
pub enum Operator {
	Plus,
	Minus,
	Divide,
	Times,
	Modulo
}

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
			"*" => Operator::Times,
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

	fn parse_number_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let string = pair.as_str().to_owned();
		let literal = string.parse::<NumberType>()
			.expect(format!("Failed to parse number \"{}\"", string).as_str());

		Expression::Number(literal)
	}

	fn parse_string_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let literal = pair.as_str().to_owned();
		let clean_literal = literal.trim_start_matches(&['\'', '\"'][..]).trim_end_matches(&['\'', '\"'][..]);

		Expression::String(clean_literal.to_owned())
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
				Rule::function_call_expression => self.parse_function_call_expression(pair.into_inner()),

				Rule::number_literal => self.parse_number_literal(pair),
				Rule::string_literal => self.parse_string_literal(pair),

				Rule::identifier => Expression::Identifier(pair.as_str().to_owned()),

				Rule::r#type => {
					println!("parse_term(pair) = {:?}", pair);
					self.parse_type(pair.into_inner())
				},

				rule => panic!("Got invalid expression rule: \"{:?}\"", rule),
			}
		} else {
			panic!("Unexpected end of input");
		}
	}

	fn parse_generics(&self, pair: Pair<'p, Rule>) -> Vec<Expression> {
		if pair.as_rule() != Rule::generics {
			panic!("Got an unexpected rule. Expected \"generics\", got \"{:?}\"", pair.as_rule());
		}

		let insides = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new(); //* Expression::Identifier only

		for arg in insides {
			if arg.as_rule() != Rule::identifier {
				panic!("Got an unexpected rule as a generics argument. Expected \"identifier\", got \"{:?}\"", arg.as_rule());
			}

			generics.push(Expression::Identifier(arg.as_str().to_owned()));
		}

		generics
	}

	fn parse_type(&self, pairs_borrowed: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs_borrowed.clone();
		let pair = pairs
			.next()
			.expect("Failed to parse type");

		if pair.as_rule() != Rule::r#type {
			panic!("Got an unexpected rule. Expected \"type\", got \"{:?}\"", pair.as_rule());
		}

		let mut insides = pair.into_inner();

		// check if the type is an array type
		let next_pair = insides.peek().expect("Failed to parse type (got an empty type)");
		let is_array_type = next_pair.as_rule() == Rule::array_type;

		if is_array_type {
			insides = next_pair.into_inner();
		}
		
		// get type identifier
		let name_pair = insides.next().expect("Failed to parse type name");
		let mut generics: Vec<Expression> = Vec::new();

		// check if there are generics present
		if insides.len() > 0 {
			let generics_pair = insides.next().expect("Failed to parse type generics");
			generics = self.parse_generics(generics_pair);
		}

		Expression::Type {
			base: ParserTypeUtility::from_string(name_pair.as_str().to_owned()),
			generics: Box::new(generics),
			array_type: is_array_type
		}
	}

	fn parse_function_call_expression(&self, pairs_borrowed: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs_borrowed.clone();
		
		// extract callee
		let callee_pair = pairs.next().expect("Identifier or expression expected in function call expression, got nothing");

		match callee_pair.as_rule() {
			Rule::number_literal | Rule::string_literal => panic!("Number and string literals are not valid function names"),
			_ => ()
		}

		let callee = self.parse_expression(pairs_borrowed);

		// extract arguments
		let args_option_pair = pairs.next();
		
		// if there's no arguments, return early
		if args_option_pair.is_none() {
			return Expression::FunctionCall(Box::new(callee), Vec::new())
		}

		let args_pair = args_option_pair.unwrap();
		let mut args: Vec<Expression> = Vec::new();

		for arg_pair in args_pair.into_inner() {
			// arg_pair is always wrapped in an expression in this case
			let parsed = self.parse_expression(arg_pair.into_inner());
			args.push(parsed);
		}

		// return
		Expression::FunctionCall(Box::new(callee), args)
	}

	fn parse_function_definition(&self, pairs_borrowed: Pairs<'p, Rule>) -> Statement {
		// TODO: implement parsing function generics

		let mut pairs = pairs_borrowed.clone();
		let mut header_pairs = pairs.next().expect("Failed to parse function header").into_inner();

		// parsing header
		let name = header_pairs.next().expect("Failed to parse function name");
		let mut args: Vec<FunctionArgument> = Vec::new();

		// check if arguments exist
		if header_pairs.len() > 0 {
			// must clone to prevent argument_pairs from eating the return type
			let argument_pairs = header_pairs
				.clone()
				.next()
				.expect("Failed to parse function arguments")
				.into_inner();

			let filtered_arguments: Vec<Pair<'p, Rule>> = argument_pairs
				.clone()
				.filter(|pair| pair.as_rule() == Rule::function_definition_argument)
				.collect();

			for arg_pair in filtered_arguments {
				// arg_pair is here expected to be function_definition_argument
				let mut arg = arg_pair.into_inner();

				let arg_name = self.parse_expression(arg.clone());
				arg.next().expect("Failed to parse function argument type");

				let arg_type = self.parse_type(arg);
				args.push(FunctionArgument::new(arg_name, arg_type));
			}
		}

		let body_pairs = pairs.next().expect("Failed to parse function body").into_inner();

		// now can safely skip function_definition_arguments node
		// check if arguments exist
		let return_type = if header_pairs.len() >= 1 {
			if header_pairs.len() > 1 {
				header_pairs.next();
			}

			self.parse_type(header_pairs)
		} else {
			Expression::Type {
				base: ParserTypeUtility::void_type(),
				generics: Box::new(Vec::new()),
				array_type: false
			}
		};

		Statement::FunctionDefinition {
			name: name.as_str().to_owned(),
			arguments: args,
			return_type,
			statements: self.parse_program(body_pairs)
		}
	}

	fn parse_type_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented(Some(Rule::type_definition_stmt))
	}

	fn parse_class_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented(Some(Rule::class_definition))
	}

	fn parse_class_declaration(&self, pairs: Pairs<'p, Rule>) -> Statement {
		Statement::Unimplemented(Some(Rule::class_declaration))
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
		let parsed = self.parse_expression(pairs);

		if let Expression::FunctionCall(callee, args) = parsed {
			return Statement::FunctionCall(callee, args);
		}

		panic!("Unable to parse function call statement: callee or arguments parameters are invalid");
	}

	fn parse_return(&self, pairs: Pairs<'p, Rule>) -> Statement {
		if pairs.len() > 0 {
			return Statement::Return(Box::new(self.parse_expression(pairs)));
		}

		Statement::Return(Box::new(Expression::Empty))
	}
}