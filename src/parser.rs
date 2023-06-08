use std::iter::Peekable;

use crate::tokenizer::Rule;
use crate::compiler::Type;

use pest::iterators::{Pair, Pairs};

pub type NumberType = i32;

pub struct ASTParser<'p> {
	pairs: Pairs<'p, Rule>
}

#[derive(Debug)]
pub enum Statement {
	FunctionDefinition {
		name: String,
		generics: Vec<Expression>,
		arguments: Vec<FunctionArgument>,
		return_type: Expression, //* Expression::Type
		statements: Vec<Statement>
	},

	TypeDefinition,

	ClassDefinition {
		name: String,
		generics: Vec<Expression>,
		members: Vec<ClassDefinitionMember>
	},

	ClassDeclaration,

	VariableDefinition {
		name: String,
		kind: Expression, //* Experssion::Type
		value: Expression
	},

	FunctionCall {
		callee: Expression,
		generics: Vec<Expression>,
		arguments: Vec<Expression>
	},

	Return(Expression),

	// special statements that are not intended to be used traditionally
	Unimplemented(Option<Rule>)
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct FunctionArgument {
	name: Expression, //* Expression::Identifier
	kind: Expression //* Expression::Type
}

#[derive(Debug)]
pub enum ClassDefinitionMember {
	Variable {
		name: String,
		kind: Expression, //* Expression::Type
		default_value: Expression
	},

	Function {
		name: String,
		generics: Vec<Expression>,
		arguments: Vec<FunctionArgument>,
		return_type: Expression, //* Expression::Type
		statement: Vec<Statement>
	}
}

impl FunctionArgument {
	pub fn new(name: Expression, kind: Expression) -> FunctionArgument {
		match kind {
			Expression::Type { .. } => FunctionArgument { name, kind },
			_ => panic!("Failed to create a new FunctionArgument: \"Type\" expected in \"kind\", got \"{:?}\"", kind)
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expression {
	Number(i32),
	String(String),

	Unary(Operator, Box<Expression>),
	Binary(Box<Expression>, Operator, Box<Expression>),

	FunctionCall {
		callee: Box<Expression>,
		generics: Vec<Expression>,
		arguments: Vec<Expression>
	},

	ArrayAccess(Box<Expression>, Box<Expression>),
	DotAccess(Box<Expression>, Box<Expression>),
	ArrowAccess(Box<Expression>, Box<Expression>),
	Array(Vec<Expression>),
	Identifier(String),

	Type {
		base: Box<Expression>,
		generics: Vec<Expression>,
		array_type: bool
	},

	TypeCast {
		value: Box<Expression>,
		to_type: Box<Expression> //* */ Expression::Type
	},

	Empty,
	Unimplemented
}

pub struct ParserTypeUtility;

impl ParserTypeUtility {
	pub fn void_type() -> Box<Expression> {
		ParserTypeUtility::from_parser_type(Type::Void)
	}

	pub fn from_parser_type(parser_type: Type) -> Box<Expression> {
		Box::new(Expression::Identifier(
			parser_type.as_parser_type_str().to_owned()
		))
	}

	pub fn from_string(name: String) -> Box<Expression> {
		Box::new(Expression::Identifier(name))
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

	fn parse_expression_pairs(&self, pairs_borrowed: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs_borrowed.clone();
		self.parse_expression(pairs.next().expect("Unexpected end of input"))
	}

	fn parse_expression(&self, pair: Pair<'p, Rule>) -> Expression {
		let mut pairs = pair.into_inner().peekable();
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

				Rule::binary_expression | Rule::expression => self.parse_expression_pairs(pair.into_inner()),
				Rule::function_call_expression => self.parse_function_call_expression(pair.into_inner()),
				Rule::type_cast_expression => self.parse_type_cast_expression(pair.into_inner()),

				Rule::number_literal => self.parse_number_literal(pair),
				Rule::string_literal => self.parse_string_literal(pair),

				Rule::identifier => Expression::Identifier(pair.as_str().to_owned()),
				Rule::r#type => self.parse_type(pair),

				rule => panic!("Got invalid expression rule: \"{:?}\"", rule),
			}
		} else {
			panic!("Unexpected end of input");
		}
	}

	fn parse_type_cast_expression(&self, pairs_borrowed: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs_borrowed.clone();

		// verify but skip value pair to let parse_expression handle it
		pairs
			.next()
			.expect("Failed to parse value of a type cast");

		let type_pair = pairs
			.next()
			.expect("Failed to parse type of a type cast");

		Expression::TypeCast {
			value: Box::new(self.parse_expression_pairs(pairs_borrowed)),
			to_type: Box::new(self.parse_type(type_pair))
		}
	}

	/// Used for function **definition** statements. Parses generics **as identifiers** to later be substituted with proper types
	/// 
	/// # Arguments
	/// 
	/// * `pair` - A Pest.rs parser pair with type Rule::definition_generics
	fn parse_generics_as_identifiers(&self, pair: Pair<'p, Rule>) -> Vec<Expression> {
		if pair.as_rule() != Rule::definition_generics {
			panic!("Got an unexpected rule. Expected \"definition_generics\", got \"{:?}\"", pair.as_rule());
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

	/// Used for function **call** statements. Parses generics **as parser expression types** to later be substituted with proper types
	/// 
	/// # Arguments
	/// 
	/// * `pair` - A Pest.rs parser pair with type Rule::call_generics
	fn parse_generics_as_types(&self, pair: Pair<'p, Rule>) -> Vec<Expression> {
		if pair.as_rule() != Rule::call_generics {
			panic!("Got an unexpected rule. Expected \"call_generics\", got \"{:?}\"", pair.as_rule());
		}

		let insides = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new(); //* Expression::Identifier only

		for arg in insides {
			if arg.as_rule() != Rule::r#type {
				panic!("Got an unexpected rule as a generics argument. Expected \"type\", got \"{:?}\"", arg.as_rule());
			}

			generics.push(self.parse_type(arg));
		}

		generics
	}

	fn parse_type(&self, pair: Pair<'p, Rule>) -> Expression {
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
			generics = self.parse_generics_as_identifiers(generics_pair);
		}

		Expression::Type {
			base: ParserTypeUtility::from_string(name_pair.as_str().to_owned()),
			generics,
			array_type: is_array_type
		}
	}

	fn parse_function_call_expression(&self, pairs_borrowed: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs_borrowed.clone();
		
		// extract callee
		let callee_pair = pairs
			.next()
			.expect("Identifier or expression expected in function call expression, got nothing");

		match callee_pair.as_rule() {
			Rule::number_literal | Rule::string_literal => panic!("Number and string literals are not valid function names"),
			_ => ()
		}

		let callee = self.parse_expression_pairs(pairs_borrowed);

		// extract arguments and/or generics
		let mut next_pair = pairs.next();

		// if there's no arguments or generics, return early
		if next_pair.is_none() {
			// return Expression::FunctionCall(Box::new(callee), Vec::new());
			return Expression::FunctionCall {
				callee: Box::new(callee),
				generics: Vec::new(),
				arguments: Vec::new()
			};
		}

		let next_pair_unwrapped = next_pair
			.clone()
			.unwrap();

		let mut generics: Vec<Expression> = Vec::new();

		// if the next pair is generics, parse them
		if next_pair_unwrapped.as_rule() == Rule::call_generics {
			generics = self.parse_generics_as_types(next_pair_unwrapped);

			// go to the next pair, which can be the arguments
			next_pair = pairs.next();
		}
		
		// if the next pair is not arguments then return early
		if next_pair.is_none() {
			return Expression::FunctionCall {
				callee: Box::new(callee),
				generics: generics,
				arguments: Vec::new()
			};
		}

		// otherwise, parse the arguments
		let args_pair = next_pair.unwrap();
		let mut args: Vec<Expression> = Vec::new();

		for arg_pair in args_pair.into_inner() {
			// arg_pair is always wrapped in an expression in this case
			let parsed = self.parse_expression_pairs(arg_pair.into_inner());
			args.push(parsed);
		}

		// return
		Expression::FunctionCall {
			callee: Box::new(callee),
			generics,
			arguments: args
		}
	}

	fn parse_function_definition(&self, pairs_borrowed: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs_borrowed.clone();
		let mut header_pairs = pairs.next().expect("Failed to parse function header").into_inner();

		// parsing header
		let name = header_pairs.next().expect("Failed to parse function name");

		let mut generics: Vec<Expression> = Vec::new();
		let mut arguments: Vec<FunctionArgument> = Vec::new();

		// check if arguments or generics exist
		if header_pairs.len() > 0 {
			let next_pair = header_pairs
				.clone()
				.next()
				.unwrap(); // header_pairs is guaranteed to have at least one pair left

			if next_pair.as_rule() == Rule::definition_generics {
				generics = self.parse_generics_as_identifiers(next_pair);
				header_pairs.next();
			}

			// now check for arguments
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

					// we have to save "arg" for parsing the type next, so we must clone it beforehand
					let arg_name = self.parse_expression_pairs(arg.clone());

					// now assume that the pair has been consumed, can move to the next one
					// it is expected that this will be of type Rule::type
					arg.next();

					let arg_type = arg
						.next()
						.expect("Failed to parse function argument type");

					let arg_type = self.parse_type(arg_type);
					arguments.push(FunctionArgument::new(arg_name, arg_type));
				}
			}
		}

		let body_pairs = pairs
			.next()
			.expect("Failed to parse function body")
			.into_inner();

		// now can safely skip function_definition_arguments node
		header_pairs.next();

		// check if return type exists
		let return_type = if header_pairs.len() > 0 {
			let return_type_pair = header_pairs.next().unwrap();
			self.parse_type(return_type_pair)
		} else {
			Expression::Type {
				base: ParserTypeUtility::void_type(),
				generics: Vec::new(),
				array_type: false
			}
		};

		Statement::FunctionDefinition {
			name: name.as_str().to_owned(),
			generics,
			arguments,
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

	fn parse_variable_definition(&self, pairs_borrowed: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs_borrowed.clone();

		let name = pairs
			.next()
			.expect("Failed to parse variable name");

		let mut next_pair = pairs
			.next()
			.expect("Failed to parse variable type/value");

		let mut kind = Expression::Empty;

		#[allow(unused_assignments)]
		let mut value = Expression::Empty;

		if next_pair.as_rule() == Rule::r#type {
			kind = self.parse_type(next_pair);

			next_pair = pairs
				.next()
				.expect("Failed to parse variable value");
		}
		
		if next_pair.as_rule() == Rule::expression {
			value = self.parse_expression(next_pair);
		} else {
			panic!("Got unexpected rule \"{:?}\" in variable definitition", next_pair.as_rule());
		}

		Statement::VariableDefinition {
			name: name.as_str().to_owned(),
			kind,
			value
		}
	}

	fn parse_function_call(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let parsed = self.parse_expression_pairs(pairs);

		if let Expression::FunctionCall { callee, generics, arguments } = parsed {
			return Statement::FunctionCall {
				callee: *callee,
				generics,
				arguments
			};
		}

		panic!("Unable to parse function call statement: callee or arguments parameters are invalid");
	}

	fn parse_return(&self, pairs: Pairs<'p, Rule>) -> Statement {
		if pairs.len() > 0 {
			return Statement::Return(self.parse_expression_pairs(pairs));
		}

		Statement::Return(Expression::Empty)
	}
}