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

impl FunctionArgument {
	pub fn new(name: Expression, kind: Expression) -> FunctionArgument {
		match kind {
			Expression::Type { .. } => FunctionArgument { name, kind },
			_ => panic!("Got an unexpected \"kind\" argument. Expected \"{:?}\", got \"{:?}\"", Rule::r#type, kind)
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct ClassFunctionAttributes {
	constructor: bool,
	private: bool,
	public: bool,
	get: bool,
	set: bool,

	// ugh
	static_: bool
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
		attributes: ClassFunctionAttributes,
		generics: Vec<Expression>,
		arguments: Vec<FunctionArgument>,
		return_type: Expression, //* Expression::Type
		statements: Vec<Statement>
	}
}

impl ClassDefinitionMember {
	pub fn function_from_statement(statement: Statement, attributes: ClassFunctionAttributes) -> ClassDefinitionMember {
		if let Statement::FunctionDefinition { name, generics, arguments, return_type, statements } = statement {
			return ClassDefinitionMember::Function { name, attributes, generics, arguments, return_type, statements };
		} else {
			panic!("Failed to convert invalid statement into a ClassDefinitionMember::Function");
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

				rule => panic!("Got an unexpected first rule. Expected \"{:?}\", got \"{:?}\"", Rule::program, rule)
				// rule => panic!("Expected \"{:?}\" as first rule, got \"{:?}\"", Rule::program, rule)
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

				rule => panic!("Got an unexpected statement rule \"{:?}\"", rule)
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

			operator => panic!("Got an unexpected operator. Expected \"+\", \"-\", \"/\", \"*\" or \"%\", got \"{}\"", operator)
		}
	}

	fn parse_expression(&self, expression_pair: Pair<'p, Rule>) -> Expression {
		let mut pairs = expression_pair
			.clone()
			.into_inner()
			.peekable();

		// check if an iterator is empty
		if pairs.len() < 1 {
			return self.parse_term(expression_pair);
		}

		self.parse_expression_with_precedence(&mut pairs, 0)
	}

	fn parse_expression_with_precedence(&self, pairs: &mut Peekable<Pairs<'p, Rule>>, precedence: u8) -> Expression {
		if pairs.len() < 1 {
			unreachable!("Iterator of pairs is empty");
		}

		let mut left = self.parse_term(pairs.next().unwrap());
	
		while let Some(pair) = pairs.peek() {
			if pair.as_rule() == Rule::operator {
				let operator_precedence = self.get_operator_precedence(pair);
				
				if operator_precedence < precedence {
					break;
				}
	
				let operator = self.parse_operator(pair);
				pairs.next(); // consume the operator
				
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
			
			operator => panic!("Got an unexpected operator. Expected \"+\", \"-\", \"/\", \"*\" or \"%\", got \"{}\"", operator)
		}
	}

	fn parse_identifier(&self, pair: Pair<'p, Rule>) -> Expression {
		if pair.as_rule() != Rule::identifier {
			panic!("Got an unexpected rule. Expected \"{:?}\", got \"{:?}\"", Rule::identifier, pair.as_rule());
		}

		Expression::Identifier(pair.as_str().to_owned())
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

	fn parse_term(&self, pair: Pair<'p, Rule>) -> Expression {
		match pair.as_rule() {
			Rule::unary_expression => {
				let operator = self.parse_operator(&pair);
				let operand = self.parse_term(pair);

				Expression::Unary(operator, Box::new(operand))
			}

			Rule::binary_expression | Rule::expression => self.parse_expression(pair),
			Rule::function_call_expression => self.parse_function_call_expression(pair.into_inner()),
			Rule::type_cast_expression => self.parse_type_cast_expression(pair.into_inner()),

			Rule::number_literal => self.parse_number_literal(pair),
			Rule::string_literal => self.parse_string_literal(pair),

			Rule::identifier => self.parse_identifier(pair),
			Rule::r#type => self.parse_type(pair),

			rule => panic!("Got invalid expression rule: \"{:?}\"", rule),
		}
	}

	fn parse_type_cast_expression(&self, pairs_borrowed: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs_borrowed.clone();

		// verify but skip value pair to let parse_expression handle it
		let value_pair = pairs
			.next()
			.expect("Failed to parse value of a type cast");

		let type_pair = pairs
			.next()
			.expect("Failed to parse type of a type cast");

		Expression::TypeCast {
			value: Box::new(self.parse_expression(value_pair)),
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
			panic!("Got an unexpected rule. Expected \"{:?}\", got \"{:?}\"", Rule::definition_generics, pair.as_rule());
		}

		let inner_pairs = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new(); //* Expression::Identifier only

		for arg in inner_pairs {
			if arg.as_rule() != Rule::identifier {
				panic!("Got an unexpected rule as a generics argument. Expected \"{:?}\", got \"{:?}\"", Rule::identifier, arg.as_rule());
			}

			generics.push(self.parse_identifier(arg));
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
			panic!("Got an unexpected rule. Expected \"{:?}\", got \"{:?}\"", Rule::call_generics, pair.as_rule());
		}

		let inner_pairs = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new(); //* Expression::Identifier only

		for arg in inner_pairs {
			if arg.as_rule() != Rule::r#type {
				panic!("Got an unexpected rule as a generics argument. Expected \"{:?}\", got \"{:?}\"", Rule::r#type, arg.as_rule());
			}

			generics.push(self.parse_type(arg));
		}

		generics
	}

	fn parse_type(&self, pair: Pair<'p, Rule>) -> Expression {
		if pair.as_rule() != Rule::r#type {
			panic!("Got an unexpected rule as a type. Expected \"{:?}\", got \"{:?}\"", Rule::r#type, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		// check if the type is an array type
		let next_pair = inner_pairs.peek().expect("Failed to parse type (got an empty type)");
		let is_array_type = next_pair.as_rule() == Rule::array_type;

		if is_array_type {
			inner_pairs = next_pair.into_inner();
		}
		
		// get type identifier
		let name_pair = inner_pairs.next().expect("Failed to parse type name");
		let mut generics: Vec<Expression> = Vec::new();

		// check if there are generics present
		if inner_pairs.len() > 0 {
			let generics_pair = inner_pairs.next().expect("Failed to parse type generics");
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

		// check for invalid function name
		match callee_pair.as_rule() {
			Rule::number_literal | Rule::string_literal => panic!("Number and string literals are not valid function names"),
			_ => ()
		}

		let callee = self.parse_expression(callee_pair);

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
			let expression_pair = arg_pair
				.into_inner()
				.next()
				.expect("Failed to parse function call argument");

			println!("expression_pair (function_call) = {}", expression_pair);

			let parsed = self.parse_expression(expression_pair);
			args.push(parsed);
		}

		// return
		Expression::FunctionCall {
			callee: Box::new(callee),
			generics,
			arguments: args
		}
	}

	fn parse_class_function_attributes(&self, pair: Pair<'p, Rule>) -> ClassFunctionAttributes {
		if pair.as_rule() != Rule::attributes {
			panic!("Got an unexpected rule as function attributes. Expected \"{:?}\", got \"{:?}\"", Rule::attributes, pair.as_rule());
		}

		let mut inner = pair.into_inner();

		let mut attributes: ClassFunctionAttributes = Default::default();
		let mut met_attributes: Vec<String> = Vec::new(); // keep track of which attributes have already been defined to prevent users from defining them twice

		while let Some(pair) = inner.next() {
			let as_str = pair.as_str();
			let owned = as_str.clone().to_owned();

			if met_attributes.contains(&owned) {
				panic!("Found more than one \"{}\" attribute definition. Cannot define an attribute more than once for a single function", as_str);
			}

			match as_str {
				"constructor" => attributes.constructor = true,
				"private" => attributes.private = true,
				"public" => attributes.public = true,
				"get" => attributes.get = true,
				"set" => attributes.set = true,
				"static" => attributes.static_ = true,

				_ => unreachable!("Failed to parse an unknown function attribute \"{}\"", as_str)
			};

			// mark attribute as defined
			met_attributes.push(owned);
		}

		attributes
	}

	fn parse_function_definition(&self, pairs_borrowed: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs_borrowed.clone();
		let mut header_pairs = pairs
			.next()
			.expect("Failed to parse function header")
			.into_inner();

		// parsing header
		let name = header_pairs
			.next()
			.expect("Failed to parse function name");

		let mut generics: Vec<Expression> = Vec::new();
		let mut arguments: Vec<FunctionArgument> = Vec::new();

		// check if arguments or generics exist
		if header_pairs.len() > 0 {
			let next_pair = header_pairs
				.peek()
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
					.expect("Failed to parse function definition arguments")
					.into_inner();

				let filtered_arguments: Vec<Pair<'p, Rule>> = argument_pairs
					.clone()
					.filter(|pair| pair.as_rule() == Rule::function_definition_argument)
					.collect();

				for arg_pair in filtered_arguments {
					// arg_pair is here expected to be function_definition_argument
					let mut arg = arg_pair.into_inner();

					let arg_name = self.parse_identifier(
						arg
							.next()
							.expect(&format!("Failed to parse a function argument name. Expected \"{:?}\", got nothing", Rule::identifier))
					);

					let arg_type = arg
						.next()
						.expect("Failed to parse a function definition argument type");

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
		while header_pairs.len() > 1 {
			header_pairs.next();
		}

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

	fn parse_class_definition_function(&self, pair: Pair<'p, Rule>) -> ClassDefinitionMember {
		// * NOTE: attributes are to be checked later by the optimization stage/compiler

		if pair.as_rule() != Rule::class_definition_function {
			panic!("Got an unexpected rule as a class definition member. Expected \"{:?}\", got \"{:?}\"", Rule::class_definition_function, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		let next_pair = inner_pairs
			.peek()
			.expect(&format!("Failed to parse a class definition function. Expected \"{:?}\" or \"{:?}\", got nothing", Rule::function_definition_stmt, Rule::attributes));
		
		let mut attributes: ClassFunctionAttributes = Default::default();

		if next_pair.as_rule() == Rule::attributes {
			attributes = self.parse_class_function_attributes(next_pair);

			// skip attributes if they exist
			inner_pairs.next();
		}

		let statement_pair = inner_pairs
			.next()
			.expect(&format!("Failed to parse a class definition function. Expected \"{:?}\", got nothing", Rule::function_definition_stmt));

		let function_statement = self.parse_function_definition(statement_pair.into_inner());
		ClassDefinitionMember::function_from_statement(function_statement, attributes)
	}

	fn parse_class_definition_variable(&self, pair: Pair<'p, Rule>) -> ClassDefinitionMember {
		if pair.as_rule() != Rule::class_definition_variable {
			panic!("Got an unexpected rule as a class definition member. Expected \"{:?}\", got \"{:?}\"", Rule::class_definition_variable, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		let name = inner_pairs
			.next()
			.expect(&format!("Failed to parse class definition variable. Expected \"{:?}\", got nothing", Rule::identifier));

		let kind = inner_pairs
			.next()
			.expect(&format!("Failed to parse class definition variable. Expected \"{:?}\", got nothing", Rule::r#type));

		let default_value_option = inner_pairs.next();
		let mut default_value = Expression::Empty;

		if default_value_option.is_some() {
			default_value = self.parse_expression(default_value_option.unwrap());
		}

		ClassDefinitionMember::Variable {
			name: name.as_str().to_owned(),
			kind: self.parse_type(kind),
			default_value
		}
	}

	fn parse_class_definition_member(&self, pair: Pair<'p, Rule>) -> ClassDefinitionMember {
		if pair.as_rule() != Rule::class_definition_member {
			panic!("Got unexpected rule as a class definition member. Expected \"{:?}\", got \"{:?}\"", Rule::class_definition_member, pair.as_rule());
		}

		let inner = pair
			.into_inner()
			.next()
			.expect("Failed to parse class definition member");

		match inner.as_rule() {
			Rule::class_definition_variable => self.parse_class_definition_variable(inner),
			Rule::class_definition_function => self.parse_class_definition_function(inner),
			
			rule => panic!(
				"Got unexpected rule as a class definition member. Expected \"{:?}\" or \"{:?}\", got \"{:?}\"",
				Rule::class_definition_variable,
				Rule::class_definition_function,
				rule
			)
		}
	}

	fn parse_class_definition(&self, pairs_borrowed: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs_borrowed.clone();

		let name = pairs
			.next()
			.expect("Failed to parse class definition name");

		if name.as_rule() != Rule::identifier {
			panic!("Got unexpected rule \"{:?}\" as a name for class definition, expected \"{:?}\"", name.as_rule(), Rule::identifier);
		}

		let next_pair_option = pairs.peek();
		let next_pair: Pair<Rule>;

		// if the class is empty, return early
		if next_pair_option.is_none() {
			return Statement::ClassDefinition {
				name: name.as_str().to_owned(),
				generics: Vec::new(),
				members: Vec::new()
			};
		}

		// unwrap the next pair
		next_pair = next_pair_option.unwrap();
		let mut generics: Vec<Expression> = Vec::new();

		// check if its definition_generics
		if next_pair.as_rule() == Rule::definition_generics {
			generics = self.parse_generics_as_identifiers(next_pair);

			// if no class members are provided, return early
			if pairs.peek().is_none() {
				return Statement::ClassDefinition {
					name: name.as_str().to_owned(),
					generics,
					members: Vec::new()
				};
			}
		}

		let mut members: Vec<ClassDefinitionMember> = Vec::new();

		while let Some(pair) = pairs.next() {
			members.push(self.parse_class_definition_member(pair));
		}

		Statement::ClassDefinition {
			name: name.as_str().to_owned(),
			generics,
			members
		}
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
		let expression_pair = pairs
			.peek()
			.expect("Failed to parse function call expression as a statement");

		println!("expression_pair (function_call) = {}", expression_pair);

		let parsed = self.parse_function_call_expression(expression_pair.into_inner());

		println!("parsed = {:?}", parsed);

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
			let expression_pair = pairs
				.peek()
				.expect("Failed to parse function call expression as a statement");

			println!("expression_pair (return) = {}", expression_pair);

			return Statement::Return(self.parse_expression(expression_pair));
		}

		Statement::Return(Expression::Empty)
	}
}