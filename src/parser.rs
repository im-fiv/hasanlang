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

	FunctionDeclaration {
		name: String,
		generics: Vec<Expression>,
		arguments: Vec<FunctionArgument>,
		return_type: Expression //* Expression::Type
	},

	TypeDefinition {
		name: String,
		generics: Vec<Expression>,
		definition: Expression
	},

	ClassDefinition {
		name: String,
		generics: Vec<Expression>,
		members: Vec<ClassDefinitionMember>
	},

	ClassDeclaration {
		name: String,
		generics: Vec<Expression>,
		members: Vec<ClassDeclarationMember>
	},

	VariableDefinition {
		name: String,
		kind: Expression, //* Experssion::Type
		value: Expression
	},

	VariableAssign(Expression, Expression),

	FunctionCall {
		callee: Expression,
		generics: Vec<Expression>,
		arguments: Vec<Expression>
	},

	Return(Expression),

	EnumDefinition {
		name: String,
		members: Vec<EnumMember>
	},

	If {
		condition: Expression,
		statements: Vec<Statement>,
		elseif_branches: Vec<IfBranch>,
		else_branch: Option<IfBranch>
	},

	While {
		condition: Expression,
		statements: Vec<Statement>
	},
	
	Break,

	// special statements that are not intended to be used traditionally
	Unimplemented
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct IfBranch {
	condition: Expression,
	statements: Vec<Statement>
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct EnumMember {
	name: String
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
			_ => panic!("Got an unexpected 'kind' argument. Expected '{:?}', got '{:?}'", Rule::r#type, kind)
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

#[derive(Debug)]
pub enum ClassDeclarationMember {
	Variable {
		name: String,
		kind: Expression //* Expression::Type
	},

	Function {
		name: String,
		attributes: ClassFunctionAttributes,
		generics: Vec<Expression>,
		arguments: Vec<FunctionArgument>,
		return_type: Expression //* Expression::Type
	}
}

impl ClassDeclarationMember {
	pub fn function_from_statement(statement: Statement, attributes: ClassFunctionAttributes) -> Self {
		if let Statement::FunctionDeclaration { name, generics, arguments, return_type } = statement {
			return ClassDeclarationMember::Function { name, attributes, generics, arguments, return_type };
		} else {
			panic!("Failed to convert invalid statement into a ClassDefinitionMember::Function");
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expression {
	Number(i32),
	String(String),
	Boolean(bool),

	Unary(UnaryOperator, Box<Expression>),
	Binary(Box<Expression>, BinaryOperator, Box<Expression>),

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

		// attributes
		array: bool,
		raw: bool
	},

	TypeCast(Box<Expression>, Box<Expression>),

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
pub enum BinaryOperator {
	Plus,
	Minus,
	Divide,
	Times,
	Modulo,
	Equals,
	NotEquals,
	And,
	Or,
	GreaterThan,
	LessThan,
	GreaterThanEqual,
	LessThanEqual
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
	Minus,
	Not
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
				Rule::string_literal => (),

				Rule::program => statements = Some(self.parse_program(pair.into_inner())),

				rule => panic!("Failed to parse: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::program, rule)
			}
		}

		statements.unwrap_or_else(|| unreachable!("Failed to parse program: no idea what went wrong"))
	}

	fn parse_program(&self, pairs: Pairs<'p, Rule>) -> Vec<Statement> {
		let mut statements: Vec<Statement> = Vec::new();

		for pair in pairs {
			if pair.as_rule() == Rule::EOI {
				continue;
			}

			let statement = match pair.as_rule() {
				Rule::function_definition_stmt => self.parse_function_definition(pair.into_inner()),
				Rule::function_declaration_stmt => self.parse_function_declaration(pair),
				Rule::type_definition_stmt => self.parse_type_definition(pair.into_inner()),
				Rule::class_definition => self.parse_class_definition(pair.into_inner()),
				Rule::class_declaration => self.parse_class_declaration(pair.into_inner()),
				Rule::variable_definition_stmt => self.parse_variable_definition(pair.into_inner()),
				Rule::variable_assign_stmt => self.parse_variable_assign(pair.into_inner()),
				Rule::function_call_stmt => self.parse_function_call(pair.into_inner()),
				Rule::return_stmt => self.parse_return(pair.into_inner()),
				Rule::enum_definition_stmt => self.parse_enum_definition(pair.into_inner()),

				Rule::if_stmt => self.parse_if(pair.into_inner()),
				Rule::while_stmt => self.parse_while(pair.into_inner()),
				Rule::break_stmt => Statement::Break,

				rule => panic!("Failed to parse program: got an unexpected statement rule '{:?}'", rule)
			};

			statements.push(statement);
		}

		statements
	}

	fn parse_operator(&self, pair: &Pair<'p, Rule>) -> BinaryOperator {
		match pair.as_str() {
			"+" => BinaryOperator::Plus,
			"-" => BinaryOperator::Minus,
			"/" => BinaryOperator::Divide,
			"*" => BinaryOperator::Times,
			"%" => BinaryOperator::Modulo,
			"==" => BinaryOperator::Equals,
			"!=" => BinaryOperator::NotEquals,
			"and" => BinaryOperator::And,
			"or" => BinaryOperator::Or,
			">" => BinaryOperator::GreaterThan,
			"<" => BinaryOperator::LessThan,
			">=" => BinaryOperator::GreaterThanEqual,
			"<=" => BinaryOperator::LessThanEqual,

			operator => panic!("Got an unexpected operator. Expected '+', '-', '/', '*', '%', '==', '!=', 'and', 'or', '>', '<', '>=', or '<=', got '{}'", operator)
		}
	}

	fn parse_unary_operator(&self, pair: &Pair<'p, Rule>) -> UnaryOperator {
		match pair.as_str() {
			"-" => UnaryOperator::Minus,
			"not" => UnaryOperator::Not,

			operator => panic!("Got an unexpected unary operator. Expected '-', or 'not', got '{}'", operator)
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

		if expression_pair.as_rule() == Rule::recursive_expression {
			return self.parse_recursive_expression(expression_pair.into_inner());
		}

		self.parse_expression_with_precedence(&mut pairs, 0)
	}

	fn parse_expression_with_precedence(&self, pairs: &mut Peekable<Pairs<'p, Rule>>, precedence: u8) -> Expression {
		if pairs.len() < 1 {
			unreachable!("Failed to parse expression: pairs are empty");
		}

		let left_pair = pairs
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse expression: pairs are empty"));
		
		let mut left = self.parse_term(left_pair);
	
		while let Some(pair) = pairs.peek() {
			if pair.as_rule() == Rule::binary_operator {
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
			"==" | "!=" | "and" | "or" | ">" | "<" | ">=" | "<=" => 1,
			"+" | "-" => 2,
			"*" | "/" | "%" => 3,
			
			operator => panic!("Got an unexpected operator. Expected '+', '-', '/', '*', '%', '==', '!=', 'and', or 'or', got '{}'", operator)
		}
	}

	fn parse_identifier(&self, pair: Pair<'p, Rule>) -> Expression {
		if pair.as_rule() != Rule::identifier {
			panic!("Failed to parse identifier: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::identifier, pair.as_rule());
		}

		Expression::Identifier(pair.as_str().to_owned())
	}

	fn parse_number_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let string = pair.as_str().to_owned();
		let literal = string.parse::<NumberType>()
			.expect(format!("Failed to parse number literal '{}'", string).as_str());

		Expression::Number(literal)
	}

	fn parse_string_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let literal = pair.as_str().to_owned();
		let clean_literal = literal.trim_start_matches(&['\'', '\"'][..]).trim_end_matches(&['\'', '\"'][..]);

		Expression::String(clean_literal.to_owned())
	}

	fn parse_boolean_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let literal = pair.as_str();

		match literal {
			"true" => Expression::Boolean(true),
			"false" => Expression::Boolean(false),

			_ => unreachable!("Failed to parse boolean literal: expected 'true' or 'false', got '{}'", literal)
		}
	}

	fn parse_term(&self, pair: Pair<'p, Rule>) -> Expression {
		match pair.as_rule() {
			Rule::unary_expression => self.parse_unary_expression(pair.into_inner()),
			Rule::binary_expression | Rule::expression => self.parse_expression(pair),

			Rule::array_expression => self.parse_array_expression(pair.into_inner()),
			Rule::recursive_expression => self.parse_recursive_expression(pair.into_inner()),

			Rule::number_literal |
			Rule::string_literal |
			Rule::boolean_literal => self.parse_literal(pair),

			Rule::identifier => self.parse_identifier(pair),
			Rule::r#type => self.parse_type(pair),

			rule => panic!("Failed to parse term: got invalid expression rule '{:?}'", rule),
		}
	}

	fn parse_elseif_branch(&self, pair: Pair<'p, Rule>) -> IfBranch {
		let mut pairs = pair.into_inner();

		if pairs.len() < 1 {
			panic!("Failed to parse elseif branch: pairs are empty");
		}

		let expression_pair = pairs
			.next()
			.expect("Failed to parse elseif branch: expected expression, got nothing");

		let statements_pair = pairs
			.next()
			.expect("Failed to parse elseif branch: expected statements, got nothing");

		IfBranch {
			condition: self.parse_expression(expression_pair),
			statements: self.parse_program(statements_pair.into_inner())
		}
	}

	fn parse_else_branch(&self, pair: Pair<'p, Rule>) -> IfBranch {
		let mut pairs = pair.into_inner();

		if pairs.len() < 1 {
			panic!("Failed to parse else branch: pairs are empty");
		}

		let statements_pair = pairs
			.next()
			.expect("Failed to parse else branch: expected statements, got nothing");

		IfBranch {
			condition: Expression::Empty,
			statements: self.parse_program(statements_pair.into_inner())
		}
	}

	fn parse_if(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		if pairs.len() < 1 {
			panic!("Failed to parse if statement: pairs are empty");
		}

		let condition_pair = pairs
			.next()
			.expect("Failed to parse if statement: condition is missing");

		let statements_pair = pairs
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse if statement: statements are missing"));

		let mut elseif_branches: Vec<IfBranch> = Vec::new();
		let mut else_branch: Option<IfBranch> = None;

		while let Some(pair) = pairs.next() {
			if !matches!(pair.as_rule(), Rule::if_elseif | Rule::if_else) {
				panic!("Failed to parse if statement: expected '{:?}' or '{:?}', got '{:?}'", Rule::if_elseif, Rule::if_else, pair.as_rule());
			}
			
			if pair.as_rule() == Rule::if_elseif {
				elseif_branches.push(self.parse_elseif_branch(pair));
			} else {
				else_branch = Some(self.parse_else_branch(pair));
			}
		}

		Statement::If {
			condition: self.parse_expression(condition_pair),
			statements: self.parse_program(statements_pair.into_inner()),
			elseif_branches,
			else_branch
		}
	}

	fn parse_while(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let expression_pair = pairs
			.next()
			.expect("Failed to parse while statement: expected expression, got nothing");

		let statements_pair = pairs
			.next()
			.expect("Failed to parse while statement: expected statements, got nothing");

		Statement::While {
			condition: self.parse_expression(expression_pair),
			statements: self.parse_program(statements_pair.into_inner())
		}
	}

	fn parse_unary_expression(&self, pairs: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs.clone();

		let operator = self.parse_unary_operator(&pairs.next().expect("Failed to parse unary expression: no operator is present"));
		let operand = self.parse_expression(pairs.next().expect("Failed to parse unary expression: no operand is present"));

		Expression::Unary(operator, Box::new(operand))
	}

	fn parse_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		match pair.as_rule() {
			Rule::number_literal => self.parse_number_literal(pair),
			Rule::string_literal => self.parse_string_literal(pair),
			Rule::boolean_literal => self.parse_boolean_literal(pair),

			rule => unreachable!("Failed to parse literal: got an unexpected rule. Expected number or string literal, got '{:?}'", rule)
		}
	}

	fn parse_recursive_expression(&self, pairs: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs.clone();

		if pairs.len() == 1 {
			return self.parse_expression(pairs.next().unwrap_or_else(|| unreachable!("Failed to parse a recursive expression: pairs are empty")));
		}

		let mut current_expression = Expression::Empty;

		while let Some(pair) = pairs.next() {
			current_expression = match pair.as_rule() {
				Rule::identifier => self.parse_identifier(pair),
				Rule::number_literal | Rule::string_literal => self.parse_literal(pair),

				Rule::expression => self.parse_expression(pair),

				Rule::recursive_call => self.parse_function_call_expression(current_expression, pair),
				Rule::recursive_array => self.parse_array_access_expression(current_expression, pair),
				Rule::recursive_dot => self.parse_dot_access_expression(current_expression, pair),
				Rule::recursive_arrow => self.parse_arrow_access_expression(current_expression, pair),
				Rule::recursive_as => self.parse_type_cast_expression(current_expression, pair),

				rule => unreachable!("Failed to parse a recursive expression: expected a recursive expression term, got '{:?}'", rule)
			}
		}

		current_expression
	}

	fn parse_function_call_expression(&self, expression: Expression, pair: Pair<'p, Rule>) -> Expression {
		let mut call_insides = pair.into_inner();

		if call_insides.len() < 1 {
			return Expression::FunctionCall {
				callee: Box::new(expression),
				generics: Vec::new(),
				arguments: Vec::new()
			};
		}

		let mut next_pair = call_insides
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse function call expression: call pairs iterator is empty"));

		let mut generics: Vec<Expression> = Vec::new();
		let mut arguments: Vec<Expression> = Vec::new();

		// notify that incorrect generics are being passed to the function
		if next_pair.as_rule() == Rule::definition_generics {
			panic!("Incorrect usage of parse_function_call_expression. Definition generics were passed instead of call generics");
		}

		// if the next pair is of type call_generics, parse them, and exit if there are no arguments
		if next_pair.as_rule() == Rule::call_generics {
			generics = self.parse_generics_as_types(next_pair);

			let pair = call_insides.next();
			
			if pair.is_none() {
				return Expression::FunctionCall {
					callee: Box::new(expression),
					generics,
					arguments
				};
			}

			next_pair = pair.unwrap();
		}

		// parse arguments
		for arg_pair in next_pair.into_inner() {
			// arg_pair is always wrapped in an expression in this case
			let expression_pair = arg_pair
				.into_inner()
				.next()
				.expect("Failed to parse function call argument");

			let parsed = self.parse_expression(expression_pair);
			arguments.push(parsed);
		}

		Expression::FunctionCall {
			callee: Box::new(expression),
			generics,
			arguments
		}
	}

	fn parse_arrow_access_expression(&self, expression: Expression, pair: Pair<'p, Rule>) -> Expression {
		Expression::ArrowAccess(Box::new(expression), Box::new(self.parse_expression(pair)))
	}

	fn parse_dot_access_expression(&self, expression: Expression, pair: Pair<'p, Rule>) -> Expression {
		Expression::DotAccess(Box::new(expression), Box::new(self.parse_expression(pair)))
	}

	fn parse_array_access_expression(&self, expression: Expression, pair: Pair<'p, Rule>) -> Expression {
		Expression::ArrayAccess(Box::new(expression), Box::new(self.parse_expression(pair)))
	}

	fn parse_type_cast_expression(&self, expression: Expression, pair: Pair<'p, Rule>) -> Expression {
		let kind_pair = pair
			.into_inner()
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse type cast expression: pairs are empty"));

		let kind_parsed = Box::new(self.parse_type(kind_pair));
		Expression::TypeCast(Box::new(expression), kind_parsed)
	}

	fn parse_array_expression(&self, pairs: Pairs<'p, Rule>) -> Expression {
		let mut pairs = pairs.clone();
		let mut items: Vec<Expression> = Vec::new();

		while let Some(pair) = pairs.next() {
			let parsed_pair = self.parse_expression(pair);
			items.push(parsed_pair);
		}

		Expression::Array(items)
	}

	/// Used for **definition** statements. Parses generics **as identifiers** to later be substituted with proper types
	/// 
	/// # Arguments
	/// 
	/// * `pair` - A Pest.rs parser pair with type Rule::definition_generics
	fn parse_generics_as_identifiers(&self, pair: Pair<'p, Rule>) -> Vec<Expression> {
		if pair.as_rule() != Rule::definition_generics {
			panic!("Failed to parse generics: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::definition_generics, pair.as_rule());
		}

		let inner_pairs = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new(); //* Expression::Identifier only

		for arg in inner_pairs {
			if arg.as_rule() != Rule::identifier {
				panic!("Failed to parse generics: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::identifier, arg.as_rule());
			}

			generics.push(self.parse_identifier(arg));
		}

		generics
	}

	/// Used for **call** statements. Parses generics **as parser expression types** to later be substituted with proper types
	/// 
	/// # Arguments
	/// 
	/// * `pair` - A Pest.rs parser pair with type Rule::call_generics
	fn parse_generics_as_types(&self, pair: Pair<'p, Rule>) -> Vec<Expression> {
		if pair.as_rule() != Rule::call_generics {
			panic!("Failed to parse generics: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::call_generics, pair.as_rule());
		}

		let inner_pairs = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new();

		for arg in inner_pairs {
			if arg.as_rule() != Rule::r#type {
				panic!("Failed to parse generics: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::r#type, arg.as_rule());
			}

			generics.push(self.parse_type(arg));
		}

		generics
	}

	fn parse_enum_member(&self, pair: Pair<'p, Rule>) -> EnumMember {
		let mut pairs = pair.into_inner();

		let name = pairs
			.next()
			.expect("Failed to parse enum member: name is missing")
			.as_str()
			.to_owned();

		EnumMember { name }
	}

	fn parse_enum_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let name = pairs
			.next()
			.expect("Failed to parse enum definition: enum name is missing")
			.as_str()
			.to_owned();

		let mut members: Vec<EnumMember> = Vec::new();

		while let Some(pair) = pairs.next() {
			if pair.as_rule() != Rule::enum_member {
				panic!("Failed to parse enum definition member: expected rule '{:?}', got '{:?}'", Rule::enum_member, pair.as_rule());
			}

			members.push(self.parse_enum_member(pair));
		}

		Statement::EnumDefinition { name, members }
	}

	fn parse_type(&self, pair: Pair<'p, Rule>) -> Expression {
		if pair.as_rule() != Rule::r#type {
			panic!("Failed to parse type: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::r#type, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		// check if the type is an array type
		let mut next_pair = inner_pairs.peek().expect("Failed to parse type: everything is missing");

		let is_raw_type = next_pair.as_rule() == Rule::raw_type;

		if is_raw_type {
			inner_pairs = next_pair.into_inner();
			next_pair = inner_pairs.peek().expect("Failed to parse type: expected '[]' or identifier, got nothing");
		}

		let is_array_type = next_pair.as_rule() == Rule::array_type;

		if is_array_type {
			inner_pairs = next_pair.into_inner();
		}
		
		// get type identifier
		let name_pair = inner_pairs.next().expect("Failed to parse type: name is missing");
		let mut generics: Vec<Expression> = Vec::new();

		// check if there are generics present
		if inner_pairs.len() > 0 {
			let generics_pair = inner_pairs.next().unwrap_or_else(|| unreachable!("Failed to parse type: generics are missing"));
			generics = self.parse_generics_as_identifiers(generics_pair);
		}

		Expression::Type {
			base: ParserTypeUtility::from_string(name_pair.as_str().to_owned()),
			generics,
			array: is_array_type,
			raw: is_raw_type
		}
	}

	fn parse_class_function_attributes(&self, pair: Pair<'p, Rule>) -> ClassFunctionAttributes {
		if pair.as_rule() != Rule::attributes {
			panic!("Failed to parse function attributes: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::attributes, pair.as_rule());
		}

		let mut inner = pair.into_inner();

		let mut attributes: ClassFunctionAttributes = Default::default();
		let mut met_attributes: Vec<String> = Vec::new(); // keep track of which attributes have already been defined to prevent users from defining them twice

		while let Some(pair) = inner.next() {
			let as_str = pair.as_str();
			let owned = as_str.clone().to_owned();

			if met_attributes.contains(&owned) {
				panic!("Failed to parse function attributes: found more than one '{}' attribute definition. Cannot define an attribute more than once for any given function", as_str);
			}

			match as_str {
				"constructor" => attributes.constructor = true,
				"private" => attributes.private = true,
				"public" => attributes.public = true,
				"get" => attributes.get = true,
				"set" => attributes.set = true,
				"static" => attributes.static_ = true,

				_ => unreachable!("Failed to parse function attributes: unknown attribute '{}'", as_str)
			};

			// mark attribute as defined
			met_attributes.push(owned);
		}

		attributes
	}

	fn parse_function_header(&self, pair: Pair<'p, Rule>) -> (String, Vec<Expression>, Vec<FunctionArgument>, Expression) {
		let mut header_pairs = pair.into_inner();

		let name = header_pairs
			.next()
			.expect("Failed to parse function header: function name is missing");

		let mut generics: Vec<Expression> = Vec::new();
		let mut arguments: Vec<FunctionArgument> = Vec::new();

		// check if arguments or generics exist
		if header_pairs.len() > 0 {
			let next_pair = header_pairs
				.peek()
				.unwrap_or_else(|| unreachable!("Failed to parse function header: expected generics or arguments, got nothing")); // header_pairs is guaranteed to have at least one pair left

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
					.expect("Failed to parse function header: arguments are missing")
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
							.expect("Failed to parse function header: argument name is missing")
					);

					let arg_type = arg
						.next()
						.expect("Failed to parse function header: argument type is missing");

					let arg_type = self.parse_type(arg_type);
					arguments.push(FunctionArgument::new(arg_name, arg_type));
				}
			}
		}

		while header_pairs.len() > 1 {
			header_pairs.next();
		}

		let mut return_type = Expression::Type {
			base: ParserTypeUtility::void_type(),
			generics: Vec::new(),

			array: false,
			raw: true
		};
		
		if header_pairs.len() > 0 {
			let pair = header_pairs
				.next()
				.unwrap_or_else(|| unreachable!("Failed to parse function header: expected return type, got nothing"));

			if pair.as_rule() == Rule::r#type {
				return_type = self.parse_type(pair);
			}
		}

		(name.as_str().to_owned(), generics, arguments, return_type)
	}

	fn parse_function_declaration(&self, pair: Pair<'p, Rule>) -> Statement {
		if pair.as_rule() != Rule::function_declaration_stmt {
			panic!("Failed to parse function declaration: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::function_declaration_stmt, pair.as_rule());
		}

		let header_pair = pair
			.into_inner()
			.next()
			.expect("Failed to parse function declaration: function header is missing");

		let (name, generics, arguments, return_type) = self.parse_function_header(header_pair);
		Statement::FunctionDeclaration { name, generics, arguments, return_type }
	}

	fn parse_function_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let header_pair = pairs
			.next()
			.expect("Failed to parse function definition: function header is missing");

		// parsing header
		let (name, generics, arguments, return_type) = self.parse_function_header(header_pair);

		let body_pairs = pairs
			.next()
			.expect("Failed to parse function definition: function body is missing")
			.into_inner();

		Statement::FunctionDefinition {
			name,
			generics,
			arguments,
			return_type,
			statements: self.parse_program(body_pairs)
		}
	}

	fn parse_type_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let name_pair = pairs
			.next()
			.expect("Failed to parse type definition: expected an identifier as a type name, got nothing");

		let mut next_pair = pairs
			.next()
			.expect("Failed to parse type definition: expected generics/type, got nothing");

		let mut generics: Vec<Expression> = Vec::new();

		if next_pair.as_rule() == Rule::definition_generics {
			generics = self.parse_generics_as_identifiers(next_pair);

			next_pair = pairs
				.next()
				.expect("Failed to parse type definition: expected a type, got nothing");
		}

		let type_expression = self.parse_type(next_pair);

		Statement::TypeDefinition {
			name: name_pair.as_str().to_owned(),
			generics,
			definition: type_expression
		}
	}

	fn parse_class_definition_function(&self, pair: Pair<'p, Rule>) -> ClassDefinitionMember {
		// * NOTE: attributes are to be checked later by the optimization stage/compiler

		if pair.as_rule() != Rule::class_definition_function {
			panic!("Failed to parse a class definition function: expected rule '{:?}', got '{:?}'", Rule::class_definition_function, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		let next_pair = inner_pairs
			.peek()
			.expect(&format!("Failed to parse a class definition function: expected '{:?}' or '{:?}', got nothing", Rule::function_definition_stmt, Rule::attributes));
		
		let mut attributes: ClassFunctionAttributes = Default::default();

		if next_pair.as_rule() == Rule::attributes {
			attributes = self.parse_class_function_attributes(next_pair);

			// skip attributes if they exist
			inner_pairs.next();
		}

		let statement_pair = inner_pairs
			.next()
			.expect(&format!("Failed to parse a class definition function: expected rule '{:?}', got nothing", Rule::function_definition_stmt));

		let function_statement = self.parse_function_definition(statement_pair.into_inner());
		ClassDefinitionMember::function_from_statement(function_statement, attributes)
	}

	fn parse_class_definition_variable(&self, pair: Pair<'p, Rule>) -> ClassDefinitionMember {
		if pair.as_rule() != Rule::class_definition_variable {
			panic!("Failed to parse class definiton variable: expected rule '{:?}', got '{:?}'", Rule::class_definition_variable, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		let name = inner_pairs
			.next()
			.expect(&format!("Failed to parse class definition variable: expected '{:?}', got nothing", Rule::identifier));

		let kind = inner_pairs
			.next()
			.expect(&format!("Failed to parse class definition variable: expected '{:?}', got nothing", Rule::r#type));

		let default_value_option = inner_pairs.next();
		let mut default_value = Expression::Empty;

		if default_value_option.is_some() {
			default_value = self.parse_expression(default_value_option.unwrap_or_else(|| unreachable!("Failed to parse class definition variable: default value pair is missing")));
		}

		ClassDefinitionMember::Variable {
			name: name.as_str().to_owned(),
			kind: self.parse_type(kind),
			default_value
		}
	}

	fn parse_class_definition_member(&self, pair: Pair<'p, Rule>) -> ClassDefinitionMember {
		if pair.as_rule() != Rule::class_definition_member {
			panic!("Failed to parse class definition member: expected rule '{:?}', got '{:?}'", Rule::class_definition_member, pair.as_rule());
		}

		let inner = pair
			.into_inner()
			.next()
			.expect("Failed to parse class definition member: pairs are empty");

		match inner.as_rule() {
			Rule::class_definition_variable => self.parse_class_definition_variable(inner),
			Rule::class_definition_function => self.parse_class_definition_function(inner),
			
			rule => panic!(
				"Failed to parse class definition member: got unexpected rule as a class definition member. Expected '{:?}' or '{:?}', got '{:?}'",
				Rule::class_definition_variable,
				Rule::class_definition_function,
				rule
			)
		}
	}

	fn parse_class_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let name = pairs
			.next()
			.expect("Failed to parse class definition: class name is missing");

		if name.as_rule() != Rule::identifier {
			panic!("Failed to parse class definition: expected rule '{:?}' as a name for class definition, got '{:?}'", Rule::identifier, name.as_rule());
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
		next_pair = next_pair_option.unwrap_or_else(|| unreachable!("Failed to parse class definition: unexpected end of pairs. Expected class members or generics, got nothing"));
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

			// otherwise, skip the current pair
			pairs.next();
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

	fn parse_class_declaration_function(&self, pair: Pair<'p, Rule>) -> ClassDeclarationMember {
		// * NOTE: attributes are to be checked later by the optimization stage/compiler

		if pair.as_rule() != Rule::class_declaration_function {
			panic!("Got an unexpected rule as a class declaration member: expected rule '{:?}', got '{:?}'", Rule::class_declaration_function, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		let next_pair = inner_pairs
			.peek()
			.expect(&format!("Failed to parse a class declaration function: expected '{:?}' or '{:?}', got nothing", Rule::class_declaration_function, Rule::attributes));
		
		let mut attributes: ClassFunctionAttributes = Default::default();

		if next_pair.as_rule() == Rule::attributes {
			attributes = self.parse_class_function_attributes(next_pair);

			// skip attributes if they exist
			inner_pairs.next();
		}

		let function_pair = inner_pairs
			.next()
			.expect(&format!("Failed to parse a class declaration function: expected rule '{:?}', got nothing", Rule::function_definition_stmt));

		let function_statement = self.parse_function_declaration(function_pair);
		ClassDeclarationMember::function_from_statement(function_statement, attributes)
	}

	fn parse_class_declaration_variable(&self, pair: Pair<'p, Rule>) -> ClassDeclarationMember {
		if pair.as_rule() != Rule::class_declaration_variable {
			panic!("Failed to parse class declaration member: got an unexpected rule. Expected rule '{:?}', got '{:?}'", Rule::class_declaration_variable, pair.as_rule());
		}

		let mut inner_pairs = pair.into_inner();

		let name = inner_pairs
			.next()
			.expect(&format!("Failed to parse class declaration variable: expected rule '{:?}', got nothing", Rule::identifier));

		let kind = inner_pairs
			.next()
			.expect(&format!("Failed to parse class declaration variable: expected rule '{:?}', got nothing", Rule::r#type));

		ClassDeclarationMember::Variable {
			name: name.as_str().to_owned(),
			kind: self.parse_type(kind)
		}
	}

	fn parse_class_declaration_member(&self, pair: Pair<'p, Rule>) -> ClassDeclarationMember {
		if pair.as_rule() != Rule::class_declaration_member {
			panic!("Failed to parse class declaration member: got an unexpected rule. Expected rule '{:?}', got '{:?}'", Rule::class_declaration_member, pair.as_rule());
		}

		let inner = pair
			.into_inner()
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse class declaration member: pairs are empty"));

		match inner.as_rule() {
			Rule::class_declaration_variable => self.parse_class_declaration_variable(inner),
			Rule::class_declaration_function => self.parse_class_declaration_function(inner),
			
			rule => panic!(
				"Failed to parse class declaration member: expected '{:?}' or '{:?}', got '{:?}'",
				Rule::class_declaration_variable,
				Rule::class_declaration_function,
				rule
			)
		}
	}

	fn parse_class_declaration(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let name = pairs
			.next()
			.expect("Failed to parse class declaration: class name is missing");

		if name.as_rule() != Rule::identifier {
			panic!("Failed to parse class declaration: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::identifier, name.as_rule());
		}

		let next_pair_option = pairs.peek();
		let next_pair: Pair<Rule>;

		// if the class is empty, return early
		if next_pair_option.is_none() {
			return Statement::ClassDeclaration {
				name: name.as_str().to_owned(),
				generics: Vec::new(),
				members: Vec::new()
			};
		}

		// unwrap the next pair
		next_pair = next_pair_option.unwrap_or_else(|| unreachable!("Failed to parse class declaration: expected generics or class members, got nothing"));
		let mut generics: Vec<Expression> = Vec::new();

		// check if its definition_generics
		if next_pair.as_rule() == Rule::definition_generics {
			generics = self.parse_generics_as_identifiers(next_pair);

			// if no class members are provided, return early
			if pairs.peek().is_none() {
				return Statement::ClassDeclaration {
					name: name.as_str().to_owned(),
					generics,
					members: Vec::new()
				};
			}

			// otherwise, skip the current pair
			pairs.next();
		}

		let mut members: Vec<ClassDeclarationMember> = Vec::new();

		while let Some(pair) = pairs.next() {
			members.push(self.parse_class_declaration_member(pair));
		}

		Statement::ClassDeclaration {
			name: name.as_str().to_owned(),
			generics,
			members
		}
	}

	fn parse_variable_definition(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let name = pairs
			.next()
			.expect("Failed to parse variable definition: variable name is missing");

		let mut next_pair = pairs
			.next()
			.expect("Failed to parse variable definition: expected type/value, got nothing");

		let mut kind = Expression::Empty;

		#[allow(unused_assignments)]
		let mut value = Expression::Empty;

		if next_pair.as_rule() == Rule::r#type {
			kind = self.parse_type(next_pair);

			next_pair = pairs
				.next()
				.expect("Failed to parse variable definition: variable value is missing");
		}
		
		if next_pair.as_rule() == Rule::expression {
			value = self.parse_expression(next_pair);
		} else {
			panic!("Failed to parse variable definition: expected rule '{:?}', got '{:?}'", Rule::expression, next_pair.as_rule());
		}

		Statement::VariableDefinition {
			name: name.as_str().to_owned(),
			kind,
			value
		}
	}

	fn parse_variable_assign(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let mut pairs = pairs.clone();

		let variable_pair = pairs
			.next()
			.expect("Failed to parse variable assign: expected a variable name, got nothing");

		let value_pair = pairs
			.next()
			.expect("Failed to parse variable assign: expected an expression, got nothing");

		Statement::VariableAssign(
			self.parse_expression(variable_pair),
			self.parse_expression(value_pair)
		)
	}

	fn parse_function_call(&self, pairs: Pairs<'p, Rule>) -> Statement {
		let last_pair = pairs
			.clone()
			.last()
			.unwrap_or_else(|| unreachable!("Failed to parse function call statement: pairs are empty"));

		// check if the expression doesn't end with a function call "<...>(...)"
		if last_pair.as_rule() != Rule::recursive_call {
			panic!("Failed to parse function call statement: expression statement is not a function call");
		}

		let parsed = self.parse_recursive_expression(pairs);

		if let Expression::FunctionCall { callee, generics, arguments } = parsed {
			return Statement::FunctionCall {
				callee: *callee,
				generics,
				arguments
			};
		}

		panic!("Failed to parse function call statement: callee or arguments parameters are invalid");
	}

	fn parse_return(&self, pairs: Pairs<'p, Rule>) -> Statement {
		if pairs.len() > 0 {
			let expression_pair = pairs
				.peek()
				.expect("Failed to parse function call expression as a statement");

			return Statement::Return(self.parse_expression(expression_pair));
		}

		Statement::Return(Expression::Empty)
	}
}