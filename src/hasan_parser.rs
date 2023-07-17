use std::iter::Peekable;

use crate::pest_parser::Rule;

use pest::error::{Error, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::Span;

macro_rules! error {
	($self:ident, $msg:expr, $span:expr) => {
		panic!("{}", $self.create_error($msg, $span))
	};

	($self:ident, $msg:expr, $span:expr, $($var_args:expr),*) => {
		panic!("{}", $self.create_error(&format!($msg, $($var_args),*), $span))
	};
}

pub struct HasanParser<'p> {
	pairs: Pairs<'p, Rule>
}

#[derive(Debug)]
pub enum Statement<'p> {
	FunctionDefinition {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		generics: Vec<Expression<'p>>,
		arguments: Vec<FunctionArgument<'p>>,
		return_type: Option<Expression<'p>>, //* Expression::Type
		statements: Vec<Statement<'p>>,
		span: Span<'p>
	},

	FunctionDeclaration {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		generics: Vec<Expression<'p>>,
		arguments: Vec<FunctionArgument<'p>>,
		return_type: Option<Expression<'p>>, //* Expression::Type
		span: Span<'p>
	},

	TypeDefinition {
		name: Span<'p>,
		generics: Vec<Expression<'p>>,
		definition: Expression<'p>,
		span: Span<'p>
	},

	ClassDefinition {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		generics: Vec<Expression<'p>>,
		members: Vec<ClassDefinitionMember<'p>>,
		span: Span<'p>
	},

	ClassDeclaration {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		generics: Vec<Expression<'p>>,
		members: Vec<ClassDeclarationMember<'p>>,
		span: Span<'p>
	},

	VariableDefinition {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		kind: Expression<'p>, //* Expression::Type
		value: Expression<'p>,
		span: Span<'p>
	},

	VariableAssign {
		name: Expression<'p>,
		value: Expression<'p>,
		span: Span<'p>
	},

	FunctionCall {
		callee: Expression<'p>,
		generics: Vec<Expression<'p>>,
		arguments: Vec<Expression<'p>>,
		span: Span<'p>
	},

	Return {
		value: Expression<'p>,
		span: Span<'p>
	},

	EnumDefinition {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		variants: Vec<EnumVariant<'p>>,
		span: Span<'p>
	},

	If {
		condition: Expression<'p>,
		statements: Vec<Statement<'p>>,
		elseif_branches: Vec<ConditionBranch<'p>>,
		else_branch: Option<ConditionBranch<'p>>,
		span: Span<'p>
	},

	While {
		condition: Expression<'p>,
		statements: Vec<Statement<'p>>,
		span: Span<'p>
	},
	
	Break(Span<'p>),

	// special statements that are not intended to be used traditionally
	Unimplemented
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct ConditionBranch<'p> {
	condition: Expression<'p>,
	statements: Vec<Statement<'p>>,
	span: Span<'p>
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct EnumVariant<'p> {
	name: Span<'p>,
	span: Span<'p>
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct FunctionArgument<'p> {
	name: Expression<'p>, //* Expression::Identifier
	kind: Expression<'p>, //* Expression::Type
	span: Span<'p>
}

impl<'p> FunctionArgument<'p> {
	pub fn new(name: Expression<'p>, kind: Expression<'p>, span: Span<'p>) -> Self {
		match kind {
			Expression::Type { .. } => FunctionArgument { name, kind, span },
			_ => panic!("Got an unexpected 'kind' argument. Expected '{:?}', got '{:?}'", Rule::r#type, kind)
		}
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct ClassFunctionAttributes<'p> {
	attributes: Vec<ClassFunctionAttribute<'p>>,
	span: Span<'p>
}

#[derive(Debug)]
pub enum ClassFunctionAttribute<'p> {
	Constructor(Span<'p>),
	Get(Span<'p>),
	Set(Span<'p>)
}

impl<'p> ClassFunctionAttributes<'p> {
	fn new(span: Span<'p>) -> Self {
		ClassFunctionAttributes {
			attributes: Vec::new(),
			span
		}
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct GeneralModifiers<'p> {
	modifiers: Vec<GeneralModifier<'p>>,
	span: Span<'p>
}

#[derive(Debug)]
pub enum GeneralModifier<'p> {
	Public(Span<'p>),
	Static(Span<'p>)
}

impl<'p> GeneralModifiers<'p> {
	fn new(span: Span<'p>) -> Self {
		GeneralModifiers {
			modifiers: Vec::new(),
			span
		}
	}
}

#[derive(Debug)]
pub enum ClassDefinitionMember<'p> {
	Variable {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		kind: Expression<'p>, //* Expression::Type
		default_value: Expression<'p>,
		span: Span<'p>
	},

	Function {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		attributes: Option<ClassFunctionAttributes<'p>>,
		generics: Vec<Expression<'p>>,
		arguments: Vec<FunctionArgument<'p>>,
		return_type: Option<Expression<'p>>, //* Expression::Type
		statements: Vec<Statement<'p>>,
		span: Span<'p>
	}
}

impl<'p> ClassDefinitionMember<'p> {
	pub fn function_from_statement(statement: Statement<'p>, attributes: Option<ClassFunctionAttributes<'p>>) -> Self {
		if let Statement::FunctionDefinition {
			modifiers,
			name,
			generics,
			arguments,
			return_type,
			statements,
			span
		} = statement {
			return ClassDefinitionMember::Function {
				modifiers,
				name,
				attributes,
				generics,
				arguments,
				return_type,
				statements,
				span
			};
		} else {
			panic!("Failed to convert invalid statement into a ClassDefinitionMember::Function");
		}
	}
}

#[derive(Debug)]
pub enum ClassDeclarationMember<'p> {
	Variable {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		kind: Expression<'p>, //* Expression::Type
		span: Span<'p>
	},

	Function {
		modifiers: GeneralModifiers<'p>,
		name: Span<'p>,
		attributes: Option<ClassFunctionAttributes<'p>>,
		generics: Vec<Expression<'p>>,
		arguments: Vec<FunctionArgument<'p>>,
		return_type: Option<Expression<'p>>, //* Expression::Type
		span: Span<'p>
	}
}

impl<'p> ClassDeclarationMember<'p> {
	pub fn function_from_statement(statement: Statement<'p>, attributes: Option<ClassFunctionAttributes<'p>>) -> Self {
		if let Statement::FunctionDeclaration {
			modifiers,
			name,
			generics,
			arguments,
			return_type,
			span
		} = statement {
			return ClassDeclarationMember::Function {
				modifiers,
				name,
				attributes,
				generics,
				arguments,
				return_type,
				span
			};
		} else {
			panic!("Failed to convert invalid statement into a ClassDefinitionMember::Function");
		}
	}
}

type IntType = i64;
type FloatType = f64;

#[derive(Debug)]
pub enum Expression<'p> {
	Int(IntType, Span<'p>),
	Float(FloatType, Span<'p>),
	String(String, Span<'p>),
	Boolean(bool, Span<'p>),

	Unary {
		operator: UnaryOperator,
		operand: Box<Expression<'p>>,

		span: Span<'p>
	},

	Binary {
		lhs: Box<Expression<'p>>,
		operator: BinaryOperator,
		rhs: Box<Expression<'p>>,

		span: Span<'p>
	},

	FunctionCall {
		callee: Box<Expression<'p>>,
		generics: Vec<Expression<'p>>,
		arguments: Vec<Expression<'p>>,

		span: Span<'p>
	},

	ArrayAccess {
		expression: Box<Expression<'p>>,
		accessor: Box<Expression<'p>>,

		span: Span<'p>
	},

	DotAccess {
		expression: Box<Expression<'p>>,
		accessor: Box<Expression<'p>>,

		span: Span<'p>
	},

	ArrowAccess {
		expression: Box<Expression<'p>>,
		accessor: Box<Expression<'p>>,

		span: Span<'p>
	},

	Array(Vec<Expression<'p>>, Span<'p>),
	Identifier(Span<'p>),

	Type {
		base: Box<Expression<'p>>,
		generics: Vec<Expression<'p>>,

		// attributes
		array: bool,
		raw: bool,

		span: Span<'p>
	},

	TypeCast {
		value: Box<Expression<'p>>,
		kind: Box<Expression<'p>>,

		span: Span<'p>
	},

	AnonymousFunction {
		generics: Vec<Expression<'p>>,
		arguments: Vec<FunctionArgument<'p>>,
		return_type: Box<Option<Expression<'p>>>,
		statements: Vec<Statement<'p>>,

		span: Span<'p>
	},

	Empty,
	Unimplemented
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum UnaryOperator {
	Minus,
	Not
}

impl<'p> HasanParser<'p> {
	pub fn new(pairs: Pairs<'p, Rule>) -> Self {
		HasanParser { pairs }
	}

	fn create_error(&self, message: &str, span: Span<'p>) -> Error<Rule> {
		Error::new_from_span(
			ErrorVariant::CustomError { message: message.to_owned() },
			span
		)
	}

	pub fn parse(&self) -> Vec<Statement> {
		let mut statements: Option<Vec<Statement>> = None;

		for pair in self.pairs.clone() {
			match pair.as_rule() {
				Rule::COMMENT |
				Rule::WHITESPACE |
				Rule::line_comment |
				Rule::block_comment => (),

				Rule::program => statements = Some(self.parse_program(pair.into_inner())),

				rule => error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::program, rule)
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
				Rule::function_definition_stmt => self.parse_function_definition(pair),
				Rule::function_declaration_stmt => self.parse_function_declaration(pair),
				Rule::type_definition_stmt => self.parse_type_definition(pair),
				Rule::class_definition => self.parse_class_definition(pair),
				Rule::class_declaration => self.parse_class_declaration(pair),
				Rule::variable_definition_stmt => self.parse_variable_definition(pair),
				Rule::variable_assign_stmt => self.parse_variable_assign(pair),
				Rule::function_call_stmt => self.parse_function_call(pair),
				Rule::return_stmt => self.parse_return(pair),
				Rule::enum_definition_stmt => self.parse_enum_definition(pair),

				Rule::if_stmt => self.parse_if(pair),
				Rule::while_stmt => self.parse_while(pair),
				Rule::break_stmt => Statement::Break(pair.as_span()),

				rule => error!(self, "unexpected statement '{:?}'", pair.as_span(), rule)
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

			operator => error!(self, "expected '+', '-', '/', '*', '%', '==', '!=', 'and', 'or', '>', '<', '>=' or '<=', got '{}'", pair.as_span(), operator)
		}
	}

	fn parse_unary_operator(&self, pair: &Pair<'p, Rule>) -> UnaryOperator {
		match pair.as_str() {
			"-" => UnaryOperator::Minus,
			"not" => UnaryOperator::Not,

			operator => error!(self, "expected '-' or 'not', got '{}'", pair.as_span(), operator)
		}
	}

	fn parse_expression(&self, expression_pair: Pair<'p, Rule>) -> Expression {
		let mut pairs = expression_pair
			.clone()
			.into_inner()
			.peekable();

		let span = expression_pair.as_span();

		// check if an iterator is empty
		if pairs.len() < 1 {
			return self.parse_term(expression_pair);
		}

		if expression_pair.as_rule() == Rule::recursive_expression {
			return self.parse_recursive_expression(expression_pair.into_inner());
		}

		self.parse_expression_with_precedence(&mut pairs, span, 0)
	}

	fn parse_expression_with_precedence(&self, pairs: &mut Peekable<Pairs<'p, Rule>>, span: Span<'p>, precedence: u8) -> Expression {
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
				
				// consume the operator and get the current end position at the same time
				let rhs_start = pairs
					.next()
					.unwrap_or_else(|| unreachable!("Failed to parse expression: expected a pair, got nothing"))
					.as_span()
					.end_pos();

				let rhs_span = Span::new(
					self.pairs.as_str(),
					rhs_start.pos(),
					span.end()
				).expect("Failed to parse expression: failed to create span");

				// TODO: fix spans

				let right = self.parse_expression_with_precedence(pairs, rhs_span, operator_precedence + 1);
	
				left = Expression::Binary {
					lhs: Box::new(left),
					operator,
					rhs: Box::new(right),
					span
				};
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
			
			operator => error!(self, "expected '+', '-', '/', '*', '%', '==', '!=', 'and', or 'or', got '{}'", pair.as_span(), operator)
		}
	}

	fn parse_identifier(&self, pair: Pair<'p, Rule>) -> Expression {
		if pair.as_rule() != Rule::identifier {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::identifier, pair.as_rule());
		}

		Expression::Identifier(pair.as_span())
	}

	fn parse_number_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();
		let string = pair.as_str().to_owned();

		let literal = match string.parse::<IntType>() {
			Ok(i) => Expression::Int(i, span),
			Err(_) => match string.parse::<FloatType>() {
				Ok(f) => Expression::Float(f, span),
				Err(_) => error!(self, "failed to parse number literal '{}'", pair.as_span(), string),
			},
		};

		literal
	}

	fn parse_string_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let literal = pair.as_str().to_owned();
		let clean_literal = literal.trim_start_matches(&['\'', '\"'][..]).trim_end_matches(&['\'', '\"'][..]);

		Expression::String(clean_literal.to_owned(), pair.as_span())
	}

	fn parse_boolean_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		let literal = pair.as_str();

		match literal {
			"true" => Expression::Boolean(true, pair.as_span()),
			"false" => Expression::Boolean(false, pair.as_span()),

			_ => error!(self, "expected 'true' or 'false', got '{}'", pair.as_span(), literal)
		}
	}

	fn parse_term(&self, pair: Pair<'p, Rule>) -> Expression {
		match pair.as_rule() {
			Rule::anonymous_function => self.parse_anonymous_function(pair),
			Rule::unary_expression => self.parse_unary_expression(pair),
			Rule::binary_expression | Rule::expression => self.parse_expression(pair),

			Rule::array_expression => self.parse_array_expression(pair),
			Rule::recursive_expression => self.parse_recursive_expression(pair.into_inner()),

			Rule::number_literal |
			Rule::string_literal |
			Rule::boolean_literal => self.parse_literal(pair),

			Rule::identifier => self.parse_identifier(pair),
			Rule::r#type => self.parse_type(pair),

			rule => error!(self, "invalid expression rule '{:?}'", pair.as_span(), rule)
		}
	}

	fn parse_anonymous_function(&self, pair: Pair<'p, Rule>) -> Expression {
		if pair.as_rule() != Rule::anonymous_function {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::anonymous_function, pair.as_rule());
		}

		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let mut generics: Vec<Expression> = Vec::new();
		let mut arguments: Vec<FunctionArgument> = Vec::new();
		let mut statements: Vec<Statement> = Vec::new();
		let mut return_type: Option<Expression> = None;

		while let Some(pair) = pairs.next() {
			match pair.as_rule() {
				Rule::do_block => statements = self.parse_program(pair.into_inner()),
				Rule::r#type => return_type = Some(self.parse_type(pair)),
				Rule::function_arguments => arguments = self.parse_function_arguments(pair),
				Rule::definition_generics => generics = self.parse_generics_as_identifiers(pair),

				rule => error!(
					self,
					"expected '{:?}', '{:?}', '{:?}' or '{:?}', got '{:?}'",
					pair.as_span(),

					Rule::do_block,
					Rule::r#type,
					Rule::function_arguments,
					Rule::definition_generics,
					rule
				)
			};
		}

		Expression::AnonymousFunction {
			generics,
			arguments,
			return_type: Box::new(return_type),
			statements,
			span
		}
	}

	fn parse_elseif_branch(&self, pair: Pair<'p, Rule>) -> ConditionBranch {
		if pair.as_rule() != Rule::if_elseif {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::if_elseif, pair.as_rule());
		}

		let span = pair.as_span();
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

		ConditionBranch {
			condition: self.parse_expression(expression_pair),
			statements: self.parse_program(statements_pair.into_inner()),
			span
		}
	}

	fn parse_else_branch(&self, pair: Pair<'p, Rule>) -> ConditionBranch {
		if pair.as_rule() != Rule::if_else {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::if_else, pair.as_rule());
		}

		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		if pairs.len() < 1 {
			panic!("Failed to parse else branch: pairs are empty");
		}

		let statements_pair = pairs
			.next()
			.expect("Failed to parse else branch: expected statements, got nothing");

		ConditionBranch {
			condition: Expression::Empty,
			statements: self.parse_program(statements_pair.into_inner()),
			span
		}
	}

	fn parse_if(&self, pair: Pair<'p, Rule>) -> Statement {
		if pair.as_rule() != Rule::if_stmt {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::if_stmt, pair.as_rule());
		}

		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		if pairs.len() < 1 {
			panic!("Failed to parse if statement: pairs are empty");
		}

		let condition_pair = pairs
			.next()
			.expect("Failed to parse if statement: condition is missing");

		let statements_pair = pairs
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse if statement: statements are missing"));

		let mut elseif_branches: Vec<ConditionBranch> = Vec::new();
		let mut else_branch: Option<ConditionBranch> = None;

		while let Some(pair) = pairs.next() {
			if !matches!(pair.as_rule(), Rule::if_elseif | Rule::if_else) {
				error!(self, "expected '{:?}' or '{:?}', got '{:?}'", pair.as_span(), Rule::if_elseif, Rule::if_else, pair.as_rule());
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
			else_branch,
			span
		}
	}

	fn parse_while(&self, pair: Pair<'p, Rule>) -> Statement {
		if pair.as_rule() != Rule::while_stmt {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::while_stmt, pair.as_rule());
		}

		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let expression_pair = pairs
			.next()
			.expect("Failed to parse while statement: expected expression, got nothing");

		let statements_pair = pairs
			.next()
			.expect("Failed to parse while statement: expected statements, got nothing");

		Statement::While {
			condition: self.parse_expression(expression_pair),
			statements: self.parse_program(statements_pair.into_inner()),
			span
		}
	}

	fn parse_unary_expression(&self, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let operator = self.parse_unary_operator(&pairs.next().expect("Failed to parse unary expression: no operator is present"));
		let operand = self.parse_expression(pairs.next().expect("Failed to parse unary expression: no operand is present"));

		Expression::Unary {
			operator,
			operand: Box::new(operand),
			span
		}
	}

	fn parse_literal(&self, pair: Pair<'p, Rule>) -> Expression {
		match pair.as_rule() {
			Rule::number_literal => self.parse_number_literal(pair),
			Rule::string_literal => self.parse_string_literal(pair),
			Rule::boolean_literal => self.parse_boolean_literal(pair),

			rule => error!(self, "expected number or string literal, got '{:?}'", pair.as_span(), rule)
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

				rule => error!(self, "expected a recursive expression term, got '{:?}'", pair.as_span(), rule)
			}
		}

		current_expression
	}

	fn parse_function_call_expression(&self, expression: Expression<'p>, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();
		let mut call_insides = pair.into_inner();

		if call_insides.len() < 1 {
			return Expression::FunctionCall {
				callee: Box::new(expression),
				generics: Vec::new(),
				arguments: Vec::new(),
				span
			};
		}

		let mut next_pair = call_insides
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse function call expression: call pairs iterator is empty"));

		let mut generics: Vec<Expression> = Vec::new();
		let mut arguments: Vec<Expression> = Vec::new();

		// notify that incorrect generics are being passed to the function
		if next_pair.as_rule() == Rule::definition_generics {
			error!(self, "definition generics were passed instead of call generics", next_pair.as_span());
		}

		// if the next pair is of type call_generics, parse them, and exit if there are no arguments
		if next_pair.as_rule() == Rule::call_generics {
			generics = self.parse_generics_as_types(next_pair);

			let pair = call_insides.next();
			
			if pair.is_none() {
				return Expression::FunctionCall {
					callee: Box::new(expression),
					generics,
					arguments,
					span
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
			arguments,
			span
		}
	}

	fn parse_arrow_access_expression(&self, expression: Expression<'p>, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();
		
		Expression::ArrowAccess {
			expression: Box::new(expression),
			accessor: Box::new(self.parse_expression(pair)),
			span
		}
	}

	fn parse_dot_access_expression(&self, expression: Expression<'p>, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();
		
		Expression::DotAccess {
			expression: Box::new(expression),
			accessor: Box::new(self.parse_expression(pair)),
			span
		}
	}

	fn parse_array_access_expression(&self, expression: Expression<'p>, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();
		
		Expression::ArrayAccess {
			expression: Box::new(expression),
			accessor: Box::new(self.parse_expression(pair)),
			span
		}
	}

	fn parse_type_cast_expression(&self, expression: Expression<'p>, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();
		let kind_pair = pair
			.into_inner()
			.next()
			.unwrap_or_else(|| unreachable!("Failed to parse type cast expression: pairs are empty"));

		let kind_parsed = Box::new(self.parse_type(kind_pair));

		Expression::TypeCast {
			value: Box::new(expression),
			kind: kind_parsed,
			span
		}
	}

	fn parse_array_expression(&self, pair: Pair<'p, Rule>) -> Expression {
		let span = pair.as_span();

		let mut pairs = pair.into_inner();
		let mut items: Vec<Expression> = Vec::new();

		while let Some(pair) = pairs.next() {
			let parsed_pair = self.parse_expression(pair);
			items.push(parsed_pair);
		}

		Expression::Array(items, span)
	}

	/// Used for **definition** statements. Parses generics **as identifiers** to later be substituted with proper types
	/// 
	/// # Arguments
	/// 
	/// * `pair` - A Pest.rs parser pair with type Rule::definition_generics
	fn parse_generics_as_identifiers(&self, pair: Pair<'p, Rule>) -> Vec<Expression> {
		if pair.as_rule() != Rule::definition_generics {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::definition_generics, pair.as_rule());
		}

		let inner_pairs = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new(); //* Expression::Identifier only

		for arg in inner_pairs {
			if arg.as_rule() != Rule::identifier {
				error!(self, "expected '{:?}', got '{:?}'", arg.as_span(), Rule::identifier, arg.as_rule());
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
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::call_generics, pair.as_rule());
		}

		let inner_pairs = pair.into_inner();
		let mut generics: Vec<Expression> = Vec::new();

		for arg in inner_pairs {
			if arg.as_rule() != Rule::r#type {
				error!(self, "expected '{:?}', got '{:?}'", arg.as_span(), Rule::r#type, arg.as_rule());
			}

			generics.push(self.parse_type(arg));
		}

		generics
	}

	fn parse_enum_variant(&self, pair: Pair<'p, Rule>) -> EnumVariant {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let name = pairs
			.next()
			.expect("Failed to parse enum variant: name is missing")
			.as_span();

		EnumVariant { name, span }
	}

	fn parse_enum_definition(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let modifiers_pair = pairs
			.next()
			.expect("Failed to parse enum definition: modifiers are missing");

		let modifiers = self.parse_general_modifiers(modifiers_pair);

		let name = pairs
			.next()
			.expect("Failed to parse enum definition: enum name is missing")
			.as_span();

		let mut variants: Vec<EnumVariant> = Vec::new();

		while let Some(pair) = pairs.next() {
			if pair.as_rule() != Rule::enum_variant {
				error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::enum_variant, pair.as_rule());
			}

			variants.push(self.parse_enum_variant(pair));
		}

		Statement::EnumDefinition { modifiers, name, variants, span }
	}

	fn parse_type(&self, pair: Pair<'p, Rule>) -> Expression {
		if pair.as_rule() != Rule::r#type {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::r#type, pair.as_rule());
		}

		let span = pair.as_span();
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
			base: Box::new(Expression::Identifier(name_pair.as_span())),
			generics,
			array: is_array_type,
			raw: is_raw_type,
			span
		}
	}

	fn parse_class_function_attributes(&self, pair: Pair<'p, Rule>) -> ClassFunctionAttributes {
		if pair.as_rule() != Rule::attributes {
			panic!("Failed to parse function attributes: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::attributes, pair.as_rule());
		}

		let span = pair.as_span();
		let mut inner = pair.into_inner();

		let mut attributes = ClassFunctionAttributes::new(span);
		let mut met_attributes: Vec<String> = Vec::new(); // keep track of which attributes have already been defined to prevent users from defining them twice

		while let Some(pair) = inner.next() {
			let as_str = pair.as_str();
			let span = pair.as_span();

			let owned_str = as_str.clone().to_owned();

			if met_attributes.contains(&owned_str) {
				error!(self, "Found more than one '{}' attribute definition", span, as_str);
			}

			use ClassFunctionAttribute::*;

			let attribute = match as_str {
				"constructor" => Constructor(span),
				"get" => Get(span),
				"set" => Set(span),

				_ => unreachable!("Failed to parse function attributes: unknown attribute '{}'", as_str)
			};

			attributes.attributes.push(attribute);

			// mark attribute as defined
			met_attributes.push(owned_str);
		}

		attributes
	}

	fn parse_function_arguments(&self, pair: Pair<'p, Rule>) -> Vec<FunctionArgument> {
		if pair.as_rule() != Rule::function_arguments {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::function_arguments, pair.as_rule());
		}

		// not needed yet
		// let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let mut arguments: Vec<FunctionArgument> = Vec::new();

		while let Some(pair) = pairs.next() {
			if pair.as_rule() != Rule::function_argument {
				error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::function_argument, pair.as_rule());
			}

			let arg_span = pair.as_span();
			let mut arg_pairs = pair.into_inner();

			let name_pair = arg_pairs
				.next()
				.expect("Failed to parse function definition arguments: argument name is missing");

			let kind_pair = arg_pairs
				.next()
				.expect("Failed to parse function definition arguments: argument type is missing");

			arguments.push(FunctionArgument {
				name: self.parse_identifier(name_pair),
				kind: self.parse_type(kind_pair),
				span: arg_span
			});
		}

		arguments
	}

	fn parse_general_modifiers(&self, pair: Pair<'p, Rule>) -> GeneralModifiers {
		if pair.as_rule() != Rule::general_modifiers {
			error!(self, "expected '{:?}', got '{:?}'", pair.as_span(), Rule::general_modifiers, pair.as_rule());
		}

		let mut modifiers = GeneralModifiers::new(pair.as_span());
		let mut pairs = pair.into_inner();

		let mut met_modifiers: Vec<String> = Vec::new();

		while let Some(pair) = pairs.next() {
			let as_str = pair.as_str();
			let span = pair.as_span();

			let owned_str = as_str.clone().to_owned();

			if met_modifiers.contains(&owned_str) {
				error!(self, "found more than one '{}' modifier definition", span, as_str);
			}

			use GeneralModifier::*;

			let modifier = match as_str {
				"pub" => Public(span),
				"static" => Static(span),

				_ => unreachable!("Failed to parse modifiers: unknown modifier '{}'", as_str)
			};

			modifiers.modifiers.push(modifier);

			// mark modifier as defined
			met_modifiers.push(owned_str);
		}

		modifiers
	}

	fn parse_function_header(&self, pair: Pair<'p, Rule>) -> (GeneralModifiers, Span, Vec<Expression>, Vec<FunctionArgument>, Option<Expression>) {
		let mut header_pairs = pair.into_inner();

		let modifiers_pair = header_pairs
			.next()
			.expect("Failed to parse function header: modifiers are missing");

		let modifiers = self.parse_general_modifiers(modifiers_pair);

		let name = header_pairs
			.next()
			.expect("Failed to parse function header: function name is missing");

		let mut generics: Vec<Expression> = Vec::new();
		let mut arguments: Vec<FunctionArgument> = Vec::new();
		let mut return_type: Option<Expression> = None;

		while let Some(pair) = header_pairs.next() {
			match pair.as_rule() {
				Rule::definition_generics => generics = self.parse_generics_as_identifiers(pair),
				Rule::function_arguments => arguments = self.parse_function_arguments(pair),
				Rule::r#type => return_type = Some(self.parse_type(pair)),

				rule => error!(self, "unexpected rule '{:?}'", pair.as_span(), rule)
			}
		}

		(modifiers, name.as_span(), generics, arguments, return_type)
	}

	fn parse_function_declaration(&self, pair: Pair<'p, Rule>) -> Statement {
		if pair.as_rule() != Rule::function_declaration_stmt {
			panic!("Failed to parse function declaration: got an unexpected rule. Expected '{:?}', got '{:?}'", Rule::function_declaration_stmt, pair.as_rule());
		}

		let span = pair.as_span();

		let header_pair = pair
			.into_inner()
			.next()
			.expect("Failed to parse function declaration: function header is missing");

		let (modifiers, name, generics, arguments, return_type) = self.parse_function_header(header_pair);
		Statement::FunctionDeclaration { modifiers, name, generics, arguments, return_type, span }
	}

	fn parse_function_definition(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let header_pair = pairs
			.next()
			.expect("Failed to parse function definition: function header is missing");

		// parsing header
		let (modifiers, name, generics, arguments, return_type) = self.parse_function_header(header_pair);

		let body_pairs = pairs
			.next()
			.expect("Failed to parse function definition: function body is missing")
			.into_inner();

		Statement::FunctionDefinition {
			modifiers,
			name,
			generics,
			arguments,
			return_type,
			statements: self.parse_program(body_pairs),
			span
		}
	}

	fn parse_type_definition(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

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
			name: name_pair.as_span(),
			generics,
			definition: type_expression,
			span
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
		
		let mut attributes: Option<ClassFunctionAttributes> = None;

		if next_pair.as_rule() == Rule::attributes {
			attributes = Some(self.parse_class_function_attributes(next_pair));

			// skip attributes if they exist
			inner_pairs.next();
		}

		let statement_pair = inner_pairs
			.next()
			.expect(&format!("Failed to parse a class definition function: expected rule '{:?}', got nothing", Rule::function_definition_stmt));

		let function_statement = self.parse_function_definition(statement_pair);
		ClassDefinitionMember::function_from_statement(function_statement, attributes)
	}

	fn parse_class_definition_variable(&self, pair: Pair<'p, Rule>) -> ClassDefinitionMember {
		if pair.as_rule() != Rule::class_definition_variable {
			panic!("Failed to parse class definiton variable: expected rule '{:?}', got '{:?}'", Rule::class_definition_variable, pair.as_rule());
		}

		let span = pair.as_span();
		let mut inner_pairs = pair.into_inner();

		let modifiers_pair = inner_pairs
			.next()
			.expect(&format!("Failed to parse class definition variable: expected '{:?}', got nothing", Rule::general_modifiers));

		let modifiers = self.parse_general_modifiers(modifiers_pair);

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
			modifiers,
			name: name.as_span(),
			kind: self.parse_type(kind),
			default_value,
			span
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

			rule => error!(
				self,
				"expected '{:?}' or '{:?}', got '{:?}'",
				inner.as_span(),
				Rule::class_definition_variable,
				Rule::class_definition_function,
				rule
			)
		}
	}

	fn parse_class_definition(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let modifiers_pair = pairs
			.next()
			.expect("Failed to parse class definition: modifiers are missing");

		let modifiers = self.parse_general_modifiers(modifiers_pair);

		let name = pairs
			.next()
			.expect("Failed to parse class definition: class name is missing");

		if name.as_rule() != Rule::identifier {
			error!(self, "expected '{:?}', got '{:?}'", name.as_span(), Rule::identifier, name.as_rule());
		}

		let next_pair_option = pairs.peek();
		let next_pair: Pair<Rule>;

		// if the class is empty, return early
		if next_pair_option.is_none() {
			return Statement::ClassDefinition {
				modifiers,
				name: name.as_span(),
				generics: Vec::new(),
				members: Vec::new(),
				span
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
					modifiers,
					name: name.as_span(),
					generics,
					members: Vec::new(),
					span
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
			modifiers,
			name: name.as_span(),
			generics,
			members,
			span
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
		
		let mut attributes: Option<ClassFunctionAttributes> = None;

		if next_pair.as_rule() == Rule::attributes {
			attributes = Some(self.parse_class_function_attributes(next_pair));

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

		let span = pair.as_span();
		let mut inner_pairs = pair.into_inner();

		let modifiers_pair = inner_pairs
			.next()
			.expect(&format!("Failed to parse class declaration variable: expected rule '{:?}', got nothing", Rule::general_modifiers));

		let modifiers = self.parse_general_modifiers(modifiers_pair);

		let name = inner_pairs
			.next()
			.expect(&format!("Failed to parse class declaration variable: expected rule '{:?}', got nothing", Rule::identifier));

		let kind = inner_pairs
			.next()
			.expect(&format!("Failed to parse class declaration variable: expected rule '{:?}', got nothing", Rule::r#type));

		ClassDeclarationMember::Variable {
			modifiers,
			name: name.as_span(),
			kind: self.parse_type(kind),
			span
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

			rule => error!(
				self,
				"expected '{:?}' or '{:?}', got '{:?}'",
				inner.as_span(),
				Rule::class_declaration_variable,
				Rule::class_declaration_function,
				rule
			)
		}
	}

	fn parse_class_declaration(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let modifiers_pair = pairs
			.next()
			.expect(&format!("Failed to parse class declaration variable: expected rule '{:?}', got nothing", Rule::general_modifiers));

		let modifiers = self.parse_general_modifiers(modifiers_pair);

		let name = pairs
			.next()
			.expect("Failed to parse class declaration: class name is missing");

		if name.as_rule() != Rule::identifier {
			error!(self, "expected '{:?}', got '{:?}'", name.as_span(), Rule::identifier, name.as_rule());
		}

		let next_pair_option = pairs.peek();
		let next_pair: Pair<Rule>;

		// if the class is empty, return early
		if next_pair_option.is_none() {
			return Statement::ClassDeclaration {
				modifiers,
				name: name.as_span(),
				generics: Vec::new(),
				members: Vec::new(),
				span
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
					modifiers,
					name: name.as_span(),
					generics,
					members: Vec::new(),
					span
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
			modifiers,
			name: name.as_span(),
			generics,
			members,
			span
		}
	}

	fn parse_variable_definition(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let modifiers_pair = pairs
			.next()
			.expect(&format!("Failed to parse class declaration variable: expected rule '{:?}', got nothing", Rule::general_modifiers));

		let modifiers = self.parse_general_modifiers(modifiers_pair);

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
			error!(self, "expected expression, got '{:?}'", next_pair.as_span(), next_pair.as_rule());
		}

		Statement::VariableDefinition {
			modifiers,
			name: name.as_span(),
			kind,
			value,
			span
		}
	}

	fn parse_variable_assign(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let mut pairs = pair.into_inner();

		let variable_pair = pairs
			.next()
			.expect("Failed to parse variable assign: expected a variable name, got nothing");

		let value_pair = pairs
			.next()
			.expect("Failed to parse variable assign: expected an expression, got nothing");

		Statement::VariableAssign {
			name: self.parse_expression(variable_pair),
			value: self.parse_expression(value_pair),
			span
		}
	}

	fn parse_function_call(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let pairs = pair.into_inner();

		let last_pair = pairs
			.clone()
			.last()
			.unwrap_or_else(|| unreachable!("Failed to parse function call statement: pairs are empty"));

		// check if the expression doesn't end with a function call "<...>(...)"
		if last_pair.as_rule() != Rule::recursive_call {
			error!(self, "expression statement is not a function call", span);
		}

		let parsed = self.parse_recursive_expression(pairs);

		if let Expression::FunctionCall {
			callee,
			generics,
			arguments,
			span
		} = parsed {
			return Statement::FunctionCall {
				callee: *callee,
				generics,
				arguments,
				span
			};
		}

		panic!("Failed to parse function call statement: callee or arguments parameters are invalid");
	}

	fn parse_return(&self, pair: Pair<'p, Rule>) -> Statement {
		let span = pair.as_span();
		let pairs = pair.into_inner();

		if pairs.len() > 0 {
			let expression_pair = pairs
				.peek()
				.expect("Failed to parse function call expression as a statement");

			return Statement::Return {
				value: self.parse_expression(expression_pair),
				span
			};
		}

		Statement::Return {
			value: Expression::Empty,
			span
		}
	}
}