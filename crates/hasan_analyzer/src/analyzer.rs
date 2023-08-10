mod built_ins;
mod scope;
mod symbol;

use built_ins::*;
use scope::*;
use symbol::*;

use anyhow::{Error, bail};

use hasan_parser as p;
use hasan_hir as hir;

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	pub scope: Scope,
	ast: p::Program
}

impl SemanticAnalyzer {
	pub fn new(ast: p::Program) -> Self {
		Self {
			scope: Scope::new(),
			ast
		}
	}

	pub fn analyze(&mut self) -> Result<hir::Program, Error> {
		let mut converted_ast: hir::Program = hir::Program::default();

		for statement in self.ast.statements.clone() {
			let converted = self.analyze_statement(statement)?;
			converted_ast.statements.push(converted);
		}

		Ok(converted_ast)
	}

	fn analyze_statement(&mut self, statement: p::Statement) -> Result<hir::Statement, Error> {
		use p::Statement::*;
		
		match statement {
			VariableDefinition { modifiers, name, kind, value } =>
				self.analyze_variable_definition(modifiers, name, kind, value),
			
			FunctionDefinition(_) |
			FunctionDeclaration(_) => self.analyze_function_stmt(statement),

			ClassDefinition { modifiers, name, generics, members } =>
				self.analyze_class_definition(modifiers, name, generics, members),

			_ => bail!("Unsupported statement `{}`", statement.to_string())
		}
	}

	fn type_from_expression(&self, expression: &p::Expression) -> Result<hir::TypeRef, Error> {
		// TODO
		todo!()
	}

	fn convert_type(&self, kind: &p::Type) -> Result<hir::TypeRef, Error> {
		match kind.to_owned() {
			p::Type::Regular(kind) => {
				// TODO: Recursively resolve type aliases

				// TODO: Generics
				if !kind.generics.is_empty() {
					bail!("Generics are not yet supported");
				}

				let mut dimensions = 0;

				if kind.array {
					dimensions += 1;
				}

				let symbol = self.scope.get_symbol(&kind.name)?;

				if !symbol.is_class() {
					bail!("Symbol `{}` is not a type", kind.name);
				}

				Ok(hir::TypeRef(symbol.as_class()?, dimensions))
			},

			p::Type::Function(kind) => {
				// TODO
				todo!("function type resolving")
			}
		}
	}

	fn analyze_variable_definition(
		&mut self,
		modifiers: p::GeneralModifiers,
		name: String,
		kind: Option<p::Type>,
		value: p::Expression
	) -> Result<hir::Statement, Error> {
		use p::GeneralModifier::*;

		if !self.scope.flags.in_function {
			bail!("Cannot define a variable outside of a function");
		}
		
		let m_public = modifiers.contains(&Public);
		let m_const = modifiers.contains(&Constant);
		let m_static = modifiers.contains(&Static);

		if m_public && self.ast.module_info.is_none() {
			bail!("`pub` modifiers are not permitted outside of modules");
		}

		if m_static {
			bail!("`static` modifiers are not permitted outside of classes");
		}

		let kind_resolved = self.type_from_expression(&value)?;

		if let Some(kind_given) = kind {
			let kind_given = self.convert_type(&kind_given)?;

			if kind_given != kind_resolved {
				bail!("Mismatched types for variable `{}`: expected `{}` but `{}` was provided", name, kind_resolved.to_string(), kind_given.to_string());
			}
		}

		let variable = hir::Variable {
			name: name.clone(),
			kind: kind_resolved,
			value,

			is_constant: m_const
		};

		self.scope.insert_symbol(name, Symbol::Variable(variable.clone()))?;

		Ok(hir::Statement::VariableDefinition(variable))
	}

	fn convert_function_argument(&mut self, argument: p::FunctionArgument) -> Result<hir::FunctionArgument, Error> {
		let resolved_type = self.convert_type(&argument.kind)?;

		Ok(hir::FunctionArgument {
			name: argument.name,
			kind: resolved_type
		})
	}

	fn analyze_function_prototype(&mut self, prototype: p::FunctionPrototype) -> Result<hir::FunctionPrototype, Error> {
		use p::GeneralModifier::*;
		
		let p::FunctionPrototype {
			modifiers,
			name,
			generics,
			arguments,
			return_type
		} = prototype;

		let m_public = modifiers.contains(&Public);
		let m_const = modifiers.contains(&Constant);
		let m_static = modifiers.contains(&Static);

		if m_public && self.ast.module_info.is_none() {
			bail!("`pub` modifiers are not permitted outside of modules");
		}

		if m_const {
			bail!("`const` modifiers are not permitted in function prototypes");
		}

		if m_static {
			bail!("`static` modifiers are not permitted outside of classes");
		}

		// TODO: Generics
		if !generics.is_empty() {
			bail!("Generics are not yet supported");
		}

		let arguments = {
			let mut result = vec![];

			for argument in arguments {
				result.push(self.convert_function_argument(argument)?);
			}

			result
		};

		// TODO: Attempt to infer the return type

		if return_type.is_none() {
			bail!("Failed to infer the return type of function `{}`", name);
		}

		let return_type = self.convert_type(&return_type.unwrap())?;

		Ok(hir::FunctionPrototype {
			name,
			arguments,
			return_type
		})
	}

	fn analyze_function(&mut self, function: p::Function) -> Result<hir::Function, Error> {
		let prototype = self.analyze_function_prototype(function.prototype)?;

		let body: Option<Vec<hir::Statement>> = if let Some(func_body) = function.body {
			let original_scope = self.scope.clone();
			let mut converted = vec![];

			let mut new_scope = original_scope.create_child_scope();
			new_scope.flags.in_function = true;
			new_scope.flags.global = false;

			self.scope = new_scope;

			for statement in func_body {
				converted.push(self.analyze_statement(statement)?);
			}

			self.scope = original_scope;
			Some(converted)
		} else {
			None
		};

		Ok(hir::Function {
			prototype,
			body
		})
	}

	fn analyze_function_stmt(&mut self, statement: p::Statement) -> Result<hir::Statement, Error> {
		let function = match statement {
			p::Statement::FunctionDefinition(function) |
			p::Statement::FunctionDeclaration(function) => function,

			_ => unreachable!()
		};

		let function = self.analyze_function(function)?;

		if function.body.is_none() {
			return Ok(hir::Statement::FunctionDeclaration(function));
		}

		Ok(hir::Statement::FunctionDefinition(function))
	}

	fn analyze_class_member(&mut self, member: p::ClassMember) -> Result<hir::ClassMember, Error> {
		// TODO: Implement this function
		todo!()
	}

	fn analyze_class_definition(
		&mut self,
		modifiers: p::GeneralModifiers,
		name: String,
		generics: Vec<p::DefinitionType>,
		members: Vec<p::ClassMember>
	) -> Result<hir::Statement, Error> {
		use p::GeneralModifier::*;

		let m_public = modifiers.contains(&Public);
		let m_const = modifiers.contains(&Constant);
		let m_static = modifiers.contains(&Static);

		if m_public && self.ast.module_info.is_none() {
			bail!("`pub` modifiers are not permitted outside of modules");
		}

		if m_const {
			bail!("`const` modifiers are not permitted in class definitions");
		}

		if m_static {
			bail!("`static` modifiers are not permitted in class definitions")
		}

		// TODO: Generics
		if !generics.is_empty() {
			bail!("Generics are not yet supported");
		}

		let members = {
			let original_scope = self.scope.clone();
			let mut converted: Vec<hir::ClassMember> = vec![];

			let mut new_scope = original_scope.create_child_scope();
			new_scope.flags.in_class = true;

			for member in members {
				converted.push(self.analyze_class_member(member)?);
			}

			converted
		};

		let class = hir::Class {
			name: name.clone(),
			members,
			implements_interfaces: vec![]
		};

		self.scope.insert_symbol(name, Symbol::Class(class.clone()))?;
		Ok(hir::Statement::ClassDefinition(class))
	}
}