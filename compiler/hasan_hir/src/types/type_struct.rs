use crate::{
	ClassMember, HirCodegen, HirDiagnostics,
	FunctionPrototype, Function, IntrinsicInterface
};

use hasan_parser::NUM_SPACES;
use indent::indent_all_by;

/// Every type is essentially a class, even functions.
/// All functions/closures automatically implement their according intrinsic interface
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
	pub name: String,
	pub members: Vec<ClassMember>,
	pub implements_interfaces: Vec<String>
}

impl Type {
	pub fn member_by_name(&self, name: &str) -> Option<ClassMember> {
		self
			.members
			.clone()
			.into_iter()
			.find(|member| member.name() == *name)
	}
}

impl HirDiagnostics for Type {
	fn info_string(&self) -> String {
		let members = self
			.members
			.iter()
			.map(|member| {
				match member {
					ClassMember::Variable(variable) => variable.info_string(),
					ClassMember::Function(function) => function.info_string()
				}
			})
			.collect::<Vec<_>>()
			.join(", ");

		let members = if !members.is_empty() {
			format!("{}\n", members)
		} else {
			String::new()
		};

		let interfaces = self.implements_interfaces.join(", ");

		if interfaces.is_empty() && members.is_empty() {
			return format!("type {} end", self.name);
		}

		if interfaces.is_empty() {
			format!(
				"type {}\n{}end",
			
				self.name,
				indent_all_by(NUM_SPACES, members)
			)
		} else {
			format!(
				"type {}\n{}\n{}end",
			
				self.name,
				indent_all_by(NUM_SPACES, format!("impl {}", interfaces)),
				indent_all_by(NUM_SPACES, members)
			)
		}
	}
}

impl HirCodegen for Type {
	fn codegen(&self) -> String {
		self.name.clone()
	}
}

impl From<FunctionPrototype> for Type {
	fn from(prototype: FunctionPrototype) -> Self {
		Self::from(Function::from(prototype))
	}
}

impl From<Function> for Type {
	fn from(function: Function) -> Self {
		let inner_function = {
			let prototype = FunctionPrototype {
				name: IntrinsicInterface::Function.members().get(0).unwrap().name(),
				arguments: function.prototype.arguments,
				return_type: function.prototype.return_type
			};

			Function {
				prototype,
				body: function.body
			}
		};

		let class_function = crate::ClassFunction {
			attributes: vec![],
			function: inner_function,
			modifiers: crate::ClassFunctionModifiers {
				is_public: true,
				is_static: false
			}
		};

		let class_member = crate::ClassMember::Function(class_function);

		Self {
			name: function.prototype.name,
			members: vec![class_member],
			implements_interfaces: vec![IntrinsicInterface::Function.codegen()]
		}
	}
}