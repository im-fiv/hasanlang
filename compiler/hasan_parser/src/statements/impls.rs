use indent::indent_all_by;

use super::Statement;
use crate::{cond_vec_transform, vec_transform_str, HasanCodegen, NUM_SPACES};

impl HasanCodegen for Statement {
	fn codegen(&self) -> String {
		match self {
			Self::FunctionDefinition(function) | Self::FunctionDeclaration(function) => {
				function.codegen()
			}

			Self::TypeAlias {
				modifiers,
				name,
				generics,
				definition
			} => {
				let modifiers = modifiers.to_string();
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");

				let definition = definition.codegen();
				format!("{modifiers}type {name}{generics} = {definition};")
			}

			Self::ClassDefinition {
				modifiers,
				name,
				generics,
				members
			} => {
				let modifiers = modifiers.to_string();
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");

				let members = indent_all_by(
					NUM_SPACES,
					vec_transform_str(members, |value| value.codegen(), "\n")
				);

				format!("{modifiers}class {name}{generics}\n{members}\nend")
			}

			Self::VariableDefinition {
				modifiers,
				name,
				kind,
				value
			} => {
				let modifiers = modifiers.to_string();

				let value = value.codegen();

				if let Some(kind) = kind {
					let kind = kind.codegen();
					format!("{modifiers}var {name}: {kind} = {value};")
				} else {
					format!("{modifiers}var {name} = {value};")
				}
			}

			Self::VariableAssign { name, value } => {
				format!("{} = {};", name.codegen(), value.codegen())
			}

			Self::FunctionCall {
				callee,
				generics,
				arguments
			} => {
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");
				let arguments = vec_transform_str(arguments, |value| value.codegen(), ", ");

				let callee = callee.codegen();
				format!("{callee}{generics}({arguments});")
			}

			Self::Return(value) => {
				if let Some(value) = value {
					format!("return {};", value.codegen())
				} else {
					String::from("return;")
				}
			}

			Self::EnumDefinition {
				modifiers,
				name,
				variants
			} => {
				let modifiers = modifiers.to_string();
				let variants = indent_all_by(
					NUM_SPACES,
					vec_transform_str(variants, |value| value.codegen(), ",\n")
				);

				format!("{modifiers}enum {name}\n{variants}\nend")
			}

			Self::If {
				condition,
				statements,
				elseif_branches,
				else_branch
			} => {
				let statements = vec_transform_str(statements, |value| value.codegen(), "\n");

				if elseif_branches.is_empty() && else_branch.is_none() {
					return format!(
						"if {} then\n{}\nend",
						condition.codegen(),
						indent_all_by(NUM_SPACES, statements)
					);
				}

				let mut elseif_branches_str = String::new();

				for branch in elseif_branches {
					let branch_statements = vec_transform_str(
						&branch.statements,
						|statement| statement.codegen(),
						"\n"
					);

					elseif_branches_str.push_str(&format!(
						"else if {} then\n{}\n",
						branch.condition.codegen(),
						indent_all_by(NUM_SPACES, branch_statements)
					));
				}

				match else_branch {
					Some(branch) => {
						let else_statements = vec_transform_str(
							&branch.statements,
							|statement| statement.codegen(),
							"\n"
						);

						format!(
							"if {} then\n{}\n{}else\n{}\nend",
							condition.codegen(),
							indent_all_by(NUM_SPACES, statements),
							elseif_branches_str,
							indent_all_by(NUM_SPACES, else_statements)
						)
					}

					None => {
						format!(
							"if {} then\n{}\n{}end",
							condition.codegen(),
							indent_all_by(NUM_SPACES, statements),
							elseif_branches_str
						)
					}
				}
			}

			Self::While {
				condition,
				statements
			} => {
				let statements = vec_transform_str(statements, |value| value.codegen(), "\n");

				format!(
					"while {} do\n{}\nend",
					condition.codegen(),
					indent_all_by(NUM_SPACES, statements)
				)
			}

			Self::For {
				left,
				right,
				statements
			} => {
				let statements = vec_transform_str(statements, |value| value.codegen(), "\n");

				format!(
					"for {} in {} do\n{}\nend",
					left.codegen(),
					right.codegen(),
					indent_all_by(NUM_SPACES, statements)
				)
			}

			Self::Break => String::from("break;"),

			Self::InterfaceDefinition {
				modifiers,
				name,
				generics,
				members
			} => {
				let modifiers = modifiers.to_string();
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");

				let members = indent_all_by(
					NUM_SPACES,
					vec_transform_str(members, |value| value.codegen(), "\n")
				);

				format!("{modifiers}interface {name}{generics}\n{members}\nend")
			}

			Self::InterfaceImplementation {
				interface_name,
				interface_generics,
				class_name,
				class_generics,
				members
			} => {
				let interface_generics =
					cond_vec_transform!(interface_generics, |value| value.codegen(), ", ", "<{}>");
				let class_generics =
					cond_vec_transform!(class_generics, |value| value.codegen(), ", ", "<{}>");

				let members = indent_all_by(
					NUM_SPACES,
					vec_transform_str(members, |value| value.codegen(), "\n")
				);

				format!("impl {interface_name}{interface_generics} for {class_name}{class_generics}\n{members}\nend")
			}

			Self::ModuleUse { path, name } => {
				if path.is_empty() {
					format!("use module {name}")
				} else {
					let path = path.join(".");
					format!("use module {path}.{name}")
				}
			}

			Self::ModuleUseAll { path, name } => {
				if path.is_empty() {
					format!("use module {name}.*")
				} else {
					let path = path.join(".");
					format!("use module {path}.{name}.*")
				}
			}

			Self::ModuleUseItems { path, name, items } => {
				let items = indent_all_by(
					NUM_SPACES,
					vec_transform_str(items, |value| value.codegen(), ",\n")
				);

				if path.is_empty() {
					format!("use module {name}\n{items}\nend")
				} else {
					let path = path.join(".");
					format!("use module {path}.{name}\n{items}\nend")
				}
			}

			Self::Unimplemented => String::from("/* unimplemented */")
		}
	}
}
