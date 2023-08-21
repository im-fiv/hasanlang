use crate::{HasanCodegen, cond_vec_transform, vec_transform_str, NUM_SPACES};
use super::Statement;

use indent::indent_all_by;

impl HasanCodegen for Statement {
	fn codegen(&self) -> String {
		match self {
			Self::FunctionDefinition(function) |
			Self::FunctionDeclaration(function) => function.codegen(),

			Self::TypeAlias { modifiers, name, generics, definition } => {
				let modifiers = cond_vec_transform!(modifiers, |value| value.to_string(), " ", "{} ");
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");

				format!("{}type {}{} = {};", modifiers, name, generics, definition.codegen())
			},

			Self::ClassDefinition { modifiers, name, generics, members } => {
				let modifiers = cond_vec_transform!(modifiers, |value| value.to_string(), " ", "{} ");
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");

				let members = vec_transform_str(members, |value| value.codegen(), "\n");

				format!("{}class {}{}\n{}\nend", modifiers, name, generics, indent_all_by(NUM_SPACES, members))
			},

			Self::VariableDefinition { modifiers, name, kind, value } => {
				let modifiers = cond_vec_transform!(modifiers, |value| value.to_string(), " ", "{} ");

				if let Some(kind) = kind {
					format!("{}var {}: {} = {};", modifiers, name, kind.codegen(), value.codegen())
				} else {
					format!("{}var {} = {};", modifiers, name, value.codegen())
				}
			},

			Self::VariableAssign { name, value } => format!("{} = {};", name.codegen(), value.codegen()),

			Self::FunctionCall { callee, generics, arguments } => {
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");
				let arguments = vec_transform_str(arguments, |value| value.codegen(), ", ");

				format!("{}{}({});", callee.codegen(), generics, arguments)
			},

			Self::Return(value) => if let Some(value) = value {
				format!("return {};", value.codegen())
			} else {
				"return;".to_owned()
			},

			Self::EnumDefinition { modifiers, name, variants } => {
				let modifiers = cond_vec_transform!(modifiers, |value| value.to_string(), " ", "{} ");
				let variants = vec_transform_str(variants, |value| value.codegen(), ",\n");
				
				format!(
					"{}enum {}\n{}\nend",
					
					modifiers,
					name,
					indent_all_by(NUM_SPACES, variants)
				)
			},

			Self::If { condition, statements, elseif_branches, else_branch } => {
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
					
					elseif_branches_str.push_str(
						&format!(
							"else if {} then\n{}\n",

							branch.condition.codegen(),
							indent_all_by(NUM_SPACES, branch_statements)
						)
					);
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
					},

					None => {
						format!(
							"if {} then\n{}\n{}end",

							condition.codegen(),
							indent_all_by(NUM_SPACES, statements),
							elseif_branches_str
						)
					}
				}
			},

			Self::While { condition, statements } => {
				let statements = vec_transform_str(statements, |value| value.codegen(), "\n");

				format!(
					"while {} do\n{}\nend",

					condition.codegen(),
					indent_all_by(NUM_SPACES, statements)
				)
			},

			Self::For { left, right, statements } => {
				let statements = vec_transform_str(statements, |value| value.codegen(), "\n");

				format!(
					"for {} in {} do\n{}\nend",

					left.codegen(),
					right.codegen(),
					indent_all_by(NUM_SPACES, statements)
				)
			},

			Self::Break => "break;".to_owned(),

			Self::InterfaceDefinition { modifiers, name, generics, members } => {
				let modifiers = cond_vec_transform!(modifiers, |value| value.to_string(), " ", "{} ");
				let generics = cond_vec_transform!(generics, |value| value.codegen(), ", ", "<{}>");

				let members = vec_transform_str(members, |value| value.codegen(), "\n");

				format!(
					"{}interface {}{}\n{}\nend",

					modifiers,
					name,
					generics,
					indent_all_by(NUM_SPACES, members)
				)
			},

			Self::InterfaceImplementation { interface_name, interface_generics, class_name, class_generics, members } => {
				let interface_generics = cond_vec_transform!(interface_generics, |value| value.codegen(), ", ", "<{}>");
				let class_generics = cond_vec_transform!(class_generics, |value| value.codegen(), ", ", "<{}>");

				let members = vec_transform_str(members, |value| value.codegen(), "\n");

				format!(
					"impl {}{} for {}{}\n{}\nend",

					interface_name,
					interface_generics,
					class_name,
					class_generics,
					indent_all_by(NUM_SPACES, members)
				)
			},

			Self::ModuleUse { path, name } => {
				if path.is_empty() {
					format!("use module {}", name)
				} else {
					format!("use module {}.{}", path.join("."), name)
				}
			},

			Self::ModuleUseAll { path, name } => {
				if path.is_empty() {
					format!("use module {}.*", name)
				} else {
					format!("use module {}.{}.*", path.join("."), name)
				}
			},
			
			Self::ModuleUseItems { path, name, items } => {
				let items = vec_transform_str(items, |value| value.codegen(), ",\n");

				if path.is_empty() {
					format!(
						"use module {}\n{}\nend",
						
						name,
						indent_all_by(NUM_SPACES, items)
					)
				} else {
					format!(
						"use module {}.{}\n{}\nend",
						
						path.join("."),
						name,
						indent_all_by(NUM_SPACES, items)
					)
				}
			},

			Self::Unimplemented => "/* unimplemented */".to_owned()
		}
	}
}