mod parser;

pub use parser::*;

pub const NUM_SPACES: usize = 4;

pub fn vec_transform_str<Elem, Func>(vec: &[Elem], func: Func, sep: &str) -> String
where
	Func: Fn(&Elem) -> String
{
	vec
		.iter()
		.map(func)
		.collect::<Vec<String>>()
		.join(sep)
}