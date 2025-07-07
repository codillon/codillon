use crate::utils::*;

#[derive(Debug)]
struct Website
{
    content: Vec<String>,
    cursor: usize,
}

impl Website
{
    pub fn get_content(&self) -> &Vec<String>
    {
        &self.content
    }
}