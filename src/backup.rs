use std::ops::Range;
use leptos::{prelude::*, svg::line};
use crate::utils::{is_well_formed_func, is_well_formed_instrline};

/// This always hold a correct version of the function.
/// It should **always** have the same line numbers with the current text editor.
/// 
/// The commit is accepted if and only if the updated content will pass global test &&
/// each of the line passes line well-formness checking
#[derive(Debug, Default)]
pub struct Backup(ArcRwSignal<Vec<String>>);

#[derive(Debug)]
pub enum Commit
{
    /// Try to delete [start, end) lines in the content
    Delete(Range<usize>),

    /// Try to insert a vec of string at a at a certain index
    Insert(usize, Vec<String>),

    /// Try to substitue one line
    Update(usize, String)
}

impl Backup {
    /// The commit is accepted if and only if the updated content passes correctness test.
    /// And content will notify its subscribers only when the commit is accepted.
    /// 
    /// This function is not a subscriber of content. Should be used in Effect. 
    pub fn commit(&self, entry: &Commit){
        let mut new_content = self.0.get_untracked();
        let length = new_content.len();
        
        match entry {
            Commit::Delete(range) => {
                // Validate range
                if range.end > length || range.start >= range.end {
                    return;
                }
                
                // Remove the lines in the range
                new_content.splice(range.clone(), vec![]);
            }
            Commit::Insert(idx, lines) => {
                // Validate index
                if *idx > length {
                    return;
                }
                if lines.iter().any( |line| is_well_formed_instrline(&line).is_err())
                {
                    return;
                }

                // Insert lines at index
                new_content.splice(*idx..*idx, lines.clone());
            }
            Commit::Update(idx, line) => {
                // Validate index
                if *idx >= length {
                    return;
                }
                if is_well_formed_instrline(&line).is_err()
                {
                    return;
                }
                
                // Update the line at index
                new_content[*idx] = line.clone();
            }
        }
        
        // Check if the new content is well-formed
        if is_well_formed_func(&new_content.join("\n")) {
            // Commit the changes if well-formed
            self.0.set(new_content);
        }
    }
}