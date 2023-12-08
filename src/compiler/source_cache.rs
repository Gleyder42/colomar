use std::collections::HashMap;
use std::fs::DirEntry;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::{fs, io};

struct SourceCache {
    dir: PathBuf,
    last_modified: HashMap<PathBuf, SystemTime>,
    cached_files: HashMap<PathBuf, String>,
}

impl SourceCache {
    pub fn new(path: impl Into<PathBuf>) -> SourceCache {
        Self {
            dir: path.into(),
            last_modified: HashMap::new(),
            cached_files: HashMap::new(),
        }
    }

    fn save_change_time_stamps(&mut self) -> io::Result<()> {
        let mut cb = |entry: &DirEntry| {
            let last_modified = entry.metadata()?.modified()?;
            self.last_modified.insert(entry.path(), last_modified);
            Ok(())
        };
        visit_dirs(&self.dir, &mut cb)
    }

    pub fn changed_files(&mut self) -> io::Result<()> {
        let mut to_load = Vec::new();

        let mut cb = |entry: &DirEntry| {
            let path_buf = entry.path();

            let saved_last_modified = self
                .last_modified
                .entry(path_buf.clone())
                .or_insert(SystemTime::now());
            let current_last_modified = entry.metadata()?.modified()?;
            if &current_last_modified != saved_last_modified {
                to_load.push(path_buf)
            }

            Ok(())
        };

        visit_dirs(&self.dir, &mut cb)?;

        for path_buf in to_load {
            let source = fs::read_to_string(&path_buf)?;
            self.cached_files.insert(path_buf, source);
        }

        Ok(())
    }
}

pub fn read_files_to_string(dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut vec = Vec::new();

    let mut cb = |dir_entry: &DirEntry| {
        vec.push(dir_entry.path());
        Ok(())
    };

    visit_dirs(dir, &mut cb)?;

    Ok(vec)
}

fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&DirEntry) -> io::Result<()>) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry)?;
            }
        }
    }
    Ok(())
}
