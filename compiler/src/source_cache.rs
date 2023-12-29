use crate::database::CompilerDatabase;
use crate::span::{SpanInterner, SpanSource, SpanSourceId};
use ariadne::Source;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs::{DirEntry, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::{fs, io};

pub struct CachedFile {
    pub content: String,
    pub source: Source,
    pub last_update: SystemTime,
}

pub struct SourceCache {
    pub directories: Vec<PathBuf>,
    files: HashMap<PathBuf, CachedFile>,
}

impl SourceCache {
    pub fn new(path: PathBuf) -> SourceCache {
        Self {
            directories: vec![path],
            files: HashMap::new(),
        }
    }

    fn read_dir_paths(&self) -> io::Result<Vec<PathBuf>> {
        let mut paths = Vec::new();

        let mut cb = |dir_entry: &DirEntry| {
            paths.push(dir_entry.path());
            Ok(())
        };

        for path in &self.directories {
            visit_dirs(path, &mut cb)?;
        }

        Ok(paths)
    }

    pub fn update_files(&mut self) -> io::Result<&HashMap<PathBuf, CachedFile>> {
        let paths = self.read_dir_paths()?;
        for path in paths {
            let last_modified = path.metadata()?.modified()?;

            match self.files.get_mut(&path) {
                Some(cached_file) if last_modified > cached_file.last_update => {
                    let mut file = File::open(path)?;
                    let size = file.metadata().map(|m| m.len() as usize).ok();
                    size.map(|size| cached_file.content.reserve_exact(size));
                    cached_file.content.clear();
                    file.read_to_string(&mut cached_file.content)?;
                    cached_file.source = ariadne::Source::from(&cached_file.content);
                    cached_file.last_update = SystemTime::now()
                }
                None => {
                    let content = fs::read_to_string(&path)?;
                    self.files.insert(
                        path,
                        CachedFile {
                            source: ariadne::Source::from(&content),
                            content,
                            last_update: SystemTime::now(),
                        },
                    );
                }
                Some(_) => { /* Do nothing if the file has not been modified since last update*/ }
            }
        }
        Ok(&self.files)
    }
}

pub struct LookupSourceCache<'a> {
    pub source_cache: &'a SourceCache,
    pub interner: &'a CompilerDatabase,
    pub src_dir: &'a PathBuf,
}

pub struct EmptyLookupSource(Source);

impl Default for EmptyLookupSource {
    fn default() -> Self {
        EmptyLookupSource(Source::from(""))
    }
}

impl ariadne::Cache<SpanSourceId> for EmptyLookupSource {
    fn fetch(&mut self, id: &SpanSourceId) -> Result<&Source, Box<dyn Debug + '_>> {
        Ok(&self.0)
    }

    fn display<'a>(&self, id: &'a SpanSourceId) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new("none"))
    }
}

impl<'a> ariadne::Cache<SpanSourceId> for LookupSourceCache<'a> {
    fn fetch(&mut self, id: &SpanSourceId) -> Result<&Source, Box<dyn Debug + '_>> {
        let span_source = self.interner.lookup_intern_span_source(*id);
        self.source_cache
            .files
            .get(&span_source)
            .map(|cached_file| &cached_file.source)
            .ok_or(Box::new(format!("Cannot lookup {}", span_source.display())))
    }

    fn display<'b>(&self, id: &'b SpanSourceId) -> Option<Box<dyn Display + 'b>> {
        let span_source = self.interner.lookup_intern_span_source(*id);
        let span_source = span_source
            .strip_prefix(self.src_dir)
            .unwrap()
            .to_path_buf();

        struct SpanSourceDisplay(SpanSource);

        impl Display for SpanSourceDisplay {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0.display())
            }
        }

        Some(Box::new(SpanSourceDisplay(span_source)))
    }
}

pub fn read_dir_paths(dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut vec = Vec::new();

    let mut cb = |dir_entry: &DirEntry| {
        vec.push(dir_entry.path());
        Ok(())
    };

    visit_dirs(dir, &mut cb)?;

    Ok(vec)
}

pub fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&DirEntry) -> io::Result<()>) -> io::Result<()> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::thread::sleep;
    use std::time::Duration;
    use tempdir::TempDir;

    /// # Important
    /// When naming the returning TempDir with '_', the rust compiler,
    /// will optimize the TempDir out, dropping it at that place, which will also delete the files.
    /// But these files are important for the test.
    /// Appending a name to the underscore still marks the variable as unused, but the compiler will not drop the value.
    fn setup() -> io::Result<(TempDir, PathBuf, Vec<PathBuf>)> {
        let temp_dir = TempDir::new("unit_tests")?;
        let src_dir: PathBuf = temp_dir.path().join("test_src");

        fs::create_dir(&src_dir)?;
        let a = src_dir.join("a.co");
        fs::write(&a, "Hello World")?;
        let b = src_dir.join("b.co");
        fs::write(&b, "This is a test file")?;
        let c = src_dir.join("c.co");
        fs::write(&c, "Don't panic!")?;

        Ok((temp_dir, src_dir, vec![a, b, c]))
    }

    #[test]
    fn test_read_files_first_time() -> io::Result<()> {
        let (_temp_dir, src_dir, files) = setup()?;
        println!("{:?}", fs::read_dir(&src_dir)?);
        let mut cache = SourceCache::new(src_dir);

        let updated = cache.update_files()?;

        assert_eq!(
            updated.len(),
            files.len(),
            "Compare if updated and files have the same length"
        );
        for (path, cached_file) in updated {
            let actual = fs::read_to_string(path)?;
            let expected = &cached_file.content;

            assert_eq!(&actual, expected, "Compare read string with update string");
            assert_eq!(
                ariadne::Source::from(&actual),
                cached_file.source,
                "Compare ariadne sources"
            )
        }
        Ok(())
    }

    #[test]
    fn test_update_content_after_file_changed() -> io::Result<()> {
        let (_temp_dir, src_dir, files) = setup()?;
        let mut cache = SourceCache::new(src_dir);

        let updated = cache.update_files()?;
        let expected = "The first file has changed";
        let first_file = &files[0];
        fs::write(first_file, expected)?;

        let actual = &cache.update_files()?[first_file];

        assert_eq!(
            &actual.content, expected,
            "Compare the updated value with the actual one"
        );
        assert_eq!(
            ariadne::Source::from(&actual.content),
            ariadne::Source::from(expected),
            "Compare ariadne sources"
        );
        Ok(())
    }

    #[test]
    fn test_newer_read_will_update_timestamp() -> io::Result<()> {
        let (_temp_dir, src_dir, files) = setup()?;
        let mut cache = SourceCache::new(src_dir);

        let _ = cache.update_files()?;
        // Let the test sleep for a short durations, so the System time changes
        sleep(Duration::from_millis(1));
        let second_update = SystemTime::now();
        let updated = cache.update_files()?;

        for (path, cached_file) in updated {
            assert!(
                cached_file.last_update <= second_update,
                "Check if last read {:?} is lesser than the second {:?} update",
                cached_file.last_update,
                second_update
            )
        }
        Ok(())
    }
}
