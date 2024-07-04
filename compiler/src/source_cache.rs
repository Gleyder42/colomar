use crate::span;
use crate::span::{SpanInterner, SpanSource, SpanSourceId};
use ariadne::Source;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs::DirEntry;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::{fs, io};

#[derive(Debug)]
pub struct CachedFile {
    /// Contains the toml key defined in the native workshop definition toml file
    /// This gives the error content which native definition caused the error
    /// Without it, the developer would only know the file, but not the exact error location.
    pub source_context: Option<String>,
    /// Contains the content of the file ready to be used by ariadne.
    pub source: Source,
    /// Stores the timestamp when the content was last read from the file stored on the filesystem.
    pub last_update: SystemTime,
}

impl CachedFile {
    /// Creates a cached file from a string
    pub fn from_content(content: String, last_update: SystemTime) -> Self {
        CachedFile {
            source_context: None,
            source: Source::from(content),
            last_update,
        }
    }
}

/// Caches the content of files inside watched directories.
/// This struct is a file cache but was named FileFetcher to avoid ambiguity with [ariadne::FileCache].
#[derive(Debug)]
pub struct FileFetcher {
    watched_directories: Vec<PathBuf>,
    path_to_cached_file_map: HashMap<PathBuf, CachedFile>,
}

impl FileFetcher {
    pub fn new(watched_directory: PathBuf) -> FileFetcher {
        Self {
            watched_directories: vec![watched_directory],
            path_to_cached_file_map: HashMap::new(),
        }
    }

    /// Inserts a new file to be cached.
    /// Overwrite a previously cached file.
    pub fn insert_file(&mut self, path: PathBuf, context: String, content: &str) {
        let mut cached_file = CachedFile::from_content(content.to_string(), SystemTime::now());
        cached_file.source_context = Some(context);

        match self.path_to_cached_file_map.get_mut(&path) {
            None => {
                self.path_to_cached_file_map.insert(path, cached_file);
            }
            Some(file) => *file = cached_file,
        }
    }

    fn read_watched_directories(&self) -> io::Result<Vec<PathBuf>> {
        let mut file = Vec::new();

        let mut visit_dir_entry = |dir_entry: &DirEntry| {
            file.push(dir_entry.path());
            Ok(())
        };

        for watched_directory in &self.watched_directories {
            visit_dirs(watched_directory, &mut visit_dir_entry)?;
        }

        Ok(file)
    }

    /// Reads the content of all watched files that have changed since they were last read.
    /// This essentially ensures that all cached files are up-to-date with the actual files stored
    /// on the filesystem.
    pub fn update_files(&mut self) -> io::Result<&HashMap<PathBuf, CachedFile>> {
        let paths = self.read_watched_directories()?;
        for path in paths {
            // Stores the timestamp when the actual file was last modified
            let last_modified = path.metadata()?.modified()?;

            match self.path_to_cached_file_map.get_mut(&path) {
                Some(cached_file) if last_modified > cached_file.last_update => {
                    let content = fs::read_to_string(&path)?;
                    cached_file.last_update = SystemTime::now();
                    cached_file.source = ariadne::Source::from(content);
                }
                None => {
                    let content = fs::read_to_string(&path)?;
                    self.path_to_cached_file_map
                        .insert(path, CachedFile::from_content(content, SystemTime::now()));
                }
                Some(_) => { /* Do nothing if the file has not been modified since last update*/ }
            }
        }
        Ok(&self.path_to_cached_file_map)
    }
}

/// Struct which implements ariadne::Cache and should be used with ariadne.
pub struct SourceCache<'a> {
    // used to read the content of request file path
    pub source_cache: &'a mut FileFetcher,
    // used to lookup interned requested id
    pub interner: &'a dyn SpanInterner,
    // used to strip the prefix of request file path
    pub src_dir: &'a PathBuf,
}

lazy_static! {
    static ref EMPTY_SOURCE: Source = Source::from("");
}

impl<'a> ariadne::Cache<SpanSourceId> for SourceCache<'a> {
    fn fetch(&mut self, id: &SpanSourceId) -> Result<&Source, Box<dyn Debug + '_>> {
        let span_source = self.interner.lookup_intern_span_source(*id);

        // Cannot compare FAKE_SPAN_SOURCE_NAME directly with span_source because
        // FAKE_SPAN_SOURCE_NAME is not an actual PathBuf, but rather some generated type by lazy_static!.
        // Therefore we use as_os_str() as that can be compared to a PathBuf
        if span_source == span::FAKE_SPAN_SOURCE_NAME.as_os_str() {
            Ok(&EMPTY_SOURCE)
        } else {
            self.source_cache
                .path_to_cached_file_map
                .get(&span_source)
                .map(|cached_file| &cached_file.source)
                .ok_or(Box::new(format!("Cannot lookup {}", span_source.display())))
        }
    }

    fn display<'b>(&self, id: &'b SpanSourceId) -> Option<Box<dyn Display + 'b>> {
        // required so span_source and source_content are owned.
        struct SpanSourceDisplay(SpanSource, Option<String>);

        impl Display for SpanSourceDisplay {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match &self.1 {
                    Some(source_context) => write!(f, "{}({})", self.0.display(), source_context),
                    None => write!(f, "{}", self.0.display()),
                }
            }
        }

        let span_source = self.interner.lookup_intern_span_source(*id);
        let span_source = span_source
            .strip_prefix(self.src_dir)
            .map(|it| it.to_path_buf())
            .unwrap_or_else(|_| span_source);

        let source_context = self
            .source_cache
            .path_to_cached_file_map
            .get(&span_source)
            .and_then(|it| it.source_context.as_ref())
            .cloned();

        Some(Box::new(SpanSourceDisplay(span_source, source_context)))
    }
}

pub struct EmptyLookupSource(Source);

impl ariadne::Cache<SpanSourceId> for EmptyLookupSource {
    fn fetch(&mut self, _id: &SpanSourceId) -> Result<&Source, Box<dyn Debug + '_>> {
        Ok(&self.0)
    }

    fn display<'a>(&self, _id: &'a SpanSourceId) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new("none"))
    }
}

impl Default for EmptyLookupSource {
    fn default() -> Self {
        EmptyLookupSource(Source::from(""))
    }
}

pub fn visit_dirs(
    dir: &Path,
    visit_dir: &mut dyn FnMut(&DirEntry) -> io::Result<()>,
) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, visit_dir)?;
            } else {
                visit_dir(&entry)?;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
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
        let mut cache = FileFetcher::new(src_dir);

        let updated = cache.update_files()?;

        assert_eq!(
            updated.len(),
            files.len(),
            "Compare if updated and files have the same length"
        );
        for (path, cached_file) in updated {
            let actual = fs::read_to_string(path)?;

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
        let mut cache = FileFetcher::new(src_dir);

        let _ = cache.update_files()?;
        let expected = "The first file has changed";
        let first_file = &files[0];
        fs::write(first_file, expected)?;

        sleep(Duration::from_millis(1));

        let actual = &cache.update_files()?[first_file];

        assert_eq!(
            actual.source,
            ariadne::Source::from(expected),
            "Compare ariadne sources"
        );
        Ok(())
    }

    #[test]
    fn test_newer_read_will_update_timestamp() -> io::Result<()> {
        let (_temp_dir, src_dir, _) = setup()?;
        let mut cache = FileFetcher::new(src_dir);

        let _ = cache.update_files()?;
        // Let the test sleep for a short durations, so the System time changes
        sleep(Duration::from_millis(1));
        let second_update = SystemTime::now();
        let updated = cache.update_files()?;

        for (_, cached_file) in updated {
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
