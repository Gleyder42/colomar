use crate::span;
use crate::span::{SpanInterner, SpanSource, SpanSourceId};
use ariadne::Source;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs::{DirEntry, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::{fs, io};

#[derive(Debug)]
pub struct CachedFile {
    pub source_context: Option<String>,
    pub content: String,
    pub source: Source,
    pub last_update: SystemTime,
}

impl CachedFile {
    pub fn from_content(content: String, last_update: SystemTime) -> Self {
        CachedFile {
            source_context: None,
            source: Source::from(&content),
            content,
            last_update,
        }
    }
}

#[derive(Debug)]
pub struct FileFetcher {
    directories: Vec<PathBuf>,
    files: HashMap<PathBuf, CachedFile>,
}

impl FileFetcher {
    pub fn new(path: PathBuf) -> FileFetcher {
        Self {
            directories: vec![path],
            files: HashMap::new(),
        }
    }

    pub fn set_file(&mut self, path: PathBuf, context: String, content: &str) {
        let mut cached_file = CachedFile::from_content(content.to_string(), SystemTime::now());
        cached_file.source_context = Some(context);

        match self.files.get_mut(&path) {
            None => {
                self.files.insert(path, cached_file);
            }
            Some(file) => *file = cached_file,
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
                    self.files
                        .insert(path, CachedFile::from_content(content, SystemTime::now()));
                }
                Some(_) => { /* Do nothing if the file has not been modified since last update*/ }
            }
        }
        Ok(&self.files)
    }
}

pub struct SourceCache<'a> {
    pub source_cache: &'a mut FileFetcher,
    pub interner: &'a dyn SpanInterner,
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
                .files
                .get(&span_source)
                .map(|cached_file| &cached_file.source)
                .ok_or(Box::new(format!("Cannot lookup {}", span_source.display())))
        }
    }

    fn display<'b>(&self, id: &'b SpanSourceId) -> Option<Box<dyn Display + 'b>> {
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
            .unwrap_or_else(|_| self.interner.lookup_intern_span_source(*id));

        let source_context = self
            .source_cache
            .files
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
        let mut cache = FileFetcher::new(src_dir);

        let _ = cache.update_files()?;
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
