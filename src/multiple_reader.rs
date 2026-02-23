use std::{fs::File, io::{self, BufRead, BufReader, Cursor, Read, Seek}};
use anyhow::{Context, Result};

pub enum MultipleReader {
    String(String),
    File(File),
    Bytes(Vec<u8>)
}

impl MultipleReader {
    pub fn from_stdin() -> Result<Self> {
        let stdin = io::stdin();
        let mut reader = stdin.lock();
        if cfg!(windows) { //windows does not support reading bytes from STDIN, so read it as text
            let mut buf = String::new();
            reader.read_to_string(&mut buf).context("Could not read text from STDIN (on Windows, reading bytes from STDIN is not supported.")?;
            log::info!("read from stdin in text mode with length {}", buf.len());
            return Ok(Self::String(buf));
        } else {
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf)?;
            log::info!("read from stdin in binary mode with length {}", buf.len());
            return Ok(Self::Bytes(buf));
        }
    }

    pub fn from_file(file: File) -> Self {
        return Self::File(file);
    }

    pub fn get(&mut self) -> Result<Box<dyn BufRead + '_>> {
        match self {
            MultipleReader::String(s) => Ok(Box::new(Cursor::new(s))),
            MultipleReader::File(file) => {
                file.seek(io::SeekFrom::Start(0))?;
                return Ok(Box::new(BufReader::new(file)));
            },
            MultipleReader::Bytes(b) => Ok(Box::new(Cursor::new(b))),
        }
    }
}