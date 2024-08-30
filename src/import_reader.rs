use std::{io::{Lines, BufRead, BufReader}, fs::File, path::Path};
use anyhow::{anyhow, Result, Context, Error};
use num_rational::BigRational;

pub trait ImportRead {
    /**
     * This method should be called before any of the next_line methods. 
     * It will advance the reader by one line on first call, and on any subsequent calls will return this line without advancing the reader.
     */
    fn get_first_line(&mut self) -> Result<&String>;

    fn get_last_line_number(&self) -> usize;
    fn get_last_line(&self) -> &str;

    fn next_line_raw(&mut self) -> Result<()>;

    fn next_line_string(&mut self) -> Result<String> {
        self.next_line()?;
        Ok(self.get_last_line().to_string())
    }

    fn next_line_index(&mut self) -> Result<usize> {
        self.next_line()?;
        self.get_last_line().trim().parse::<usize>().with_context(|| format!("failed to read integer at line {}; found `{}`", self.get_last_line_number(), self.get_last_line()))
    }

    fn next_line_natural(&mut self) -> Result<u64> {
        self.next_line()?;
        self.get_last_line().trim().parse::<u64>().with_context(|| format!("failed to read integer at line {}; found `{}`", self.get_last_line_number(), self.get_last_line()))
    }

    fn next_line_weight(&mut self) -> Result<BigRational> {
        self.next_line()?;
        //attempt to read a rational
        let weighte = self.get_last_line().trim().parse::<BigRational>();
        if weighte.is_ok() {
            return Ok(weighte.unwrap());
        }

        //attempt to read a float
        {
            let weight = self.get_last_line().trim().parse::<f64>().with_context(|| format!("failed to interpret line {} as rational or float; found `{}`", self.get_last_line_number(), self.get_last_line()))?;
            let weightr: BigRational = BigRational::from_float(weight).with_context(|| format!("failed to transform float into rational at line {}; found `{}`", self.get_last_line_number(), self.get_last_line()))?;
            Ok(weightr)
        }
    }

    fn next_line(&mut self) -> Result<()> {
        //read line and unpack
        self.next_line_raw()?;
        while self.get_last_line().starts_with('#') {
            self.next_line_raw()?;
        }
        Ok(())
    }
}

pub struct ImportReader<R: BufRead> {
    first_line: String,
    first_line_read: bool,
    first_line_error: bool,
    lines: Lines<R>,
    line_no: usize,
    line: String
}

impl <R: BufRead> ImportReader<R> {
    pub fn new(reader: R) -> Self {
        ImportReader {
            first_line: "".to_string(),
            first_line_read: false,
            first_line_error: false,
            lines: reader.lines(),
            line_no: 0,
            line: "".to_string()
        }
    }

    pub fn from_path<P>(path: P) -> Result<ImportReader<BufReader<File>>> where P: AsRef<Path> {
        let file = File::open(path)?;
        Ok(ImportReader::new(BufReader::new(file)))
    }
}

impl <'a, R: BufRead> ImportRead for ImportReader<R> {

    fn get_first_line(&mut self) -> Result<&String> {
        if self.first_line_read {
            if self.first_line_error {
                return Err(anyhow!("Error while reading first line"));
            } else {
                return Ok(&self.first_line);
            }
        }

        let x = self.next_line_raw();
        self.first_line_read = true;
        if let Err(_) = x {
            self.first_line_error = true;
        } else {
            self.first_line = self.get_last_line().to_owned();
        }

        return self.get_first_line();
    }

    fn get_last_line_number(&self) -> usize {
        self.line_no
    }

    fn get_last_line(&self) -> &str {
        &self.line
    }

    fn next_line_raw(&mut self) -> Result<()> {
        match self.lines.next() {
            Some(line) => {
                self.line_no += 1;
                self.line = line?;
                return Ok(())
            },
            None => return Err(anyhow!("premature end of file")),
        }
    }
}

/**
 * A reader that reads the entire reader into memory on creation, and which can be reset.
 */
pub struct ImportReaderResettable {
    first_line: String,
    first_line_read: bool,
    first_line_error: bool,
    lines: Vec<String>,
    next_line_no: usize,
    reset_to: usize
}

impl ImportReaderResettable {
    pub fn new<R>(reader: R) -> anyhow::Result<Self> where R: BufRead {
        Ok(ImportReaderResettable {
            first_line: String::new(),
            first_line_read: true,
            first_line_error: false,
            lines: reader.lines().collect::<Result<Vec<_>, _>>()?,
            next_line_no: 0,
            reset_to: 0
        })
    }

    pub fn from_path<P>(path: P) -> Result<ImportReaderResettable> where P: AsRef<Path> {
        let file = File::open(path)?;
        Ok(Self::new(BufReader::new(file))?)
    }

    pub fn reset(&mut self) {
        self.next_line_no = self.reset_to;
    }


    /**
     * Removes the part of the input that has already been consumed. 
     * A sub-sequent call to reset will bring the reader to the same point it is in now.
     */
    pub fn truncate(&mut self) {
        self.reset_to = self.next_line_no;
    }
    
}

impl ImportRead for ImportReaderResettable {

    fn get_first_line(&mut self) -> Result<&String> {
        if self.first_line_read {
            if self.first_line_error {
                return Err(anyhow!("Error while reading first line"));
            } else {
                return Ok(&self.first_line);
            }
        }

        let x = self.next_line_raw();
        self.first_line_read = true;
        if let Err(_) = x {
            self.first_line_error = true;
        } else {
            self.first_line = self.get_last_line().to_owned();
        }

        return self.get_first_line();
    }

    fn get_last_line_number(&self) -> usize {
        self.next_line_no - 1
    }

    fn get_last_line(&self) -> &str {
        &self.lines[self.next_line_no - 1]
    }

    fn next_line_raw(&mut self) -> Result<()> {
        self.next_line_no += 1;
        if self.next_line_no > self.lines.len() {
            return Err(anyhow!("premature end of file"));
        } else {
            return Ok(())
        }
    }
    
}