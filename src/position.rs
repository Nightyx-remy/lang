use std::fmt::{Display, Formatter};

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                            Position                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone)]
pub struct Position {
    pub index: usize,
    pub column: usize,
    pub line: usize,
}

impl Position {

    pub fn eof() -> Position {
        return Position {
            index: usize::MAX,
            column: 0,
            line: 0,
        }
    }

    pub fn advance(&mut self, new_line: bool) {
        self.index += 1;
        if new_line {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub fn select(src: String, start: Position, end: Position) -> String {
        let mut str = String::new();

        if start.index == usize::MAX {
            // EOF
            let line_str = src.lines().nth(src.lines().count() - 1).unwrap();

            str.push_str(line_str);
            str.push_str(format!("\n{}{}\n", " ".repeat(line_str.len()), "^").as_str());
        } else {
            for line in start.line..=end.line {
                let line_str = src.lines().nth(line).unwrap();

                let offset = if line == start.line { start.column } else { 0 };
                let size = if line == end.line { end.column } else { line_str.len() } - offset;

                str.push_str(line_str);
                str.push_str(format!("\n{}{}\n", " ".repeat(offset), "^".repeat(size)).as_str());
            }
        }

        return str;
    }

}

impl Default for Position {

    fn default() -> Self {
        return Position {
            index: 0,
            column: 0,
            line: 0
        }
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                        Positioned Trait                                        //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Positioned {

    fn start(&self) -> Position;
    fn end(&self) -> Position;

    fn select(&self, src: String) -> String {
        return Position::select(src, self.start(), self.end());
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                        Positioned Object                                       //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub struct PositionedObject<T> {
    pub start: Position,
    pub end: Position,
    pub value: T
}

impl<T> PositionedObject<T> {

    pub fn new(value: T, start: Position, end: Position) -> Self {
        return Self {
            start,
            end,
            value
        }
    }

    pub fn convert<U>(&self, value: U) -> PositionedObject<U> {
        return PositionedObject::<U>::new(value, self.start, self.end);
    }

}

impl<T> Positioned for PositionedObject<T> {

    fn start(&self) -> Position {
        self.start
    }

    fn end(&self) -> Position {
        self.end
    }

}

impl<T: Display> PositionedObject<T> {

    pub fn error(&self, src: String) -> String {
        let mut str = String::new();

        str.push_str(format!("[Error]: {}\n", self.value).as_str());
        str.push_str(self.select(src).as_str());

        return str;
    }

    pub fn warn(&self, src: String) -> String {
        let mut str = String::new();

        str.push_str(format!("[Warning]: {}\n", self.value).as_str());
        str.push_str(self.select(src).as_str());

        return str;
    }

}

impl<T: Display> Display for PositionedObject<T> {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }

}

impl<T: Clone> Clone for PositionedObject<T> {

    fn clone(&self) -> Self {
        return Self {
            start: self.start.clone(),
            end: self.end.clone(),
            value: self.value.clone()
        }
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                    Useful Positioned Objects                                   //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedString = PositionedObject<String>;
pub type EmptyPositioned = PositionedObject<()>;