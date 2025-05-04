# Changelog

DD/MM/YYYY

*changes were not noted before 22/11/2024*

### 22/11/2024
	- Added support for underscores in integers
	- Updated todo.md
	- Cleaned up some parts
	- Added codegen using inkwell
		- Ints work
		- Strings work
		- TCO works
	- Cleaner parser errors (using OwoColorize)
	- Formatted using `cargo fmt`

## 23/11/2024
	- Added support for struct codegen
	- REDID THE ENTIRETY OF THE PREVIOUS CODEGEN!
	- STRUCTS WORK FULLY (EXCEPT SELF-REFERENCING STRUCTS)!

## 24/11/2024
	- Changed usage (now it's `book <filename>`)

## 30/11/2024
	- Socket server works

## 1/12/2024
	- Syntax highlighting for Sublime-Text

## 2/12/2024
	- Added tests for lexer and parser.
	- Setup a GH actions workflow.

## 3/12/2024
	- Added while loop, return, break and continue.
	- Added assignment expression.
	- Updated syntax highlighting.

## 4/12/2024
	- Removed return, break and continue.
	- Trying to rewrite the parser or work on array codegen. Terribly stuck.

## 5/12/2024
	- Re-added return, break and continue.
	- Arrays added. Indexing works. n-Dimensional arrays work. (thanks a lot void*)

## 6/12/2024
	- Span based errors in parser

## 7/12/2024
	- Added span and filename to expressions

## 8/12/2024
	- type checkcer improvements
	- alloc an array with empty fields using `array`

## 9/12/2024
	- added dynamic arrays!
	- added a `push`

## 10/12/2024
	- attempted adding methods

## 11/12/2024
	- fixed an issue where a ptr was passed instead of a struct in function args

## (After several updates) 11/4/25
	- Reverted dynamic arrays
	- All arrays are dynamic now
	- Minor bug fixes

# 12/4/25
	- added better type error and codegen error reporting

# 5/5/25
	- Renamed book/ to doc/ to avoid confusion.
	- Added a README.md
	- Added a license
