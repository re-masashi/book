Lexer
=====

The first of the 3 steps explained in the overview is: Lexing.

Lexing is the process of breaking down the source code into a stream of "tokens", aka "lexemes".

Tokens can be anything. From a string to an int or a float or a keyword.
Tokens are the basic unit which the parser uses to parse the program. (more on that later.)

Take this line of javascript for example.

.. code-block::
	
	let x = 21;

after tokenising, it becomes something like this:

.. code-block::

	[Token::Let, Token::Identifier(x), Token::Assign, Token::Number(21), Token::Semicolon]


I think we can start with the code now.

After creating a new Cargo project, create a folder(or directory. whatever you want to call it) named ``lexer`` within the existing ``src/`` folder.
Within the folder, create the files ``mod.rs`` and ``token.rs``.

Now, within the ``tokens.rs`` file.

.. code-block:: rust

	#[derive(Debug)]
	pub enum TokenType {
	    Int(i64),
	    Float(f64),
	    String(String),
	    Identifier(String),

	    // ...
	}

This enum contains the type of the token, and the data associated with the token.

But this is not all... so let's change the code.

.. code-block:: rust 

	#[derive(Debug)]
	pub enum TokenType {
		// previous code

		// keywords
		If,
		Else,
		For,
		While,
		In,
		As,
		Do,
		End,
		True,
		False,

		// did not add True and False as a Boolean(b) variant instead because
		// keeping keywords together is more neat and sane (imo)

		// operators
		Plus,
		Minus,
		Mul,
		Div,
		PlusEq,
		MinusEq,
		MulEq,
		DivEq,
		Pipe,
		Assign,

		And,
		Or,
		Not,
		Equal,
		NotEq,
		Greater,
		Less,
		GreaterEq,
		LessEq,

		// punctuators
		LParen,
		RParen,
		LBrack,
		RBrack,
		LBrace,
		RBrace,
		Colon,
		Semicolon,
		Comma,
		Dot,
		Arrow,

		Unknown,
	}

"Well, what do I do with this?"
-------------------------------

Now, comes the main part. The code for the lexing process.

