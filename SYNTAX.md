# Syntax

*whitespace is not significant*

## Functions

```
def name(arg1: type1, arg2: type2) -> return_type
	expression
```

sometimes return types can be omitted if the function returns a literal.

## Comments

```
# this is a comment

# all comments are single line
```

## Literals

```
1 # int 
3.141 # float
"pikachu" # str
"まさし" # supports unicode
true 
false # booleans
```

## Structs
```
struct Person<generic1, generic2>
	age int
	name str
end
```

## Expressions

```
# all literals are expressions

# binary operators

1+1
1.2*31.2
12121/2
1==1
1>2 or 2>=2 and 2<=2
1!=2

let x = 1 # with type not mentioned
let y: float = 1.414

if x == 2 then y else 4.1 # both arms of an if statement must have the same type

# if returns the value of the branch after evaluation
let z = if 1==1 then 21 else 22
# z becomes 22

# if the if-statement has only the if-branch, then both the branches return 0

# do expression returns the last expression as it's value
do
	printstrln("hi")
	1+2
end

# we can have any expression within any expression
if do
	printstrln("if condition")
	true
end 
then do
	printstrln("then branch")
	if 1==1 then printstrln("ok.")
end

```
## Generics

**generics are not fully working yet**
```
struct Vector<T>
	x T
	y T
	z T
end
```

```
def afunctionwithgenerics(arg1: type1<generic1, generic2, generic3>, arg2: type2) -> return_type<generic1>
	expression
```
