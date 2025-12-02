// Package lexer provides functionality for generating lexers from EBNF grammar specifications
package lexer

import (
	"fmt"
	"regexp"
	"strings"
)

// Generator represents a lexer generator that creates Go code for tokenizing input based on EBNF grammar definitions
type Generator struct {
	packageName string
	grammar     string
}

// NewGenerator creates a new lexer generator
func NewGenerator(packageName, grammar string) *Generator {
	return &Generator{
		packageName: packageName,
		grammar:     grammar,
	}
}

// Generate creates the lexer Go code from the EBNF grammar
func (g *Generator) Generate() (string, error) {
	// Parse the EBNF grammar to extract token definitions
	tokenDefinitions, err := g.parseGrammar()
	if err != nil {
		return "", err
	}

	// Generate the Go code for the lexer
	code, err := g.generateLexerCode(tokenDefinitions)
	if err != nil {
		return "", err
	}

	return code, nil
}

// parseGrammar extracts token definitions from the EBNF grammar
func (g *Generator) parseGrammar() (map[string][]string, error) {
	// This is where we'll parse the EBNF grammar to identify tokens
	// Enhanced to handle different grammar formats including custom formats
	tokens := make(map[string][]string)

	lines := strings.Split(g.grammar, "\n")

	// First, check if the grammar has both lexical and syntax sections (indicating structured format)
	hasLexicalSections := false
	hasSyntaxSections := false

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "+Lexical::") {
			hasLexicalSections = true
		} else if strings.HasPrefix(line, "+Syntax::") {
			hasSyntaxSections = true
		}
	}

	// Only use section-based filtering if both lexical and syntax sections are present
	useSectionFiltering := hasLexicalSections && hasSyntaxSections

	// Track if we're in a lexical section (only relevant if section filtering is active)
	inLexicalSection := true

	for _, line := range lines {
		line = strings.TrimSpace(line)

		// Handle sections only if section filtering is active
		if useSectionFiltering {
			if strings.HasPrefix(line, "+Lexical::") {
				inLexicalSection = true
				continue
			} else if strings.HasPrefix(line, "+Syntax::") {
				inLexicalSection = false
				continue
			}
		}

		// Skip empty lines and comment lines
		if line == "" || strings.HasPrefix(line, "//") {
			continue
		}

		// Skip section headers
		if strings.HasPrefix(line, "+") {
			continue
		}

		// If using section filtering and we're not in lexical section, skip this line
		if useSectionFiltering && !inLexicalSection {
			continue
		}

		// Handle both = and ::= as assignment operators
		var parts []string
		if strings.Contains(line, "::=") {
			parts = strings.SplitN(line, "::=", 2)
		} else {
			parts = strings.SplitN(line, "=", 2)
		}

		if len(parts) == 2 {
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])

			// Remove common EBNF syntax elements like semicolons, brackets, etc.
			right = strings.TrimSuffix(right, ";")
			right = strings.ReplaceAll(right, "{", "")
			right = strings.ReplaceAll(right, "}", "")
			right = strings.ReplaceAll(right, "[", "")
			right = strings.ReplaceAll(right, "]", "")

			// Check if this could be a token definition by looking for common patterns
			// Focus on lexical tokens which typically have quoted literals or character classes
			if g.isTokenDefinition(right) {
				// Extract patterns from the right side
				patterns := g.extractTokenPatterns(right)
				if len(patterns) > 0 {
					tokens[left] = patterns
				}
			}
		}
	}

	return tokens, nil
}

// extractTokenPatterns extracts specific patterns from token definitions
func (g *Generator) extractTokenPatterns(definition string) []string {
	var patterns []string
	seen := make(map[string]bool) // Track unique patterns

	// Remove common EBNF operators and extract atomic patterns
	def := definition

	// Handle parenthesized groups
	def = strings.ReplaceAll(def, "(", "")
	def = strings.ReplaceAll(def, ")", "")

	// Handle alternatives (separated by |)
	alternatives := strings.Split(def, "|")

	for _, alt := range alternatives {
		alt = strings.TrimSpace(alt)

		// Handle character classes like [a-z], [A-Z], [0-9], etc.
		charClassRegex := regexp.MustCompile(`\[.*?\]`)
		charClasses := charClassRegex.FindAllString(alt, -1)
		for _, class := range charClasses {
			if !seen[class] {
				patterns = append(patterns, class)
				seen[class] = true
			}
		}

		// Handle quoted strings
		quotedStringRegex := regexp.MustCompile(`"[^"]*"|'[^']*'`)
		quotedStrings := quotedStringRegex.FindAllString(alt, -1)
		for _, str := range quotedStrings {
			if !seen[str] {
				patterns = append(patterns, str)
				seen[str] = true
			}
		}

		// Handle single characters
		singleCharRegex := regexp.MustCompile(`'.'|"[^"]{1}"`)
		singleChars := singleCharRegex.FindAllString(alt, -1)
		for _, ch := range singleChars {
			if !seen[ch] {
				patterns = append(patterns, ch)
				seen[ch] = true
			}
		}
	}

	return patterns
}

// isTokenDefinition checks if a rule definition looks like it defines a token
func (g *Generator) isTokenDefinition(def string) bool {
	defLower := strings.ToLower(def)

	// Contains quotes - likely a literal token
	if strings.Contains(def, `"`) || strings.Contains(def, "'") {
		return true
	}

	// Contains character class patterns like [0-9], [a-zA-Z]
	matched, _ := regexp.MatchString(`\[.+\]`, def)
	if matched {
		return true
	}

	// Look for specific lexical keywords in the rule name or definition
	lexKeywords := []string{
		"keyword", "literal", "const", "ident", "char", "string", "num",
		"int", "float", "comment", "token", "escape", "printable", "digit",
		"letter", "character", "hex", "oct", "bin", "upper", "lower",
	}

	for _, keyword := range lexKeywords {
		if strings.Contains(defLower, keyword) {
			return true
		}
	}

	// If it's a simple rule with alternatives of single characters or quoted strings
	// (e.g., letter = "a" | "b" | "c" or digit = "0" | "1" | "2")
	// This pattern is common in EBNF token definitions
	quotedStringCount := strings.Count(def, `"`) + strings.Count(def, `'`)
	if quotedStringCount >= 2 {  // At least two quoted strings suggest a token list
		return true
	}

	return false
}

// generateLexerCode creates the Go code for the lexer based on token definitions
func (g *Generator) generateLexerCode(tokenDefinitions map[string][]string) (string, error) {
	var code strings.Builder

	// Package declaration
	code.WriteString(fmt.Sprintf("package %s\n\n", g.packageName))

	// Imports - only include those that are actually used
	code.WriteString(`import (
	"errors"
	"fmt"
	"io"
	"unicode"
	"regexp"
)
`)

	// Generate token types
	code.WriteString("\n// TokenType represents the different types of tokens that can be recognized by the lexer.\n")
	code.WriteString("const (\n")
	code.WriteString("\tTokenType_Undefined = iota // TokenType_Undefined represents an undefined or unrecognized token type\n")

	// Add common token types that are used in the lexer implementation
	// These are basic types that should always be available
	commonTypes := []string{
		"Keyword", "Label", "NumberLiteral", "StringLiteral", "CharLiteral",
		"Plus", "Minus", "Asterisk", "ForwardSlash", "OpenParan", "CloseParan",
		"OpenBrace", "CloseBrace", "OpenBracket", "CloseBracket", "Comma",
		"Equal", "Colon", "Period", "Semicolon", "Bang", "EqualEqual", "BangEqual",
		"LessThan", "LessThanEqual", "GreaterThan", "GreaterThanEqual",
	}

	for _, typeName := range commonTypes {
		code.WriteString(fmt.Sprintf("\tTokenType_%s // TokenType_%s represents the %s token\n", typeName, typeName, typeName))
	}

	for name := range tokenDefinitions {
		goName := convertToGoName(name)
		// Only add custom token types that aren't already in the common types
		isCommon := false
		for _, common := range commonTypes {
			if common == goName {
				isCommon = true
				break
			}
		}
		if !isCommon {
			code.WriteString(fmt.Sprintf("\tTokenType_%s // TokenType_%s represents the %s token\n", goName, goName, name))
		}
	}
	code.WriteString(")\n\n")

	// TokenTypeString mapping
	code.WriteString("var (\n")
	code.WriteString("\tTokenTypeString map[int]string = map[int]string{\n")
	code.WriteString("\t\tTokenType_Undefined: \"Undefined\",\n")

	// Add common types to the mapping
	for _, typeName := range []string{
		"Keyword", "Label", "NumberLiteral", "StringLiteral", "CharLiteral",
		"Plus", "Minus", "Asterisk", "ForwardSlash", "OpenParan", "CloseParan",
		"OpenBrace", "CloseBrace", "OpenBracket", "CloseBracket", "Comma",
		"Equal", "Colon", "Period", "Semicolon", "Bang", "EqualEqual", "BangEqual",
		"LessThan", "LessThanEqual", "GreaterThan", "GreaterThanEqual",
	} {
		code.WriteString(fmt.Sprintf("\t\tTokenType_%s: \"%s\",\n", typeName, typeName))
	}

	for name := range tokenDefinitions {
		goName := convertToGoName(name)
		// Only add custom token types that aren't already in the common types
		isCommon := false
		for _, common := range []string{
			"Keyword", "Label", "NumberLiteral", "StringLiteral", "CharLiteral",
			"Plus", "Minus", "Asterisk", "ForwardSlash", "OpenParan", "CloseParan",
			"OpenBrace", "CloseBrace", "OpenBracket", "CloseBracket", "Comma",
			"Equal", "Colon", "Period", "Semicolon", "Bang", "EqualEqual", "BangEqual",
			"LessThan", "LessThanEqual", "GreaterThan", "GreaterThanEqual",
		} {
			if common == goName {
				isCommon = true
				break
			}
		}
		if !isCommon {
			code.WriteString(fmt.Sprintf("\t\tTokenType_%s: \"%s\",\n", goName, goName))
		}
	}
	code.WriteString("\t}\n)\n\n")

	// Add error variables
	code.WriteString("var (\n")
	code.WriteString("\terrUnexpectedEOF = errors.New(\"unexpected EOF\") // errUnexpectedEOF is returned when a string literal is not properly closed\n")
	code.WriteString("\terrInvalidRune   = errors.New(\"invalid rune\")   // errInvalidRune is returned when an invalid character is encountered\n")
	code.WriteString(")\n\n")

	// Token struct
	code.WriteString(`// Token represents a single token from the input string with its type and value.
type Token struct {
	Type  int    // The type of the token as defined by the TokenType constants
	Value string // The actual text value of the token from the input string
}

// String makes Token implement the Stringer interface.
func (t Token) String() string {
	s, ok := TokenTypeString[t.Type]

	if !ok {
		return TokenTypeString[TokenType_Undefined]
	}

	return s
}
`)

	// Create pattern matcher to generate grammar-based token matching code
	patternMatcher := NewPatternMatcher(tokenDefinitions)

	// Generate pattern matching methods
	code.WriteString(patternMatcher.GeneratePatternMatchingCode())

	// Generate helper functions
	code.WriteString(patternMatcher.GenerateHelperFunctions())

	// Lexer struct
	code.WriteString(`// New creates and returns a new Lexer configured to read from the provided input string.
// The lexer will tokenize the input string according to the rules defined in the lexer package.
func New(input string) *Lexer {
	return &Lexer{
		input: []rune(input),
	}
}

// Lexer provides functionality to tokenize an input string into a sequence of tokens.
// It supports peeking at the next token without consuming it and handles various token types
// according to the EBNF grammar definition.
type Lexer struct {
	input []rune // The input string converted to runes for proper Unicode handling
	index int    // Current position in the input
	buf   *Token // Buffered token for peeking functionality
}

// getRune returns the rune at the current index of the input and increments the index.
// If the index is beyond the length of the input, getRune returns an io.EOF error.
func (l *Lexer) getRune() (r rune, err error) {
	if l.index >= len(l.input) {
		return 0, io.EOF
	}

	r = l.input[l.index]
	l.index++
	return r, nil
}

// peekRune returns the rune at the current index of the input without incrementing the index.
// If the index is beyond the length of the input, peekRune returns an io.EOF error.
func (l *Lexer) peekRune() (r rune, err error) {
	if l.index >= len(l.input) {
		return 0, io.EOF
	}

	return l.input[l.index], nil
}

func (l *Lexer) ConsumeToken(expected int) (err error) {
	tok, err := l.GetToken()
	if err != nil {
		return err
	}
	if tok.Type != expected {
		err = fmt.Errorf("unexpected token, wanted %q got %q", TokenTypeString[expected], tok.String())
	}
	return nil
}

// GetToken returns the next token in the input string.
// If there are no more tokens to process in the string, GetToken returns an
// io.EOF error.
func (l *Lexer) GetToken() (tok Token, err error) {
	if l.buf != nil {
		tok = *l.buf
		l.buf = nil
		return tok, nil
	}

	var r rune

	for {
		r, err = l.peekRune()

		if err != nil {
			return tok, err
		}

		if unicode.IsSpace(r) {
			l.index++
			continue
		}

		// Try to match tokens based on grammar definitions first
		var grammarToken Token
		var matched bool
		grammarToken, matched, err = l.matchGrammarToken()
		if err != nil {
			return tok, err
		}
		if matched {
			return grammarToken, nil
		}

		// Handle special single-character tokens that are common
		switch r {
		case '"':
			l.index++
			return l.getTokenStringLiteral()
		case '\'':
			l.index++
			return l.getTokenCharLiteral()
		}

		// Fallback to common token patterns if no grammar-defined tokens matched
		// Check for number literals
		if isNumberStart(r) {
			return l.getTokenNumberLiteral()
		}

		// Check for identifiers/labels
		if isLabelStart(r) {
			return l.getTokenLabel()
		}

		// Check for operators and punctuation tokens
		operatorToken, found := l.checkOperatorToken()
		if found {
			return operatorToken, nil
		}

		// If no known token pattern matched, return an error
		err = fmt.Errorf("%w: %s", errInvalidRune, string(r))
		return
	}
}

// isNumberStart checks if a rune can start a number literal
func isNumberStart(r rune) bool {
	return unicode.IsDigit(r)
}

// isLabelStart checks if a rune can start an identifier/label
func isLabelStart(r rune) bool {
	return unicode.IsLetter(r) || r == '_'
}

// checkOperatorToken checks for multi-character operators
func (l *Lexer) checkOperatorToken() (Token, bool) {
	// Look ahead at more characters to identify multi-character operators
	if l.index+1 < len(l.input) {
		twoChar := string([]rune{l.input[l.index], l.input[l.index+1]})
		switch twoChar {
		case "==", "!=", "<=", ">=":
			l.index += 2
			switch twoChar {
			case "==":
				return Token{Type: TokenType_EqualEqual}, true
			case "!=":
				return Token{Type: TokenType_BangEqual}, true
			case "<=":
				return Token{Type: TokenType_LessThanEqual}, true
			case ">=":
				return Token{Type: TokenType_GreaterThanEqual}, true
			}
		}
	}

	// Check for single character operators
	if l.index < len(l.input) {
		r := l.input[l.index]
		switch r {
		case '+':
			l.index++
			return Token{Type: TokenType_Plus}, true
		case '-':
			l.index++
			return Token{Type: TokenType_Minus}, true
		case '*':
			l.index++
			return Token{Type: TokenType_Asterisk}, true
		case '/':
			// Check for comments (//)
			if l.index+1 < len(l.input) && l.input[l.index+1] == '/' {
				l.index += 2
				l.skipComments()
				return Token{}, false // Continue processing after skipping comment
			}
			l.index++
			return Token{Type: TokenType_ForwardSlash}, true
		case '(':
			l.index++
			return Token{Type: TokenType_OpenParan}, true
		case ')':
			l.index++
			return Token{Type: TokenType_CloseParan}, true
		case '{':
			l.index++
			return Token{Type: TokenType_OpenBrace}, true
		case '}':
			l.index++
			return Token{Type: TokenType_CloseBrace}, true
		case '[':
			l.index++
			return Token{Type: TokenType_OpenBracket}, true
		case ']':
			l.index++
			return Token{Type: TokenType_CloseBracket}, true
		case ',':
			l.index++
			return Token{Type: TokenType_Comma}, true
		case '.':
			l.index++
			return Token{Type: TokenType_Period}, true
		case ':':
			l.index++
			return Token{Type: TokenType_Colon}, true
		case ';':
			l.index++
			return Token{Type: TokenType_Semicolon}, true
		case '=':
			l.index++
			return Token{Type: TokenType_Equal}, true
		case '!':
			l.index++
			return Token{Type: TokenType_Bang}, true
		case '<':
			l.index++
			return Token{Type: TokenType_LessThan}, true
		case '>':
			l.index++
			return Token{Type: TokenType_GreaterThan}, true
		}
	}

	return Token{}, false
}

// getTokenNumberLiteral scans and returns a number literal token from the current position.
// It handles both integer and floating-point numbers (with decimal points).
func (l *Lexer) getTokenNumberLiteral() (tok Token, err error) {
	tok.Type = TokenType_NumberLiteral
	var r rune
	hasDecimalPoint := false

	for {
		r, err = l.peekRune()

		if err == io.EOF {
			return tok, nil
		}

		if err != nil {
			return tok, err
		}

		if unicode.IsNumber(r) {
			l.index++
			tok.Value += string(r)
			continue
		}

		if r == '.' && !hasDecimalPoint {
			l.index++
			tok.Value += string(r)
			hasDecimalPoint = true
			continue
		}

		return tok, nil
	}
}

// getTokenLabel scans and returns a label token from the current position.
// A label is defined as a sequence of letters, numbers, and underscores.
func (l *Lexer) getTokenLabel() (tok Token, err error) {
	tok.Type = TokenType_Label
	var r rune

	for {
		r, err = l.peekRune()

		if err != nil {
			break
		}

		if isLabelRune(r) {
			l.index++
			tok.Value += string(r)
			continue
		}

		break
	}

	// Check if this label is a keyword (for now, assuming some common ones)
	// In a real implementation, we'd check against grammar-defined keywords
	if l.isKeyword(tok.Value) {
		tok.Type = TokenType_Keyword
	}

	if err == io.EOF {
		return tok, nil
	}

	return tok, err
}

// isLabelRune returns whether the provided rune is a valid label rune.
// Valid label runes are letters, numbers, and underscores.
func isLabelRune(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsNumber(r) || r == '_'
}

// isKeyword checks if a value is a keyword
func (l *Lexer) isKeyword(value string) bool {
	keywords := map[string]bool{
		"if":       true,
		"else":     true,
		"while":    true,
		"for":      true,
		"return":   true,
		"func":     true,
		"var":      true,
		"true":     true,
		"false":    true,
		"null":     true,
		"const":    true,
		"let":      true,
		"break":    true,
		"continue": true,
		"do":       true,
		"switch":   true,
		"case":     true,
		"default":  true,
	}
	return keywords[value]
}

// getTokenStringLiteral scans and returns a string literal token from the current position.
func (l *Lexer) getTokenStringLiteral() (tok Token, err error) {
	tok.Type = TokenType_StringLiteral
	var r rune

	for {
		r, err = l.getRune()

		if err == io.EOF {
			err = errUnexpectedEOF
			return
		}

		if err != nil {
			return
		}

		if r == '"' {
			break
		}

		// Handle escape sequences
		if r == '\\' {
			if l.index < len(l.input) {
				nextRune := l.input[l.index]
				l.index++
				switch nextRune {
				case 'n':
					tok.Value += "\n"
				case 't':
					tok.Value += "\t"
				case 'r':
					tok.Value += "\r"
				case '\\':
					tok.Value += "\\"
				case '"':
					tok.Value += "\""
				default:
					tok.Value += string(nextRune)
				}
			} else {
				tok.Value += string(r)
			}
		} else {
			tok.Value += string(r)
		}
	}

	return tok, nil
}

// getTokenCharLiteral scans and returns a character literal token from the current position.
func (l *Lexer) getTokenCharLiteral() (tok Token, err error) {
	tok.Type = TokenType_CharLiteral
	var r rune

	for {
		r, err = l.getRune()

		if err == io.EOF {
			err = errUnexpectedEOF
			return
		}

		if err != nil {
			return
		}

		if r == '\'' {
			break
		}

		// Handle escape sequences
		if r == '\\' {
			if l.index < len(l.input) {
				nextRune := l.input[l.index]
				l.index++
				switch nextRune {
				case 'n':
					tok.Value += "\n"
				case 't':
					tok.Value += "\t"
				case 'r':
					tok.Value += "\r"
				case '\\':
					tok.Value += "\\"
				case '\'':
					tok.Value += "'"
				default:
					tok.Value += string(nextRune)
				}
			} else {
				tok.Value += string(r)
			}
		} else {
			tok.Value += string(r)
		}
	}

	return tok, nil
}

func (l *Lexer) skipComments() error {
	for {
		r, err := l.peekRune()
		if err != nil {
			return err
		}

		if r == '\n' || r == '\r' {
			break
		}
		l.index++
	}

	return nil
}

// PeekToken returns the current token in the input string without advancing the lexer position.
// This allows looking ahead at the next token without consuming it.
// If there are no more tokens to process in the string, PeekToken returns an
// io.EOF error.
func (l *Lexer) PeekToken() (tok Token, err error) {
	if l.buf != nil {
		tok = *l.buf
		return tok, nil
	}

	tok, err = l.GetToken()
	if err != nil {
		return tok, err
	}
	l.buf = &tok
	return tok, err
}
`)

	return code.String(), nil
}

// generateTokenHelperFunctions creates helper functions for token recognition based on grammar patterns
func (g *Generator) generateTokenHelperFunctions(tokenDefinitions map[string][]string) string {
	var code strings.Builder

	// Generate helper functions for each token type
	for name, patterns := range tokenDefinitions {
		goName := convertToGoName(name)

		// Add comment about the function purpose
		code.WriteString(fmt.Sprintf("// is%sRune returns whether the provided rune matches the %s token pattern.\n", goName, name))

		// Generate helper function
		code.WriteString(fmt.Sprintf("func is%sRune(r rune) bool {\n", goName))
		code.WriteString("\t// TODO: Implement specific pattern matching for " + name + " based on grammar\n")

		// For now, we'll add basic pattern matching logic
		// In a real implementation, we'd generate the specific logic based on the patterns
		needsDefault := true
		for _, pattern := range patterns {
			if strings.Contains(pattern, "[") && strings.Contains(pattern, "]") {
				// Character class like [a-zA-Z] or [0-9]
				class := strings.Trim(pattern, "[]")
				if strings.Contains(class, "a-zA-Z") {
					code.WriteString("\tif unicode.IsLetter(r) {\n\t\treturn true\n\t}\n")
					needsDefault = false
				} else if strings.Contains(class, "0-9") {
					code.WriteString("\tif unicode.IsDigit(r) {\n\t\treturn true\n\t}\n")
					needsDefault = false
				}
				// More character class pattern handling would go here
			} else if strings.HasPrefix(pattern, `"`) && strings.HasSuffix(pattern, `"`) {
				// Quoted string literal - would be handled differently in actual implementation
			}
		}

		if needsDefault {
			code.WriteString("\treturn false\n")
		}

		code.WriteString("}\n\n")
	}

	return code.String()
}

// convertToGoName converts a grammar token name to a Go-compatible name
func convertToGoName(name string) string {
	// Remove any non-alphanumeric characters (except underscores) and capitalize appropriately
	result := ""
	capitalize := true
	
	for _, ch := range name {
		if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') {
			if capitalize {
				result += strings.ToUpper(string(ch))
				capitalize = false
			} else {
				result += string(ch)
			}
		} else if ch == '_' || ch == '-' {
			capitalize = true
		} else {
			// For any other character, keep the previous capitalization state
			continue
		}
	}
	
	// If the result is empty or starts with a number, prepend "Token"
	if result == "" || (len(result) > 0 && result[0] >= '0' && result[0] <= '9') {
		result = "Token" + result
	}
	
	return result
}