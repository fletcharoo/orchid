// Package lexer provides functionality for matching token patterns extracted from EBNF grammar
package lexer

import (
	"fmt"
	"regexp"
	"strings"
)

// PatternMatcher represents a pattern matching system for grammar-based token recognition
type PatternMatcher struct {
	patterns map[string][]string // maps token names to their pattern definitions
}

// NewPatternMatcher creates a new pattern matcher
func NewPatternMatcher(tokenDefinitions map[string][]string) *PatternMatcher {
	return &PatternMatcher{
		patterns: tokenDefinitions,
	}
}

// GeneratePatternMatchingCode generates Go code for pattern matching based on grammar definitions
func (pm *PatternMatcher) GeneratePatternMatchingCode() string {
	var code strings.Builder

	// Generate the matchGrammarToken method with grammar-specific logic
	code.WriteString(`// matchGrammarToken attempts to match tokens based on the grammar definitions
func (l *Lexer) matchGrammarToken() (Token, bool, error) {
	// Try to match the longest possible token from current position
	longestMatch := ""
	longestTokenType := TokenType_Undefined
	matchFound := false

	// Check each grammar-defined token pattern starting from the current position
	inputRemaining := string(l.input[l.index:])

	// Define regex patterns for each token type based on grammar
`)

	// Generate pattern matching code for each token type
	for tokenName, patterns := range pm.patterns {
		goName := convertToGoName(tokenName)
		code.WriteString(fmt.Sprintf("\t// Try matching %s tokens\n", tokenName))
		
		for _, pattern := range patterns {
			// Convert EBNF pattern to regex pattern
			regexPattern := pm.ebnfToRegex(pattern)
			if regexPattern != "" {
				code.WriteString(fmt.Sprintf("\t{ // Matching %s\n", tokenName))
				code.WriteString(fmt.Sprintf("\t\tre := regexp.MustCompile(`^%s`)\n", regexPattern))
				code.WriteString("\t\tif matches := re.FindStringSubmatch(inputRemaining); len(matches) > 0 {\n")
				code.WriteString("\t\t\tmatch := matches[0]\n")
				code.WriteString(fmt.Sprintf("\t\t\tif len(match) > len(longestMatch) {\n"))
				code.WriteString(fmt.Sprintf("\t\t\t\tlongestMatch = match\n"))
				code.WriteString(fmt.Sprintf("\t\t\t\tlongestTokenType = TokenType_%s\n", goName))
				code.WriteString("\t\t\t\tmatchFound = true\n")
				code.WriteString("\t\t\t}\n")
				code.WriteString("\t\t}\n")
				code.WriteString("\t}\n")
			}
		}
	}

	code.WriteString(`
	if matchFound {
		// Consume the matched text
		matchRunes := []rune(longestMatch)
		for range matchRunes {
			l.getRune() // This advances the index
		}
		return Token{Type: longestTokenType, Value: longestMatch}, true, nil
	}

	// No grammar-defined token matched at this position
	return Token{}, false, nil
}
`)

	return code.String()
}

// ebnfToRegex converts an EBNF pattern to a Go regex pattern
func (pm *PatternMatcher) ebnfToRegex(ebnfPattern string) string {
	// Handle character classes like [a-zA-Z], [0-9], etc.
	if strings.HasPrefix(ebnfPattern, "[") && strings.HasSuffix(ebnfPattern, "]") {
		// Already in regex format, return as is with quantifier for matching one or more
		return ebnfPattern + "+"  // Match one or more of the character class
	}

	// Handle quoted strings - these are literals
	if (strings.HasPrefix(ebnfPattern, `"`) && strings.HasSuffix(ebnfPattern, `"`)) ||
		(strings.HasPrefix(ebnfPattern, "'") && strings.HasSuffix(ebnfPattern, "'")) {
		// Extract the literal content and escape it for regex
		content := ebnfPattern[1 : len(ebnfPattern)-1] // Remove quotes
		return regexp.QuoteMeta(content)
	}

	// Handle common abbreviations
	switch strings.ToLower(strings.TrimSpace(ebnfPattern)) {
	case "digit", "[0-9]":
		return `[0-9]+`
	case "digits", "number":
		return `[0-9]+`
	case "letter", "[a-za-z]":
		return `[a-zA-Z]+`
	case "alnum", "alphanumeric":
		return `[a-zA-Z0-9]+`
	case "word", "identifier":
		return `[a-zA-Z_][a-zA-Z0-9_]*`
	case "whitespace":
		return `\s+`
	case "integer":
		return `[+-]?[0-9]+`
	case "float", "real":
		return `[+-]?[0-9]*\.?[0-9]+`
	case "string", "str":
		return `"([^"\\]|\\.)*"` // Basic string pattern
	}

	// For more complex patterns, we might need to parse the EBNF more thoroughly
	// For now, return an empty string to indicate we can't convert this pattern
	return ""
}

// GenerateHelperFunctions generates helper functions for token recognition
func (pm *PatternMatcher) GenerateHelperFunctions() string {
	var code strings.Builder

	// Generate helper functions for character classes defined in the grammar
	// This would create functions like isKeywordRune, isOperatorRune, etc.
	
	for tokenName := range pm.patterns {
		goName := convertToGoName(tokenName)
		
		// For now, we'll add basic helper functions for common token types
		// In a full implementation, these would be generated based on the actual patterns
		code.WriteString(fmt.Sprintf(`
// is%sRune returns whether the provided rune is part of a %s token.
func is%sRune(r rune) bool {
	// This function would be generated based on the grammar patterns for %s
	// For now, this is a placeholder
	switch r {
	case 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 
		 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
		 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
		 '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_':
		return true
	default:
		return false
	}
}
`, goName, tokenName, goName, tokenName))
	}

	return code.String()
}