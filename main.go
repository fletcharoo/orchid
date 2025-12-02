// Package main provides the orchid command-line tool for generating parsers from EBNF grammars
package main

import (
	"flag"
	"fmt"
	"os"
	"orchid/internal/lexer"
	"io/ioutil"
)

func main() {
	var (
		grammarFile = flag.String("g", "", "EBNF grammar file path")
		packageName = flag.String("p", "parser", "Package name for generated code")
		outputDir   = flag.String("o", "", "Output directory for generated files (default: same as grammar file)")
	)
	
	flag.Parse()
	
	if *grammarFile == "" {
		fmt.Fprintf(os.Stderr, "Error: grammar file is required (-g flag)\n")
		flag.Usage()
		os.Exit(1)
	}
	
	// Read the grammar file
	grammar, err := ioutil.ReadFile(*grammarFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading grammar file: %v\n", err)
		os.Exit(1)
	}
	
	// Generate the lexer
	generator := lexer.NewGenerator(*packageName, string(grammar))
	code, err := generator.Generate()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error generating lexer: %v\n", err)
		os.Exit(1)
	}
	
	// Determine output file path
	outputPath := *outputDir
	if outputPath == "" {
		// Default to same directory as grammar file
		outputPath = *grammarFile
	}
	
	// Write the generated code to file
	outputFile := fmt.Sprintf("%s_lexer.go", *packageName)
	if *outputDir != "" {
		outputFile = fmt.Sprintf("%s/%s_lexer.go", *outputDir, *packageName)
	} else {
		// Extract directory from grammar file path
		dir := ""
		for i := len(*grammarFile) - 1; i >= 0; i-- {
			if (*grammarFile)[i] == '/' {
				dir = (*grammarFile)[:i+1]
				break
			}
		}
		outputFile = fmt.Sprintf("%s%s_lexer.go", dir, *packageName)
	}
	
	err = ioutil.WriteFile(outputFile, []byte(code), 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error writing generated file: %v\n", err)
		os.Exit(1)
	}
	
	fmt.Printf("Generated lexer code written to: %s\n", outputFile)
}