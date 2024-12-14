#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <unordered_map>
#include <regex>
// #include <sstream>
// #include <stdexcept>

using namespace std;

/*
 * Make sure in each class to put functions in private accordingly.
 * Automatic type conversion / coercion.
 * split into flies / helper functions.
 */

enum TokenType
{
    T_INT,
    T_FLOAT,
    T_DOUBLE,
    T_CHAR,
    T_BOOL,
    T_STRING,
    T_ID,
    T_NUM,
    T_IF,
    T_ELIF,
    T_ELSE,
    T_RETURN,
    T_WHILE,
    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_GT,
    T_LT,
    T_EQ,
    T_NE,
    T_EOF
};

unordered_map<TokenType, string> TypeMapping = {
    {T_INT, "int"},
    {T_FLOAT, "float"},
    {T_DOUBLE, "double"},
    {T_CHAR, "char"},
    {T_STRING, "string"},
    {T_BOOL, "bool"}};

struct Token
{
    TokenType type;
    string value;
    int lineNumber;
};

class Lexer
{
private:
    string src;
    size_t pos;
    int lineNumber;

public:
    Lexer(const string &src) : src(src), pos(0), lineNumber(1) {}

    vector<Token> tokenize()
    {
        vector<Token> tokens;
        while (pos < src.size())
        {
            char current = src[pos];

            if (current == '\n')
            {
                lineNumber++;
                pos++;
                continue;
            }
            if (isspace(current))
            {
                pos++;
                continue;
            }
            if (isdigit(current))
            {
                tokens.push_back(Token{T_NUM, consumeNumber(), lineNumber});
                continue;
            }
            if (isalpha(current))
            {
                string word = consumeWord();
                TokenType type = T_ID;

                if (word == "int")
                    type = T_INT;
                else if (word == "float")
                    type = T_FLOAT;
                else if (word == "if")
                    type = T_IF;
                else if (word == "else")
                    type = T_ELSE;
                else if (word == "while")
                    type = T_WHILE;
                else if (word == "return")
                    type = T_RETURN;

                tokens.push_back(Token{type, word, lineNumber});
                continue;
            }

            switch (current)
            {
            case '=':
                tokens.push_back(Token{T_ASSIGN, "=", lineNumber});
                break;
            case '+':
                tokens.push_back(Token{T_PLUS, "+", lineNumber});
                break;
            case '-':
                tokens.push_back(Token{T_MINUS, "-", lineNumber});
                break;
            case '*':
                tokens.push_back(Token{T_MUL, "*", lineNumber});
                break;
            case '/':
                tokens.push_back(Token{T_DIV, "/", lineNumber});
                break;
            case '(':
                tokens.push_back(Token{T_LPAREN, "(", lineNumber});
                break;
            case ')':
                tokens.push_back(Token{T_RPAREN, ")", lineNumber});
                break;
            case '{':
                tokens.push_back(Token{T_LBRACE, "{", lineNumber});
                break;
            case '}':
                tokens.push_back(Token{T_RBRACE, "}", lineNumber});
                break;
            case ';':
                tokens.push_back(Token{T_SEMICOLON, ";", lineNumber});
                break;
            case '>':
                tokens.push_back(Token{T_GT, ">", lineNumber});
                break;
            case '<':
                tokens.push_back(Token{T_LT, "<", lineNumber});
                break;
            default:
                cout << "Unexpected character: " << current << " at line " << lineNumber << endl;
                exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", lineNumber});
        return tokens;
    }

    string consumeNumber()
    {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }

    string consumeWord()
    {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }
};

/*
    SymbolTable class:

    The SymbolTable class manages a collection of variable names and their associated data types.
    It serves as a fundamental part of a compiler or interpreter, ensuring that variables are properly declared
    and accessed with their correct types during semantic analysis.


    Member Functions:
    1. declareVariable(const string &name, const string &type):
       - Purpose: Declares a new variable with a specified name and type.
       - This function checks if the variable is already declared. If it is, a runtime error is thrown indicating
         that the variable has already been declared.
       - If the variable is not already declared, it adds the variable's name as a key and its type as the value
         to the `symbolTable` map.
       - It will throw a runtime error if the variable already exists in the symbol table.
       - Example usage: Declare a new variable `x` with type `int`.

       if (symbolTable.find(name) != symbolTable.end()) {
           throw runtime_error("Semantic error: Variable '" + name + "' is already declared.");
       }
       symbolTable[name] = type;

    2. getVariableType(const string &name):
       - Purpose: Returns the type of a variable given its name.
       - This function checks if the variable exists in the `symbolTable`. If the variable is not found, it throws
         a runtime error indicating that the variable has not been declared yet.
       - If the variable is declared, it retrieves and returns its associated type from the symbol table.
       - Throws a runtime error if the variable is not found.
       - Example usage: Get the type of variable `x`.

       if (symbolTable.find(name) == symbolTable.end()) {
           throw runtime_error("Semantic error: Variable '" + name + "' is not declared.");
       }
       return symbolTable[name];

    3. isDeclared(const string &name) const:
       - Purpose: Checks whether a variable has been declared.
       - This function returns a boolean value indicating whether the variable exists in the `symbolTable`.
       - It is a quick way to check the existence of a variable without retrieving its type.
       - Example usage: Check if variable `x` is declared.

       return symbolTable.find(name) != symbolTable.end();

    Private Data Members:

    - map<string, string> symbolTable:
      - A `map` that stores variable names (strings) as keys and their associated types (strings) as values.
      - This map allows efficient lookups to check if a variable is declared and to retrieve its type.

    Usage in a Compiler or Interpreter:
    - The `SymbolTable` is crucial for ensuring that variables are used consistently and correctly in a program.
    - When a variable is declared, it is added to the symbol table, and its type is stored.
    - When a variable is referenced, the symbol table is consulted to ensure that it has been declared and to retrieve its type.
    - The symbol table helps detect semantic errors such as undeclared variables or redeclared variables.
*/
class SymbolTable
{
public:
    void declareVariable(const string &name, const string &type)
    {
        if (symbolTable.find(name) != symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name + "' is already declared.");
        }
        symbolTable[name] = type;
    }

    string getVariableType(const string &name)
    {
        if (symbolTable.find(name) == symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name + "' is not declared.");
        }
        return symbolTable[name];
    }

    bool isDeclared(const string &name) const
    {
        return symbolTable.find(name) != symbolTable.end();
    }

    void printSymbolTable() const
    {
        if (symbolTable.empty())
        {
            cout << "Symbol table is empty." << endl;
            return;
        }

        cout << "SYMBOL TABLE:" << endl;
        for (const auto &entry : symbolTable)
        {
            cout << "Variable: " << entry.first << "\t\t Type: " << entry.second << endl;
        }

        cout << "\n\n";
    }

private:
    map<string, string> symbolTable;
};

class IntermediateCodeGnerator
{
public:
    vector<string> instructions;
    int tempCount = 0;
    int labelCounter = 1;

    vector<string> getInstructions()
    {
        return instructions;
    }

    string newTemp()
    {
        return "t" + to_string(tempCount++);
    }

    string generateLabel()
    {
        return "L" + to_string(labelCounter++);
    }

    void addInstruction(const string &instr)
    {
        instructions.push_back(instr);
    }

    void printInstructions()
    {
        cout << "INTERMEDIATE CODE: \n";
        for (const auto &instr : instructions)
        {
            cout << instr << endl;
        }

        cout << "\n\n";
    }
};

class Parser
{
public:
    // Constructor
    Parser(const vector<Token> &tokens, SymbolTable &symTable, IntermediateCodeGnerator &icg)
        : tokens(tokens), pos(0), symTable(symTable), icg(icg) {}
    // here the private member of this class are being initalized with the arguments passed to this constructor

    void parseProgram()
    {
        while (tokens[pos].type != T_EOF)
        {
            parseStatement();
        }
    }

private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable &symTable;
    IntermediateCodeGnerator &icg;

    void parseStatement()
    {
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT)
        {
            parseDeclaration(tokens[pos].type);
        }
        else if (tokens[pos].type == T_ID)
        {
            parseAssignment();
        }
        else if (tokens[pos].type == T_IF)
        {
            parseIfStatement();
        }
        else if (tokens[pos].type == T_WHILE)
        {
            parseWhileStatement();
        }
        else if (tokens[pos].type == T_RETURN)
        {
            parseReturnStatement();
        }
        else if (tokens[pos].type == T_LBRACE)
        {
            parseBlock();
        }
        else
        {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }

    /*
     parseDeclaration handles the parsing of variable declarations.
     It expects the token type to be `T_INT` (for declaring an integer type variable),
     followed by an identifier (variable name), and a semicolon to terminate the statement.
     It also registers the declared variable in the symbol table with type "int".
     Example:
     int x;   // This will be parsed and the symbol table will store x with type "int".
    */
    void parseDeclaration(TokenType varType)
    {
        expect(varType);                             // Expect and consume the int keyword.
        string varName = expectAndReturnValue(T_ID); // Expect and return the variable name (identifier).
        string type = TypeMapping.at(varType);
        symTable.declareVariable(varName, type); // Register the variable in the symbol table with type "int".

        if (check(T_ASSIGN))
            handleInlineAssignment(varName);

        expect(T_SEMICOLON); // Expect the semicolon to end the statement.
    }

    void handleInlineAssignment(string varName)
    {
        expect(T_ASSIGN);
        string expr = parseExpression();
        icg.addInstruction(varName + " = " + expr);
    }

    /*
     parseAssignment handles the parsing of variable assignments.
     It expects an identifier (variable name), an assignment operator `=`, followed by an expression,
     and a semicolon at the end of the statement.
     It checks if the variable is declared in the symbol table, parses the expression, and generates intermediate code
     for the assignment.
     Example:
     x = 10;   -->  This will be parsed, checking if x is declared, then generating intermediate code like `x = 10`.
    */
    void parseAssignment()
    {
        string varName = expectAndReturnValue(T_ID);
        symTable.getVariableType(varName); // Ensure the variable is declared in the symbol table.
        expect(T_ASSIGN);
        string expr = parseExpression();
        icg.addInstruction(varName + " = " + expr); // Generate intermediate code for the assignment.
        expect(T_SEMICOLON);
    }
    /*
         parseIfStatement handles the parsing of `if` statements.
         It expects the keyword `if`, followed by an expression in parentheses that serves as the condition.
         If the condition evaluates to true, it executes the statement inside the block. If an `else` part is present,
         it executes the corresponding statement after the `else` keyword.
         Intermediate code for the `if` statement is generated, including labels for conditional jumps.
         Example:
         if(5 > 3) { x = 20; }  --> This will generate intermediate code for the condition check and jump instructions.
    */
    void parseIfStatement()
    {
        expect(T_IF);                    // Expect and consume the 'if' keyword.
        expect(T_LPAREN);                // Expect and consume the opening parenthesis for the condition.
        string cond = parseExpression(); // Parse the condition expression inside the parentheses.
        expect(T_RPAREN);                // Expect and consume the closing parenthesis.

        string temp = icg.newTemp();             // Generate a new temporary variable for the condition result.
        icg.addInstruction(temp + " = " + cond); // Generate intermediate code for evaluating the condition.

        string trueLabel = icg.generateLabel();  // Generate a label for the "true" branch.
        string falseLabel = icg.generateLabel(); // Generate a label for the "false" branch.
        string endLabel = "";                    // End label for all branches, generated only if needed.

        icg.addInstruction("if " + temp + " goto " + trueLabel); // Branch to trueLabel if condition is true.
        icg.addInstruction("goto " + falseLabel);                // Otherwise, branch to falseLabel.

        // True branch
        icg.addInstruction(trueLabel + ":");
        parseStatement(); // Parse the statement inside the "if" block.

        // Handle optional "else" or "else if"
        while (tokens[pos].type == T_ELSE)
        {
            endLabel = icg.generateLabel();         // Generate an end label for after the "else" or "else if" block.
            icg.addInstruction("goto " + endLabel); // Jump to endLabel after the previous block.

            icg.addInstruction(falseLabel + ":"); // Label for the "else" or "else if" block.
            expect(T_ELSE);                       // Consume the "else" keyword.

            // Check if it's an "else if"
            if (tokens[pos].type == T_IF)
            {
                expect(T_IF);                        // Consume the 'if' keyword.
                expect(T_LPAREN);                    // Expect and consume the opening parenthesis.
                string elifCond = parseExpression(); // Parse the "else if" condition.
                expect(T_RPAREN);                    // Expect and consume the closing parenthesis.

                string elifTemp = icg.newTemp(); // Generate a temporary variable for the "else if" condition.
                icg.addInstruction(elifTemp + " = " + elifCond);

                trueLabel = icg.generateLabel();  // Generate a new label for the "else if" true branch.
                falseLabel = icg.generateLabel(); // Generate a new label for the next block.

                icg.addInstruction("if " + elifTemp + " goto " + trueLabel);
                icg.addInstruction("goto " + falseLabel);

                icg.addInstruction(trueLabel + ":");
                parseStatement(); // Parse the statement inside the "else if" block.
            }
            else
            {
                parseStatement(); // Parse the statement inside the plain "else" block.
                break;            // Break the loop as there are no further "else if" branches.
            }
        }

        if (!endLabel.empty())
        {
            icg.addInstruction(endLabel + ":"); // Add the end label after the entire chain.
        }
        else
        {
            icg.addInstruction(falseLabel + ":"); // Label for the false branch if no "else" or "else if".
        }
    }

    void parseWhileStatement()
    {
        expect(T_WHILE);                 // Expect and consume the 'while' keyword.
        expect(T_LPAREN);                // Expect and consume the opening parenthesis for the condition.
        string cond = parseExpression(); // Parse the condition expression inside the parentheses.
        expect(T_RPAREN);                // Expect and consume the closing parenthesis.

        string condLabel = icg.generateLabel();  // Label for checking the condition at the start of the loop.
        string trueLabel = icg.generateLabel();  // Label for the "true" branch (loop body).
        string falseLabel = icg.generateLabel(); // Label for the "false" branch (exit).

        icg.addInstruction(condLabel + ":");
        string temp = icg.newTemp();             // Generate a temporary variable for the condition result.
        icg.addInstruction(temp + " = " + cond); // Generate intermediate code for evaluating the condition.

        icg.addInstruction("if " + temp + " goto " + trueLabel); // If condition is true, jump to loop body.
        icg.addInstruction("goto " + falseLabel);                // If condition is false, exit the loop.

        // True branch (loop body)
        icg.addInstruction(trueLabel + ":");
        parseStatement(); // Parse the statement inside the "while" block.

        icg.addInstruction("goto " + condLabel); // Jump back to condition evaluation.
        icg.addInstruction(falseLabel + ":");    // Label for the false branch (end of loop).
    }

    /*
        parseReturnStatement handles the parsing of `return` statements.
        It expects the keyword `return`, followed by an expression to return, and a semicolon to terminate the statement.
        It generates intermediate code to represent the return of the expression.
        Example:
        return x + 5;   -->  This will generate intermediate code like `return x + 5`.
    */
    void parseReturnStatement()
    {
        expect(T_RETURN);
        string expr = parseExpression();
        icg.addInstruction("return " + expr); // Generate intermediate code for the return statement.
        expect(T_SEMICOLON);
    }
    /*
        parseBlock handles the parsing of block statements, which are enclosed in curly braces `{ }`.
        It parses the statements inside the block recursively until it reaches the closing brace.
        Example:
        { x = 10; y = 20; }   -->  This will parse each statement inside the block.
    */
    void parseBlock()
    {
        expect(T_LBRACE); // Expect and consume the opening brace `{`.
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement(); // Parse the statements inside the block.
        }
        expect(T_RBRACE);
    }
    /*
        parseExpression handles the parsing of expressions involving addition, subtraction, or comparison operations.
        It first parses a term, then processes addition (`+`) or subtraction (`-`) operators if present, generating
        intermediate code for the operations.
        Example:
        5 + 3 - 2;  -->  This will generate intermediate code like `t0 = 5 + 3` and `t1 = t0 - 2`.
    */
    string parseExpression()
    {
        string term = parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS)
        {
            TokenType op = tokens[pos++].type;
            string nextTerm = parseTerm();                                                       // Parse the next term in the expression.
            string temp = icg.newTemp();                                                         // Generate a temporary variable for the result
            icg.addInstruction(temp + " = " + term + (op == T_PLUS ? " + " : " - ") + nextTerm); // Intermediate code for operation
            term = temp;
        }
        if (tokens[pos].type == T_GT || tokens[pos].type == T_LT)
        {
            string op = (tokens[pos].type == T_GT) ? ">" : "<";                  // Determine the operator based on the token type.
            pos++;                                                               // Consume the operator.
            string nextExpr = parseExpression();                                 // Parse the next expression for the comparison.
            string temp = icg.newTemp();                                         // Generate a temporary variable for the result.
            icg.addInstruction(temp + " = " + term + " " + op + " " + nextExpr); // Intermediate code for the comparison.
            term = temp;                                                         // Update term to the temporary variable holding the result.
        }

        return term;
    }
    /*
        parseTerm handles the parsing of terms involving multiplication or division operations.
        It first parses a factor, then processes multiplication (`*`) or division (`/`) operators if present,
        generating intermediate code for the operations.
        Example:
        5 * 3 / 2;   This will generate intermediate code like `t0 = 5 * 3` and `t1 = t0 / 2`.
    */
    string parseTerm()
    {
        string factor = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV)
        {
            TokenType op = tokens[pos++].type;
            string nextFactor = parseFactor();
            string temp = icg.newTemp();                                                            // Generate a temporary variable for the result.
            icg.addInstruction(temp + " = " + factor + (op == T_MUL ? " * " : " / ") + nextFactor); // Intermediate code for operation.
            factor = temp;                                                                          // Update the factor to be the temporary result.
        }
        return factor;
    }
    /*
        parseFactor handles the parsing of factors in expressions, which can be either numeric literals, identifiers
        (variables), or expressions inside parentheses (for sub-expressions).
        Example:
        5;          -->  This will return the number "5".
        x;          -->  This will return the identifier "x".
        (5 + 3);    --> This will return the sub-expression "5 + 3".
    */
    string parseFactor()
    {
        if (tokens[pos].type == T_NUM)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_ID)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_LPAREN)
        {
            expect(T_LPAREN);
            string expr = parseExpression();
            expect(T_RPAREN);
            return expr;
        }
        else
        {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }
    /*
        expect function:
        This functin is used to check whether the current token matches the expected type.
        If the token type does not match the expected type, an error message is displayed
        and the program exits. If the token type matches, it advances the position to the next token.
    */
    void expect(TokenType type)
    {
        if (tokens[pos].type != type)
        {
            cout << "Syntax error: expected '" << type << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
        pos++;
    }
    void expectAny(const vector<TokenType> &types)
    {
        for (const auto &type : types)
        {
            if (tokens[pos].type == type)
            {
            }
        }
    }
    /*
        check function:
        Checks if a certain token present at next position. Need for extended usecases like assignment occuring
        simultaneously in a declaration statement
    */
    bool check(TokenType type)
    {
        return tokens[pos].type == type;
    }
    bool existsIn(const string word, const vector<string> collection)
    {
        return find(collection.begin(), collection.end(), word) != collection.end();
    }
    vector<string> getKeys(const unordered_map<string, TokenType> &map)
    {
        vector<string> keys;
        for (const auto &pair : map)
        {
            keys.push_back(pair.first);
        }

        return keys;
    }
    /*
    Explanation:
    - The `expect` function ensures that the parser encounters the correct tokens in the expected order.
    - It's mainly used for non-value-based tokens, such as keywords, operators, and delimiters (e.g., semicolons).
    - If the parser encounters an unexpected token, it halts the process by printing an error message, indicating where the error occurred (line number) and what was expected.
    - The `pos++` advances to the next token after confirming the expected token is present.

    Use Case:
    - This function is helpful when checking for the correct syntax or structure in a language's grammar, ensuring the parser processes the tokens in the correct order.
    */
    string expectAndReturnValue(TokenType type)
    {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }

    /*
        Why both functions are needed:
        - The `expect` function is useful when you are only concerned with ensuring the correct token type without needing its value.
        - For example, ensuring a semicolon `;` or a keyword `if` is present in the source code.
        - The `expectAndReturnValue` function is needed when the parser not only needs to check for a specific token but also needs to use the value of that token in the next stages of compilation or interpretation.
        - For example, extracting the name of a variable (`T_ID`) or the value of a constant (`T_NUMBER`) to process it in a symbol table or during expression evaluation.
    */
};

class AssemblyCodeGenerator
{
private:
    vector<string> assemblyCode;
    int labelCounter;
    unordered_map<string, string> variableRegisterMap;
    int registerCounter = 0;

    string allocateRegister(const string &var)
    {
        if (variableRegisterMap.find(var) == variableRegisterMap.end())
        {
            variableRegisterMap[var] = "r" + to_string(registerCounter++);
        }
        return variableRegisterMap[var];
    }

    string generateLabel()
    {
        return "L" + to_string(labelCounter++);
    }

    void addInstruction(const string &instruction)
    {
        assemblyCode.push_back(instruction);
    }

    void processAssignment(const string &line)
    {
        auto eqPos = line.find("=");
        string tVar = line.substr(0, eqPos);
        string value = line.substr(eqPos + 1);

        tVar = trim(tVar);
        value = trim(value);

        string targetRegister = allocateRegister(tVar);

        // Direct assignment
        if (isdigit(value[0]))
        {
            addInstruction("li " + targetRegister + ", " + value);
        }
        else
        {
            string sourceRegister = allocateRegister(value);
            addInstruction("mv " + targetRegister + ", " + sourceRegister);
        }
    }

    void processBinaryOperation(const string &line)
    {
        static const unordered_map<string, string> operationMap = {
            {"+", "add"},
            {"-", "sub"},
            {"*", "mul"},
            {"/", "div"}};

        auto eqPos = line.find("=");
        string tVar = line.substr(0, eqPos);
        string expr = line.substr(eqPos + 1);

        string op;
        if (expr.find("+") != string::npos)
            op = "+";
        else if (expr.find("-") != string::npos)
            op = "-";
        else if (expr.find("*") != string::npos)
            op = "*";
        else if (expr.find("/") != string::npos)
            op = "/";

        auto opPos = expr.find(op);
        string lhs = expr.substr(0, opPos);
        string rhs = expr.substr(opPos + 1);
        string operation = operationMap.at(op);

        tVar = trim(tVar);
        lhs = trim(lhs);
        rhs = trim(rhs);

        string targetRegister = allocateRegister(tVar);
        string lhsRegister = isdigit(lhs[0]) ? "t" + to_string(registerCounter++) : allocateRegister(lhs);
        string rhsRegister = isdigit(rhs[0]) ? "t" + to_string(registerCounter++) : allocateRegister(rhs);

        if (isdigit(lhs[0]))
            addInstruction("li " + lhsRegister + ", " + lhs);
        if (isdigit(rhs[0]))
            addInstruction("li " + rhsRegister + ", " + rhs);

        addInstruction(operation + " " + targetRegister + ", " + lhsRegister + ", " + rhsRegister);
    }

    void processRelationalOperation(const string &line)
    {
        auto eqPos = line.find("=");
        string tVar = line.substr(0, eqPos);
        string expr = line.substr(eqPos + 1);

        tVar = trim(tVar);

        // Detect relational operator
        string op;
        if (expr.find(">=") != string::npos)
            op = ">=";
        else if (expr.find("<=") != string::npos)
            op = "<=";
        else if (expr.find("==") != string::npos)
            op = "==";
        else if (expr.find("!=") != string::npos)
            op = "!=";
        else if (expr.find(">") != string::npos)
            op = ">";
        else if (expr.find("<") != string::npos)
            op = "<";

        // Split the expression
        auto opPos = expr.find(op);
        string lhs = expr.substr(0, opPos);
        string rhs = expr.substr(opPos + op.size());

        lhs = trim(lhs);
        rhs = trim(rhs);

        // Map variable registers
        string lhsReg = allocateRegister(lhs);
        string rhsReg = allocateRegister(rhs);
        string resultReg = allocateRegister(tVar);

        // Generate assembly for relational operator
        if (op == ">")
        {
            addInstruction("sgt " + resultReg + ", " + lhsReg + ", " + rhsReg);
        }
        else if (op == "<")
        {
            addInstruction("slt " + resultReg + ", " + lhsReg + ", " + rhsReg);
        }
        else if (op == ">=")
        {
            addInstruction("slt " + resultReg + ", " + lhsReg + ", " + rhsReg);
            addInstruction("seqz " + resultReg + ", " + resultReg);
        }
        else if (op == "<=")
        {
            addInstruction("sgt " + resultReg + ", " + lhsReg + ", " + rhsReg);
            addInstruction("seqz " + resultReg + ", " + resultReg);
        }
        else if (op == "==")
        {
            addInstruction("sub " + resultReg + ", " + lhsReg + ", " + rhsReg);
            addInstruction("seqz " + resultReg + ", " + resultReg);
        }
        else if (op == "!=")
        {
            addInstruction("sub " + resultReg + ", " + lhsReg + ", " + rhsReg);
            addInstruction("snez " + resultReg + ", " + resultReg);
        }
    }

    void processConditional(const string &line)
    {
        auto ifPos = line.find("if");
        auto gotoPos = line.find("goto");
        string condition = line.substr(ifPos + 2, gotoPos - (ifPos + 2));
        string label = line.substr(gotoPos + 4);

        condition = trim(condition);
        label = trim(label);

        addInstruction("bnez t2, " + label);
    }

    void processGoto(const string &line)
    {
        auto gotoPos = line.find("goto");
        string label = line.substr(gotoPos + 4);
        label = trim(label);

        addInstruction("j " + label);
    }

    void processLabel(const string &line)
    {
        addInstruction(line);
    }

    string trim(const string &str)
    {
        auto start = str.find_first_not_of(" \t");
        auto end = str.find_last_not_of(" \t");
        return (start == string::npos) ? "" : str.substr(start, end - start + 1);
    }

public:
    AssemblyCodeGenerator() : labelCounter(0) {}

    void generateAssembly(const vector<string> &intermediateCode)
    {
        for (const auto &line : intermediateCode)
        {
            string Line = trim(line); // Trim leading and trailing spaces/tabs
            if (Line.empty())
                continue; // Skip empty lines

            if (Line.find("=") != string::npos)
            {
                if (Line.find(">") != string::npos || Line.find("<") != string::npos ||
                    Line.find("==") != string::npos || Line.find(">=") != string::npos ||
                    Line.find("<=") != string::npos || Line.find("!=") != string::npos)
                {
                    // Handle relational operation
                    processRelationalOperation(Line);
                }
                else if (Line.find("+") != string::npos ||
                         Line.find("-") != string::npos ||
                         Line.find("*") != string::npos ||
                         Line.find("/") != string::npos)
                {
                    // Handle binary operation
                    processBinaryOperation(Line);
                }
                else
                {
                    // Handle simple assignment
                    processAssignment(Line);
                }
            }
            else if (Line.find("if") == 0)
            {
                processConditional(Line); // Handle conditional
            }
            else if (Line.find("goto") == 0)
            {
                processGoto(Line); // Handle goto
            }
            else if (Line.back() == ':')
            {
                processLabel(Line); // Handle label
            }
        }
    }

    void printAssemblyCode()
    {
        cout << "ASSEMBLY CODE:" << endl;
        for (const auto &instruction : assemblyCode)
        {
            cout << instruction << "\n";
        }

        cout << "\n\n";
    }
};
class Optimizer
{

private:
    unordered_map<string, string> symbolTable; // Stores variable names and their types

public:
    string eliminateDeadCode(const string &code)
    {
        cout << "Eliminating dead code in: " << code << endl;

        // Remove unreachable code after 'return'
        regex unreachableCodePattern(R"(return\s+.*?;.*)");
        string optimizedCode = regex_replace(code, unreachableCodePattern, "$1");

        // Remove unused variable declarations (simple example)
        regex unusedVariablePattern(R"(int\s+(\w+)\s*=\s*.*?;)");
        optimizedCode = regex_replace(optimizedCode, unusedVariablePattern, "");

        // Remove reassignment of the same variable
        regex redundantAssignmentPattern(R"((\w+)\s*=\s*\1;)");
        optimizedCode = regex_replace(optimizedCode, redundantAssignmentPattern, "");

        cout << "Optimized Code: " << optimizedCode << endl;
        return optimizedCode;
    }

    // Function to test parsing, constant folding, and dead code elimination
    void testOptimization()
    {
        cout << "Running Optimization Test..." << endl;

        // Test constant folding
        string expression1 = "2 + 3 * 4"; // 2 + 12 = 14
        int result1 = foldConstants(expression1);
        cout << "Expression: " << expression1 << " = " << result1 << endl;

        // Test dead code elimination
        string code1 = R"(
int main() {
    // Variable declaration
    int a = 5;
    int b = 10;

    // If-else statement
    if (a < b) {
        a = 20;
    } else {
        b = 30;
    }

    // For loop
    for (int i = 0; i < 5; ++i) {
        a += i;
    }

    // While loop
    int counter = 3;
    while (counter > 0) {
        b -= counter;
        counter--;
    }

    return 0;
}
)";

        string optimizedCode1 = eliminateDeadCode(code1);
        cout << "Original Code: \n"
             << code1 << endl;
        cout << "Optimized Code: \n"
             << optimizedCode1 << endl;
    }
    int foldConstants(const string &expression)
    {
        cout << "Folding constants in: " << expression << endl;

        stack<int> numbers;
        stack<char> operators;

        auto isOperator = [](char c)
        {
            return c == '+' || c == '-' || c == '*' || c == '/';
        };

        int n = expression.length();
        for (int i = 0; i < n; i++)
        {
            char c = expression[i];

            if (isdigit(c))
            {
                int num = 0;
                while (i < n && isdigit(expression[i]))
                {
                    num = num * 10 + (expression[i] - '0');
                    i++;
                }
                i--; // since the for loop will increment it
                numbers.push(num);
            }
            else if (isOperator(c))
            {
                while (!operators.empty() &&
                       ((operators.top() == '*' || operators.top() == '/') ||
                        (c == '+' || c == '-')))
                {
                    int b = numbers.top();
                    numbers.pop();
                    int a = numbers.top();
                    numbers.pop();
                    char op = operators.top();
                    operators.pop();

                    int result = 0;
                    switch (op)
                    {
                    case '+':
                        result = a + b;
                        break;
                    case '-':
                        result = a - b;
                        break;
                    case '*':
                        result = a * b;
                        break;
                    case '/':
                        if (b == 0)
                            throw runtime_error("Division by zero");
                        result = a / b;
                        break;
                    }
                    numbers.push(result);
                }
                operators.push(c);
            }
        }

        while (!operators.empty())
        {
            int b = numbers.top();
            numbers.pop();
            int a = numbers.top();
            numbers.pop();
            char op = operators.top();
            operators.pop();

            int result = 0;
            switch (op)
            {
            case '+':
                result = a + b;
                break;
            case '-':
                result = a - b;
                break;
            case '*':
                result = a * b;
                break;
            case '/':
                if (b == 0)
                    throw runtime_error("Division by zero");
                result = a / b;
                break;
            }
            numbers.push(result);
        }

        int foldedValue = numbers.top();
        cout << "Folded Result: " << foldedValue << endl;
        return foldedValue;
    }

    // Function to simulate function interpretation
    int interpretFunction(const string &functionName, int param1, int param2)
    {
        if (functionName == "add")
        {
            return param1 + param2;
        }
        else if (functionName == "subtract")
        {
            return param1 - param2;
        }
        else
        {
            throw runtime_error("Function not recognized");
        }
    }

    // Function to test parsing and constant folding
    void testConstantFolding()
    {
        cout << "Running Constant Folding Test..." << endl;

        // Expressions to fold
        string expression1 = "2 + 3 * 4";     // Expected: 2 + 12 = 14
        string expression2 = "10 - 4 / 2";    // Expected: 10 - 2 = 8
        string expression3 = "8 * 2 + 3 * 4"; // Expected: 16 + 12 = 28
        string expression4 = "100 / 25 * 2";  // Expected: 4 * 2 = 8

        // Test folding
        int result1 = foldConstants(expression1);
        int result2 = foldConstants(expression2);
        int result3 = foldConstants(expression3);
        int result4 = foldConstants(expression4);

        cout << "Results: " << endl;
        cout << expression1 << " = " << result1 << " (Expected: 14)" << endl;
        cout << expression2 << " = " << result2 << " (Expected: 8)" << endl;
        cout << expression3 << " = " << result3 << " (Expected: 28)" << endl;
        cout << expression4 << " = " << result4 << " (Expected: 8)" << endl;

        cout << "Constant Folding Test Completed!" << endl;
    }

    // Type-check variable assignments and store in symbolTable
    void typeCheckVariableDeclarations(const string &code)
    {
        cout << "Type checking variable declarations..." << endl;

        // Match variable declarations (int x = 10; float y = 5.5; string name = "John";)
        regex variableDeclarationPattern(R"((int|float|string|double|char)\s+(\w+)\s*=\s*([^;]+);)");
        auto wordsBegin = sregex_iterator(code.begin(), code.end(), variableDeclarationPattern);
        auto wordsEnd = sregex_iterator();

        for (sregex_iterator i = wordsBegin; i != wordsEnd; ++i)
        {
            smatch match = *i;
            string type = match[1].str();
            string varName = match[2].str();
            string value = match[3].str();

            // Check if the variable is already declared
            if (symbolTable.find(varName) != symbolTable.end())
            {
                cerr << "Error: Variable '" << varName << "' is already declared." << endl;
                continue;
            }

            // Add the variable and its type to the symbol table
            symbolTable[varName] = type;
            cout << "Declared variable: " << varName << " of type: " << type << " with initial value: " << value << endl;
        }
    }

    // Check for type consistency when variables are re-assigned
    void checkVariableReassignments(const string &code)
    {
        cout << "Checking variable reassignments..." << endl;

        // Match assignments (x = 20; name = "Ali"; y = 4.5;)
        regex assignmentPattern(R"((\w+)\s*=\s*([^;]+);)");
        auto wordsBegin = sregex_iterator(code.begin(), code.end(), assignmentPattern);
        auto wordsEnd = sregex_iterator();

        for (sregex_iterator i = wordsBegin; i != wordsEnd; ++i)
        {
            smatch match = *i;
            string varName = match[1].str();
            string value = match[2].str();

            if (symbolTable.find(varName) == symbolTable.end())
            {
                cerr << "Error: Variable '" << varName << "' is not declared before assignment." << endl;
                continue;
            }

            string expectedType = symbolTable[varName];
            string actualType = inferType(value);

            if (expectedType != actualType)
            {
                cerr << "Type Mismatch: Variable '" << varName << "' was declared as " << expectedType
                     << " but assigned a value of type " << actualType << "." << endl;
            }
            else
            {
                cout << "Reassigned variable: " << varName << " with value: " << value << endl;
            }
        }
    }

    // Infer the type of a value
    string inferType(const string &value)
    {
        if (regex_match(value, regex(R"(\d+)")))
        {
            return "int";
        }
        else if (regex_match(value, regex(R"(\d+\.\d+)")))
        {
            return "float";
        }
        else if (regex_match(value, regex(R"(["'][^"']*["'])")))
        {
            return "string";
        }
        else if (regex_match(value, regex(R"(['].\w*['])")))
        {
            return "char";
        }
        else
        {
            cerr << "Warning: Unable to infer the type of value '" << value << "'." << endl;
            return "unknown";
        }
    }

    // Eliminate unreachable code, dead code, and unused variables

    // Run all type checking and optimization functions
    void optimizeCode(const string &code)
    {
        typeCheckVariableDeclarations(code);
        checkVariableReassignments(code);
        eliminateDeadCode(code);
        testConstantFolding();
    }
};

void testCodeOptimizer()
{
    Optimizer optimizer;

    string code = R"(
        int x = 10;
        float y = 20.5;
        string name = "John";
        x = 20;
        y = 30.5;
        name = 'A'; // Type mismatch
        int z;
        z = 50;
        return x + y;
        int unusedVar = 100; // Unused variable
    )";

    optimizer.optimizeCode(code);
}

int main()
{
    string src1 = R"(
    int x;
    x = 10;
    int y = 20;
    int sum;
    sum = x + y * 3;
    if(5 > 3){
        x = 20;
    }
    )",
           src2 = R"(
    int x;
    x = 10;
    int y = 20;
    int sum;
    sum = x + y * 3;
    if(5 > 3){
        x = 20;
    }
    else{
        x = 10;
    }
    )",
           src3 = R"(
    int x = 10;
    int a;
    int w;
    w=455;
    if(x > 0){
        a = 0;
    }
    else if(x > 10){
        a = 1;
    }
    )",
           src4 = R"(
    int x = 1;
    while(x < 5){
        x = x + 1;
    }
    )";

    string src = src3;
    Lexer lexer(src);
    vector<Token> tokens = lexer.tokenize();

    SymbolTable symTable;
    IntermediateCodeGnerator icg;
    Parser parser(tokens, symTable, icg);

    parser.parseProgram();
    symTable.printSymbolTable();
    icg.printInstructions();

    vector<string> intermediateCode = icg.getInstructions();
    AssemblyCodeGenerator Generator;
    Generator.generateAssembly(intermediateCode);
    Generator.printAssemblyCode();
    testCodeOptimizer();

    return 0;
}
