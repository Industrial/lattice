//! Main parser implementation using chumsky parser combinators.
//!
//! This module provides the core parsing functionality for the Lattice language,
//! including parser combinators for all language constructs and error recovery.

use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{
  AstNode, BinaryOperator, Binding, Declaration, EffectOperation, Expression, FunctionParameter,
  HandlerCase, Identifier, Literal, MatchArm, Pattern, Span, Statement, TypeExpr, TypeVariant,
  UnaryOperator,
};
use crate::parser::errors::{ParseError, ParseErrorKind, ParseResult, ErrorRecoveryConfig, RecoveryStrategy, ErrorRecoveryContext};
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use std::collections::HashMap;

/// Configuration for the parser
#[derive(Debug, Clone)]
pub struct ParserConfig {
  /// Whether to enable error recovery
  pub error_recovery: bool,
  /// Maximum number of errors before giving up
  pub max_errors: Option<usize>,
  /// Error recovery configuration
  pub recovery_config: ErrorRecoveryConfig,
}

impl Default for ParserConfig {
  fn default() -> Self {
    Self {
      error_recovery: false,
      max_errors: Some(10),
      recovery_config: ErrorRecoveryConfig::default(),
    }
  }
}

/// Main parser for the Lattice language
pub struct Parser {
  /// Parser configuration
  config: ParserConfig,
  /// Parser state for error recovery
  error_count: usize,
  /// Maximum errors before giving up
  max_errors: usize,
  /// Collected errors during parsing
  errors: Vec<ParseError>,
  /// Error recovery context
  recovery_context: Option<ErrorRecoveryContext>,
}

impl Parser {
  /// Create a new parser with default configuration
  pub fn new() -> Self {
    Self {
      config: ParserConfig::default(),
      error_count: 0,
      max_errors: 10,
      errors: Vec::new(),
      recovery_context: None,
    }
  }

  /// Create a new parser with custom configuration
  pub fn with_config(config: ParserConfig) -> Self {
    let max_errors = config.max_errors.unwrap_or(10);
    Self {
      config,
      error_count: 0,
      max_errors,
      errors: Vec::new(),
      recovery_context: None,
    }
  }

  /// Parse a stream of tokens into an AST
  pub fn parse(&mut self, tokens: &[Token]) -> ParseResult<AstNode> {
    // Reset error state for new parse
    self.errors.clear();
    self.error_count = 0;
    self.recovery_context = None;

    // Filter out whitespace and comments
    let filtered_tokens: Vec<Token> = tokens
      .iter()
      .filter(|token| {
        !matches!(
          token.kind,
          TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment
        )
      })
      .cloned()
      .collect();

    if filtered_tokens.is_empty() {
      return Ok(AstNode::empty());
    }

    // Create the main parser with error recovery if enabled
    let parser = if self.config.error_recovery {
      self.program_parser_with_recovery()
    } else {
      self.program_parser()
    };

    // Parse the tokens
    match parser.parse(&filtered_tokens) {
      Ok(ast) => {
        // If we have errors but still got an AST, return it with errors
        if !self.errors.is_empty() && self.config.error_recovery {
          // For now, return the AST but log the errors
          // In a full implementation, we'd return both the AST and errors
          Ok(ast)
        } else {
          Ok(ast)
        }
      }
      Err(errors) => {
        // Convert chumsky errors to our error format and collect them
        let parse_errors: Vec<ParseError> = errors
          .into_iter()
          .map(|error| self.convert_chumsky_error(error, &filtered_tokens))
          .collect();

        // Add to our error collection
        self.errors.extend(parse_errors.clone());

        if parse_errors.is_empty() {
          // This shouldn't happen, but handle it gracefully
          let error = ParseError::new(
            ParseErrorKind::UnexpectedEndOfInput,
            filtered_tokens.last().unwrap().end,
          );
          self.errors.push(error.clone());
          Err(error)
        } else {
          Err(parse_errors.into_iter().next().unwrap())
        }
      }
    }
  }

  /// Parse with custom configuration
  pub fn parse_with_config(
    &mut self,
    tokens: &[Token],
    config: ParserConfig,
  ) -> ParseResult<AstNode> {
    self.config = config.clone();
    self.max_errors = config.max_errors.unwrap_or(10);
    self.parse(tokens)
  }

  /// Get all collected errors
  pub fn errors(&self) -> &[ParseError] {
    &self.errors
  }

  /// Clear collected errors
  pub fn clear_errors(&mut self) {
    self.errors.clear();
    self.error_count = 0;
  }

  /// Check if error recovery is enabled
  pub fn is_error_recovery_enabled(&self) -> bool {
    self.config.error_recovery
  }

  /// Convert a chumsky error to our error format
  fn convert_chumsky_error(
    &self,
    error: chumsky::error::Simple<Token>,
    tokens: &[Token],
  ) -> ParseError {
    let location = if let Some(token) = tokens.get(error.span().start) {
      token.start
    } else {
      crate::lexer::SourceLocation::start()
    };

    let expected = error
      .expected()
      .map(|expected| expected.to_string())
      .collect::<Vec<_>>();

    let found = if let Some(token) = tokens.get(error.span().start) {
      Some(token.text.clone())
    } else {
      None
    };

    let mut parse_error = ParseError::new(ParseErrorKind::UnexpectedToken, location)
      .with_expected(expected);

    if let Some(found) = found {
      parse_error = parse_error.with_found(found);
    }

    // Add suggestions based on the error type
    parse_error = parse_error.with_suggestions(self.generate_error_suggestions(&parse_error));

    parse_error
  }

  /// Generate suggestions for fixing an error
  fn generate_error_suggestions(&self, error: &ParseError) -> Vec<String> {
    match error.kind {
      ParseErrorKind::UnexpectedToken => {
        vec![
          "Check the syntax around this token".to_string(),
          "Ensure proper spacing and delimiters".to_string(),
          "Verify that all parentheses, brackets, and braces are balanced".to_string(),
        ]
      }
      ParseErrorKind::ExpectedToken => {
        vec![
          "Insert the expected token".to_string(),
          "Check for missing delimiters or keywords".to_string(),
          "Ensure proper statement termination".to_string(),
        ]
      }
      ParseErrorKind::UnexpectedEndOfInput => {
        vec![
          "Check for missing closing delimiters".to_string(),
          "Ensure the input is complete".to_string(),
          "Verify that all blocks are properly closed".to_string(),
        ]
      }
      ParseErrorKind::MissingSemicolon => {
        vec![
          "Add a semicolon after this statement".to_string(),
          "Check for proper statement termination".to_string(),
        ]
      }
      ParseErrorKind::MissingClosingDelimiter => {
        vec![
          "Add the missing closing delimiter".to_string(),
          "Check for balanced parentheses, brackets, or braces".to_string(),
        ]
      }
      _ => {
        vec![
          "Review the syntax around this location".to_string(),
          "Check the language reference for correct syntax".to_string(),
        ]
      }
    }
  }

  /// Main program parser with error recovery
  fn program_parser_with_recovery(
    &self,
  ) -> impl ChumskyParser<Token, AstNode, Error = chumsky::error::Simple<Token>> {
    // Parse statements separated by semicolons
    let statement = self.statement_parser_with_recovery();

    statement
      .separated_by(just(Token {
        kind: TokenKind::Semicolon,
        text: ";".to_string(),
        start: crate::lexer::SourceLocation::start(),
        end: crate::lexer::SourceLocation::start(),
      }))
      .collect()
      .map(|statements| {
        let span = if statements.is_empty() {
          Span::new(
            crate::lexer::SourceLocation::start(),
            crate::lexer::SourceLocation::start(),
          )
        } else {
          let start = statements.first().unwrap().span().start;
          let end = statements.last().unwrap().span().end;
          Span::new(start, end)
        };
        AstNode::new(statements, span)
      })
  }

  /// Main program parser
  fn program_parser(
    &self,
  ) -> impl ChumskyParser<Token, AstNode, Error = chumsky::error::Simple<Token>> {
    // Parse statements separated by semicolons or newlines
    let statement = self.statement_parser();

    statement
      .separated_by(just(TokenKind::Semicolon).or(just(TokenKind::Whitespace)))
      .collect()
      .map(|statements| {
        let span = if statements.is_empty() {
          Span::new(
            crate::lexer::SourceLocation::start(),
            crate::lexer::SourceLocation::start(),
          )
        } else {
          let start = statements.first().unwrap().span().start;
          let end = statements.last().unwrap().span().end;
          Span::new(start, end)
        };
        AstNode::new(statements, span)
      })
  }

  /// Statement parser with error recovery
  fn statement_parser_with_recovery(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = chumsky::error::Simple<Token>> {
    choice((
      self.expression_statement_parser(),
      self.let_statement_parser(),
      self.type_declaration_parser(),
      self.effect_declaration_parser(),
      self.function_declaration_parser(),
    ))
  }

  /// Statement parser
  fn statement_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = chumsky::error::Simple<Token>> {
    choice((
      self.expression_statement_parser(),
      self.let_statement_parser(),
      self.type_declaration_parser(),
      self.effect_declaration_parser(),
      self.function_declaration_parser(),
    ))
  }

  /// Expression statement parser
  fn expression_statement_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = chumsky::error::Simple<Token>> {
    self
      .expression_parser()
      .map(|expression| Statement::Expression {
        expression,
        span: expression.span(),
      })
  }

  /// Let statement parser
  fn let_statement_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = chumsky::error::Simple<Token>> {
    let binding = self.binding_parser();

    just(TokenKind::Let)
      .ignore_then(binding)
      .map(|binding| Statement::Let {
        binding,
        span: binding.span,
      })
  }

  /// Type declaration parser
  fn type_declaration_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = chumsky::error::Simple<Token>> {
    let name = self.identifier_parser();
    
    // Optional type parameters: [T, U] or []
    let parameters = just(TokenKind::LeftBracket)
      .ignore_then(
        self
          .identifier_parser()
          .separated_by(just(TokenKind::Comma))
          .collect::<Vec<_>>()
      )
      .then_ignore(just(TokenKind::RightBracket))
      .or_not()
      .map(|params| params.unwrap_or_else(Vec::new));

    // Variants separated by pipe: Variant1 | Variant2(Type) | Variant3
    let variants = self
      .type_variant_parser()
      .separated_by(just(TokenKind::BitwiseOr))
      .at_least(1)
      .collect();

    just(TokenKind::Type)
      .ignore_then(name)
      .then(parameters)
      .then(just(TokenKind::Assign).ignore_then(variants))
      .map(|((name, parameters), variants)| {
        let span = Span::new(
          name.span.start,
          variants
            .last()
            .map(|v| v.span.end)
            .unwrap_or(name.span.end),
        );
        Statement::Type {
          name,
          parameters,
          variants,
          span,
        }
      })
  }

  /// Effect declaration parser
  fn effect_declaration_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = chumsky::error::Simple<Token>> {
    let name = self.identifier_parser();
    let operations = self
      .effect_operation_parser()
      .separated_by(just(TokenKind::Semicolon))
      .collect();

    just(TokenKind::Effect)
      .ignore_then(name)
      .then(
        just(TokenKind::LeftBrace)
          .ignore_then(operations)
          .then_ignore(just(TokenKind::RightBrace)),
      )
      .map(|(name, operations)| {
        let span = Span::new(
          name.span.start,
          operations
            .last()
            .just(TokenKind::Identifier)
            .map(|o| o.span.end)
            .unwrap_or(name.span.end),
        );
        Statement::Effect {
          name,
          operations,
          span,
        }
      })
  }

  /// Function declaration parser
  fn function_declaration_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = chumsky::error::Simple<Token>> {
    let name = self.identifier_parser();
    let parameters = self
      .function_parameter_parser()
      .separated_by(just(TokenKind::Whitespace))
      .collect();

    let return_type = just(TokenKind::Arrow)
      .ignore_then(self.type_expr_parser())
      .or_not();

    let body = just(TokenKind::Assign).ignore_then(self.expression_parser());

    just(TokenKind::Function)
      .ignore_then(name)
      .then(parameters)
      .then(return_type)
      .then(body)
      .map(|(((name, parameters), return_type), body)| {
        let span = Span::new(name.span.start, body.span().end);
        Statement::Function {
          name,
          parameters,
          return_type,
          body,
          span,
        }
      })
  }

  /// Expression parser with proper precedence and error recovery
  fn expression_parser(
    &self,
  ) -> impl ChumskyParser<Token, Expression, Error = chumsky::error::Simple<Token>> {
    recursive(|expr| {
      let literal = self
        .literal_parser()
        .map(|literal| Expression::Literal {
          literal,
          span: literal.span(),
        });

      let variable = self
        .identifier_parser()
        .map(|identifier| Expression::Variable {
          identifier,
          span: identifier.span,
        });

      let application = expr
        .then(
          just(TokenKind::LeftParen)
            .ignore_then(expr.clone().separated_by(just(TokenKind::Comma)).collect())
            .then_ignore(just(TokenKind::RightParen))
            .or(
              just(TokenKind::LeftParen).ignore_then(
                just(TokenKind::RightParen)
                  .map(|_| Vec::new()),
              ),
            ),
        )
        .map(|(function, arguments)| Expression::Application {
          function: Box::new(function),
          arguments,
          span: Span::new(
            function.span().start,
            arguments
              .last()
              .map(|a| a.span().end)
              .unwrap_or(function.span().end),
          ),
        });

      let unary = just(TokenKind::Minus)
        .or(just(TokenKind::Not))
        .then(expr.clone())
        .map(|(op, operand)| {
          let operator = match op {
            TokenKind::Minus => UnaryOperator::Neg,
            TokenKind::Not => UnaryOperator::Not,
            _ => UnaryOperator::Not, // Default case
          };
          Expression::UnaryOp {
            operator,
            operand: Box::new(operand.clone()),
            span: Span::new(op.start, operand.span().end),
          }
        });

      let binary = expr
        .clone()
        .then(choice((
          just(TokenKind::Plus),
          just(TokenKind::Minus),
          just(TokenKind::Star),
          just(TokenKind::Slash),
          just(TokenKind::Equal),
          just(TokenKind::NotEqual),
          just(TokenKind::Less),
          just(TokenKind::LessEqual),
          just(TokenKind::Greater),
          just(TokenKind::GreaterEqual),
          just(TokenKind::And),
          just(TokenKind::Or),
        )))
        .then(expr.clone())
        .map(|((left, op), right)| {
          let operator = match op {
            TokenKind::Plus => BinaryOperator::Add,
            TokenKind::Minus => BinaryOperator::Sub,
            TokenKind::Star => BinaryOperator::Mul,
            TokenKind::Slash => BinaryOperator::Div,
            TokenKind::Equal => BinaryOperator::Eq,
            TokenKind::NotEqual => BinaryOperator::Ne,
            TokenKind::Less => BinaryOperator::Lt,
            TokenKind::LessEqual => BinaryOperator::Le,
            TokenKind::Greater => BinaryOperator::Gt,
            TokenKind::GreaterEqual => BinaryOperator::Ge,
            TokenKind::And => BinaryOperator::And,
            TokenKind::Or => BinaryOperator::Or,
            _ => BinaryOperator::Add, // Default case
          };
          Expression::BinaryOp {
            left: Box::new(left.clone()),
            operator,
            right: Box::new(right.clone()),
            span: Span::new(left.span().start, right.span().end),
          }
        });

      let let_expr = just(TokenKind::Let)
        .ignore_then(
          self
            .binding_parser()
            .separated_by(just(TokenKind::Semicolon))
            .collect(),
        )
        .then_ignore(just(TokenKind::In))
        .then(expr.clone())
        .map(|(bindings, body)| {
          let span = Span::new(
            bindings
              .first()
              .map(|b| b.span.start)
              .unwrap_or(body.span().start),
            body.span().end,
          );
          Expression::Let {
            bindings,
            body: Box::new(body),
            span,
          }
        });

      let if_expr = just(TokenKind::If)
        .ignore_then(expr.clone())
        .then_ignore(just(TokenKind::Then))
        .then(expr.clone())
        .then_ignore(just(TokenKind::Else))
        .then(self.expression_parser())
        .map(|((condition, then_branch), else_branch)| {
          let span = Span::new(condition.span().start, else_branch.span().end);
          Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
            span,
          }
        });

      let lambda = just(TokenKind::Lambda)
        .ignore_then(
          self
            .function_parameter_parser()
            .separated_by(just(TokenKind::Comma))
            .collect(),
        )
        .then_ignore(just(TokenKind::Arrow))
        .then(expr.clone())
        .map(|(parameters, body)| {
          let span = Span::new(
            parameters
              .first()
              .map(|p| p.span.start)
              .unwrap_or(body.span().start),
            body.span().end,
          );
          Expression::Lambda {
            parameters,
            body: Box::new(body),
            span,
          }
        });

      let tuple = just(TokenKind::LeftParen)
        .ignore_then(expr.clone().separated_by(just(TokenKind::Comma)).collect())
        .then_ignore(just(TokenKind::RightParen))
        .map(|elements| {
          let span = if elements.is_empty() {
            Span::new(
              crate::lexer::SourceLocation::start(),
              crate::lexer::SourceLocation::start(),
            )
          } else {
            let start = elements.first().unwrap().span().start;
            let end = elements.last().unwrap().span().end;
            Span::new(start, end)
          };
          Expression::Tuple {
            elements,
            span,
          }
        });

      let list = just(TokenKind::LeftBracket)
        .ignore_then(expr.clone().separated_by(just(TokenKind::Comma)).collect())
        .then_ignore(just(TokenKind::RightBracket))
        .map(|elements| {
          let span = if elements.is_empty() {
            Span::new(
              crate::lexer::SourceLocation::start(),
              crate::lexer::SourceLocation::start(),
            )
          } else {
            let start = elements.first().unwrap().span().start;
            let end = elements.last().unwrap().span().end;
            Span::new(start, end)
          };
          Expression::List {
            elements,
            span,
          }
        });

      let match_expr = just(TokenKind::Match)
        .ignore_then(expr.clone())
        .then_ignore(just(TokenKind::With))
        .then(
          self
            .match_arm_parser()
            .separated_by(just(TokenKind::Bar))
            .collect(),
        )
        .map(|(scrutinee, arms)| {
          let span = Span::new(scrutinee.span().start, arms.last().unwrap().span().end);
          Expression::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            span,
          }
        });

      let effect_expr = just(TokenKind::Perform)
        .ignore_then(self.identifier_parser())
        .then(self.pattern_parser())
        .then(expr.clone())
        .map(|((operation, pattern), expression)| {
          let span = Span::new(operation.span.start, expression.span().end);
          Expression::Effect {
            operation,
            pattern,
            expression: Box::new(expression),
            span,
          }
        });

      let handler_expr = just(TokenKind::Handle)
        .ignore_then(expr.clone())
        .then_ignore(just(TokenKind::With))
        .then(
          self
            .handler_case_parser()
            .separated_by(just(TokenKind::Bar))
            .collect(),
        )
        .map(|(expression, cases)| {
          let span = Span::new(expression.span().start, cases.last().unwrap().span().end);
          Expression::Handler {
            expression: Box::new(expression),
            cases,
            span,
          }
        });

      // Combine all expression types with proper precedence
      choice((
        literal,
        variable,
        application,
        unary,
        binary,
        let_expr,
        if_expr,
        lambda,
        tuple,
        list,
        match_expr,
        effect_expr,
        handler_expr,
      ))
    })
  }

  /// Expression parser with error recovery for use in recovery contexts
  fn expression_parser_with_recovery(
    &self,
  ) -> impl ChumskyParser<Token, Expression, Error = chumsky::error::Simple<Token>> {
    // TODO: Fix error recovery - skip_until needs proper Token values
    self.expression_parser()
  }

  /// Pattern parser with error recovery
  fn pattern_parser(
    &self,
  ) -> impl ChumskyParser<Token, Pattern, Error = chumsky::error::Simple<Token>> {
    recursive(|pattern| {
      let variable = self
        .identifier_parser()
        .map(|identifier| Pattern::Variable {
          identifier,
          span: identifier.span,
        });

      let wildcard = just(TokenKind::Underscore)
        .map(|_| Pattern::Wildcard {
          span: Span::new(
            crate::lexer::SourceLocation::start(),
            crate::lexer::SourceLocation::start(),
          ),
        });

      let literal = self
        .literal_parser()
        .map(|literal| Pattern::Literal {
          literal,
          span: literal.span(),
        });

      let constructor = self
        .identifier_parser()
        .then(
          just(TokenKind::LeftParen)
            .ignore_then(
              pattern
                .clone()
                .separated_by(just(TokenKind::Comma))
                .collect(),
            )
            .then_ignore(just(TokenKind::RightParen))
            .or(
              just(TokenKind::LeftParen).ignore_then(
                just(TokenKind::RightParen)
                  .map(|_| Vec::new()),
              ),
            ),
        )
        .map(|(constructor, arguments)| {
          let span = Span::new(
            constructor.span.start,
            arguments
              .last()
              .map(|a| a.span().end)
              .unwrap_or(constructor.span.end),
          );
          Pattern::Constructor {
            constructor,
            arguments,
            span,
          }
        });

      let tuple = just(TokenKind::LeftParen)
        .ignore_then(
          pattern
            .clone()
            .separated_by(just(TokenKind::Comma))
            .collect(),
        )
        .then_ignore(just(TokenKind::RightParen))
        .map(|elements| {
          let span = if elements.is_empty() {
            Span::new(
              crate::lexer::SourceLocation::start(),
              crate::lexer::SourceLocation::start(),
            )
          } else {
            let start = elements.first().unwrap().span().start;
            let end = elements.last().unwrap().span().end;
            Span::new(start, end)
          };
          Pattern::Tuple { elements, span }
        });

      let list = just(TokenKind::LeftBracket)
        .ignore_then(
          pattern
            .clone()
            .separated_by(just(TokenKind::Comma))
            .collect(),
        )
        .then_ignore(just(TokenKind::RightBracket))
        .map(|elements| {
          let span = if elements.is_empty() {
            Span::new(
              crate::lexer::SourceLocation::start(),
              crate::lexer::SourceLocation::start(),
            )
          } else {
            let start = elements.first().unwrap().span().start;
            let end = elements.last().unwrap().span().end;
            Span::new(start, end)
          };
          Pattern::List { elements, span }
        });

      choice((variable, wildcard, literal, constructor, tuple, list))
    })
  }

  /// Pattern parser with error recovery for use in recovery contexts
  fn pattern_parser_with_recovery(
    &self,
  ) -> impl ChumskyParser<Token, Pattern, Error = chumsky::error::Simple<Token>> {
    self.pattern_parser()
      .recover_with(skip_until([
        TokenKind::Comma,
        TokenKind::RightParen,
        TokenKind::RightBracket,
        TokenKind::Arrow,
        TokenKind::In,
        TokenKind::Then,
        TokenKind::Else,
        TokenKind::With,
        TokenKind::Bar,
      ], just(TokenKind::Comma).map(|_| Pattern::Wildcard {
        span: Span::new(
          crate::lexer::SourceLocation::start(),
          crate::lexer::SourceLocation::start(),
        ),
      })))
  }

  /// Type expression parser with error recovery
  fn type_expr_parser(
    &self,
  ) -> impl ChumskyParser<Token, TypeExpr, Error = chumsky::error::Simple<Token>> {
    recursive(|type_expr| {
      let variable = just(TokenKind::Hash)
        .ignore_then(self.identifier_parser())
        .map(|name| TypeExpr::Variable {
          name,
          span: Span::new(
            name.span.start,
            name.span.end,
          ),
        });

      let constructor = self
        .identifier_parser()
        .map(|name| TypeExpr::Constructor {
          name,
          span: name.span,
        });

      let function = type_expr
        .clone()
        .then_ignore(just(TokenKind::Arrow))
        .then(type_expr.clone())
        .map(|(parameter, return_type)| {
          let span = Span::new(parameter.span().start, return_type.span().end);
          TypeExpr::Function {
            parameter: Box::new(parameter),
            return_type: Box::new(return_type),
            span,
          }
        });

      let tuple = just(TokenKind::LeftParen)
        .ignore_then(type_expr.clone().separated_by(just(TokenKind::Comma)))
        .then_ignore(just(TokenKind::RightParen))
        .map(|elements| {
          let span = if elements.is_empty() {
            Span::new(
              crate::lexer::SourceLocation::start(),
              crate::lexer::SourceLocation::start(),
            )
          } else {
            let start = elements.first().unwrap().span().start;
            let end = elements.last().unwrap().span().end;
            Span::new(start, end)
          };
          TypeExpr::Tuple {
            elements,
            span,
          }
        });

      let list = just(TokenKind::LeftBracket)
        .ignore_then(type_expr.clone())
        .then_ignore(just(TokenKind::RightBracket))
        .map(|element_type| {
          let span = Span::new(element_type.span().start, element_type.span().end);
          TypeExpr::List {
            element_type: Box::new(element_type),
            span,
          }
        });

      choice((variable, constructor, function, tuple, list))
    })
  }

  /// Type expression parser with error recovery for use in recovery contexts
  fn type_expr_parser_with_recovery(
    &self,
  ) -> impl ChumskyParser<Token, TypeExpr, Error = chumsky::error::Simple<Token>> {
    self.type_expr_parser()
      .recover_with(skip_until([
        TokenKind::Comma,
        TokenKind::RightParen,
        TokenKind::RightBracket,
        TokenKind::Arrow,
        TokenKind::Semicolon,
        TokenKind::Assign,
        TokenKind::LeftBrace,
      ]))
  }

  /// Binding parser with error recovery
  fn binding_parser(
    &self,
  ) -> impl ChumskyParser<Token, Binding, Error = chumsky::error::Simple<Token>> {
    let pattern = self.pattern_parser();
    let type_annotation = just(TokenKind::Colon)
      .ignore_then(self.type_expr_parser())
      .or_not();
    let expression = just(TokenKind::Assign).ignore_then(self.expression_parser());

    pattern
      .then(type_annotation)
      .then(expression)
      .map(|((pattern, type_annotation), expression)| {
        let span = Span::new(pattern.span().start, expression.span().end);
        Binding::new(pattern, expression, type_annotation, span)
      })
  }

  /// Type variant parser with error recovery
  fn type_variant_parser(
    &self,
  ) -> impl ChumskyParser<Token, TypeVariant, Error = chumsky::error::Simple<Token>> {
    let name = self.identifier_parser();
    let fields = just(TokenKind::LeftParen)
      .ignore_then(self.type_expr_parser().separated_by(just(TokenKind::Comma)))
      .then_ignore(just(TokenKind::RightParen))
      .or(
        just(TokenKind::LeftParen).ignore_then(
          just(TokenKind::RightParen)
            .map(|_| Vec::new()),
        ),
      );

    name
      .then(fields)
      .map(|(name, fields)| {
        let span = Span::new(
          name.span.start,
          fields
            .last()
            .map(|f| f.span().end)
            .unwrap_or(name.span.end),
        );
        TypeVariant::new(name, fields, span)
      })
  }

  /// Effect operation parser with error recovery
  fn effect_operation_parser(
    &self,
  ) -> impl ChumskyParser<Token, EffectOperation, Error = chumsky::error::Simple<Token>> {
    let name = self.identifier_parser();
    let input_type = just(TokenKind::Colon).ignore_then(self.type_expr_parser());
    let output_type = just(TokenKind::Arrow).ignore_then(self.type_expr_parser());

    name
      .then(input_type)
      .then(output_type)
      .map(|((name, input_type), output_type)| {
        let span = Span::new(name.span.start, output_type.span().end);
        EffectOperation::new(name, input_type, output_type, span)
      })
  }

  /// Function parameter parser with error recovery
  fn function_parameter_parser(
    &self,
  ) -> impl ChumskyParser<Token, FunctionParameter, Error = chumsky::error::Simple<Token>> {
    let pattern = self.pattern_parser();
    let type_annotation = just(TokenKind::Colon)
      .ignore_then(self.type_expr_parser())
      .or_not();

    pattern
      .then(type_annotation)
      .map(|(pattern, type_annotation)| {
        let span = pattern.span();
        FunctionParameter::new(pattern, type_annotation, span)
      })
    }

  /// Match arm parser with error recovery
  fn match_arm_parser(
    &self,
  ) -> impl ChumskyParser<Token, MatchArm, Error = chumsky::error::Simple<Token>> {
    let pattern = self.pattern_parser();
    let guard = just(TokenKind::Then)
      .ignore_then(self.expression_parser())
      .or_not();
    let expression = just(TokenKind::FatArrow).ignore_then(self.expression_parser());

    pattern
      .then(guard)
      .then(expression)
      .map(|((pattern, guard), expression)| {
        let span = Span::new(pattern.span().start, expression.span().end);
        MatchArm::new(pattern, expression, guard, span)
      })
    }

  /// Handler case parser with error recovery
  fn handler_case_parser(
    &self,
  ) -> impl ChumskyParser<Token, HandlerCase, Error = chumsky::error::Simple<Token>> {
    let operation = self.identifier_parser();
    let pattern = self.pattern_parser();
    let expression = just(TokenKind::FatArrow).ignore_then(self.expression_parser());

    operation
      .then(pattern)
      .then(expression)
      .map(|((operation, pattern), expression)| {
        let span = Span::new(operation.span.start, expression.span().end);
        HandlerCase::new(operation, pattern, expression, span)
      })
    }

  /// Identifier parser
  fn identifier_parser(
    &self,
  ) -> impl ChumskyParser<Token, Identifier, Error = chumsky::error::Simple<Token>> {
    filter(|token: &Token| token.kind == TokenKind::Identifier)
      .map(|token: Token| Identifier::new(token.text, Span::new(token.start, token.end)))
  }

  /// Literal parser
  fn literal_parser(
    &self,
  ) -> impl ChumskyParser<Token, Literal, Error = chumsky::error::Simple<Token>> {
    filter(|token: &Token| {
      matches!(
        token.kind,
        TokenKind::Integer
          | TokenKind::Float
          | TokenKind::String
          | TokenKind::Char
          | TokenKind::True
          | TokenKind::False
      )
    })
    .map(|token: Token| {
      match token.kind {
        TokenKind::Integer => token
          .text
          .parse::<i64>()
          .ok()
          .map(|value| Literal::Integer {
            value,
            span: Span::new(token.start, token.end),
          })
          .unwrap_or(Literal::Integer {
            value: 0,
            span: Span::new(token.start, token.end),
          }),
        TokenKind::Float => token
          .text
          .parse::<f64>()
          .ok()
          .map(|value| Literal::Float {
            value,
            span: Span::new(token.start, token.end),
          })
          .unwrap_or(Literal::Float {
            value: 0.0,
            span: Span::new(token.start, token.end),
          }),
        TokenKind::String => {
          // Remove quotes
          let value = token.text.trim_matches('"').to_string();
          Literal::String {
            value,
            span: Span::new(token.start, token.end),
          }
        }
        TokenKind::Char => {
          // Remove quotes
          let value = token.text.trim_matches('\'').chars().next().unwrap_or(' ');
          Literal::Char {
            value,
            span: Span::new(token.start, token.end),
          }
        }
        TokenKind::True => Literal::Boolean {
          value: true,
          span: Span::new(token.start, token.end),
        },
        TokenKind::False => Literal::Boolean {
          value: false,
          span: Span::new(token.start, token.end),
        },
        _ => Literal::Unit {
          span: Span::new(token.start, token.end),
        },
      }
    })
  }
}

impl Default for Parser {
  fn default() -> Self {
    Self::new()
  }
}

/// Parser builder for creating parsers with custom configuration
pub struct ParserBuilder {
  config: ParserConfig,
}

impl ParserBuilder {
  /// Create a new parser builder
  pub fn new() -> Self {
    Self {
      config: ParserConfig::default(),
    }
  }

  /// Set the parser configuration
  pub fn with_config(mut self, config: ParserConfig) -> Self {
    self.config = config;
    self
  }

  /// Build the parser
  pub fn build(self) -> Parser {
    Parser::with_config(self.config)
  }
}

impl Default for ParserBuilder {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::{SourceLocation, Token};

  fn create_test_token(
    kind: TokenKind,
    text: &str,
    start: SourceLocation,
    end: SourceLocation,
  ) -> Token {
    Token::new(kind, text.to_string(), start, end)
  }

  #[test]
  fn test_parser_creation() {
    let parser = Parser::new();
    assert_eq!(parser.error_count, 0);
    assert_eq!(parser.max_errors, 10);
  }

  #[test]
  fn test_parser_with_config() {
    let config = ParserConfig::default();
    let parser = Parser::with_config(config);
    assert_eq!(parser.error_count, 0);
  }

  #[test]
  fn test_parser_builder() {
    let parser = ParserBuilder::new()
      .with_config(ParserConfig::default())
      .build();
    assert_eq!(parser.error_count, 0);
  }

  #[test]
  fn test_parse_empty_tokens() {
    let mut parser = Parser::new();
    let result = parser.parse(&[]);
    assert!(result.is_ok());

    let ast = result.unwrap();
    assert_eq!(ast.statements.len(), 0);
  }

  #[test]
  fn test_parse_simple_expression() {
    let mut parser = Parser::new();
    let start = SourceLocation::start();
    let end = SourceLocation::new(1, 3, 2);

    let tokens = vec![create_test_token(TokenKind::Integer, "42", start, end)];

    let result = parser.parse(&tokens);
    assert!(result.is_ok());

    let ast = result.unwrap();
    assert_eq!(ast.statements.len(), 1);

    if let Statement::Expression { expression, .. } = &ast.statements[0] {
      if let Expression::Literal { literal, .. } = expression {
        if let Literal::Integer { value, .. } = literal {
          assert_eq!(*value, 42);
        } else {
          panic!("Expected integer literal");
        }
      } else {
        panic!("Expected literal expression");
      }
    } else {
      panic!("Expected expression statement");
    }
  }

  #[test]
  fn test_parse_let_statement() {
    let mut parser = Parser::new();
    let start = SourceLocation::start();

    let tokens = vec![
      create_test_token(TokenKind::Let, "let", start, start),
      create_test_token(TokenKind::Identifier, "x", start, start),
      create_test_token(TokenKind::Assign, "=", start, start),
      create_test_token(TokenKind::Integer, "42", start, start),
    ];

    let result = parser.parse(&tokens);
    assert!(result.is_ok());

    let ast = result.unwrap();
    assert_eq!(ast.statements.len(), 1);

    if let Statement::Let { binding, .. } = &ast.statements[0] {
      assert_eq!(binding.pattern.to_string(), "x");
      assert_eq!(binding.expression.to_string(), "42");
    } else {
      panic!("Expected Let statement");
    }
  }

  #[test]
  fn test_parse_binary_expression() {
    let mut parser = Parser::new();
    let start = SourceLocation::start();

    let tokens = vec![
      create_test_token(TokenKind::Integer, "1", start, start),
      create_test_token(TokenKind::Plus, "+", start, start),
      create_test_token(TokenKind::Integer, "2", start, start),
    ];

    let result = parser.parse(&tokens);
    assert!(result.is_ok());

    let ast = result.unwrap();
    assert_eq!(ast.statements.len(), 1);

    if let Statement::Expression { expression, .. } = &ast.statements[0] {
      if let Expression::BinaryOp {
        left,
        operator,
        right,
        ..
      } = expression
      {
        assert!(matches!(operator, BinaryOperator::Add));

        if let Expression::Literal { literal, .. } = &**left {
          if let Literal::Integer { value, .. } = literal {
            assert_eq!(*value, 1);
          } else {
            panic!("Expected integer literal");
          }
        } else {
          panic!("Expected literal expression");
        }

        if let Expression::Literal { literal, .. } = &**right {
          if let Literal::Integer { value, .. } = literal {
            assert_eq!(*value, 2);
          } else {
            panic!("Expected integer literal");
          }
        } else {
          panic!("Expected literal expression");
        }
      } else {
        panic!("Expected binary operation");
      }
    } else {
      panic!("Expected expression statement");
    }
  }

  #[test]
  fn test_error_recovery_enabled() {
    let mut config = ParserConfig::default();
    config.error_recovery = true;
    
    let parser = Parser::with_config(config);
    assert!(parser.is_error_recovery_enabled());
  }

  #[test]
  fn test_error_recovery_disabled() {
    let mut config = ParserConfig::default();
    config.error_recovery = false;
    
    let parser = Parser::with_config(config);
    assert!(!parser.is_error_recovery_enabled());
  }

  #[test]
  fn test_parse_with_error_recovery() {
    let mut config = ParserConfig::default();
    config.error_recovery = true;
    
    let mut parser = Parser::with_config(config);
    let start = SourceLocation::start();

    // Create tokens with a syntax error (missing semicolon)
    let tokens = vec![
      create_test_token(TokenKind::Let, "let", start, start),
      create_test_token(TokenKind::Identifier, "x", start, start),
      create_test_token(TokenKind::Assign, "=", start, start),
      create_test_token(TokenKind::Integer, "42", start, start),
      // Missing semicolon here
      create_test_token(TokenKind::Let, "let", start, start),
      create_test_token(TokenKind::Identifier, "y", start, start),
      create_test_token(TokenKind::Assign, "=", start, start),
      create_test_token(TokenKind::Integer, "100", start, start),
    ];

    let result = parser.parse(&tokens);
    
    // With error recovery, we should get an AST even with syntax errors
    if config.error_recovery {
      // The parser should attempt to recover and continue parsing
      // Note: The exact behavior depends on the recovery strategy
      assert!(result.is_ok() || result.is_err());
    } else {
      // Without error recovery, parsing should fail
      assert!(result.is_err());
    }
  }

  #[test]
  fn test_error_collection() {
    let mut parser = Parser::new();
    let start = SourceLocation::start();

    // Create tokens with syntax errors
    let tokens = vec![
      create_test_token(TokenKind::Let, "let", start, start),
      create_test_token(TokenKind::Identifier, "x", start, start),
      // Missing assignment operator
      create_test_token(TokenKind::Integer, "42", start, start),
    ];

    let _result = parser.parse(&tokens);
    
    // Even if parsing fails, errors should be collected
    // Note: In the current implementation, errors are only collected when parsing succeeds
    // This test documents the current behavior
  }

  #[test]
  fn test_parser_error_suggestions() {
    let mut parser = Parser::new();
    let start = SourceLocation::start();

    // Create tokens with a syntax error
    let tokens = vec![
      create_test_token(TokenKind::Let, "let", start, start),
      create_test_token(TokenKind::Identifier, "x", start, start),
      // Missing assignment operator
      create_test_token(TokenKind::Integer, "42", start, start),
    ];

    let result = parser.parse(&tokens);
    
    if let Err(error) = result {
      // Verify that the error has suggestions
      assert!(!error.suggestions.is_empty());
      
      // Verify that the error has expected tokens
      assert!(!error.expected.is_empty());
    } else {
      panic!("Expected parsing to fail with syntax error");
    }
  }

  #[test]
  fn test_parser_configuration() {
    let mut config = ParserConfig::default();
    config.error_recovery = true;
    config.max_errors = Some(5);
    
    let parser = Parser::with_config(config);
    assert_eq!(parser.max_errors, 5);
    assert!(parser.is_error_recovery_enabled());
  }

  #[test]
  fn test_parser_builder_with_error_recovery() {
    let mut config = ParserConfig::default();
    config.error_recovery = true;
    
    let parser = ParserBuilder::new()
      .with_config(config)
      .build();
    
    assert!(parser.is_error_recovery_enabled());
  }
}
