use crate::lexer::{Token, TokenKind};
use crate::parser::ast::*;
use crate::parser::errors::{ParseError, ParseErrorKind, ParseResult};
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;

/// A parser for the Lattice language
#[derive(Debug, Clone)]
pub struct Parser;

impl Parser {
  /// Create a new parser instance
  pub fn new() -> Self {
    Self
  }

  /// Helper function to match a token by its kind
  fn token_kind(&self, kind: TokenKind) -> impl ChumskyParser<Token, Token, Error = Simple<Token>> {
    filter_map(move |span: std::ops::Range<usize>, token: Token| {
      if token.kind == kind {
        Ok(token)
      } else {
        Err(Simple::custom(span, format!("Expected {:?}", kind)))
      }
    })
  }

  /// Parse a stream of tokens into an AST
  pub fn parse(&self, tokens: &[Token]) -> ParseResult<AstNode> {
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
      return Ok(AstNode {
        statements: Vec::new(),
        span: Span::new(
          crate::lexer::SourceLocation::start(),
          crate::lexer::SourceLocation::start(),
        ),
      });
    }

    // Parse the program
    let program_parser = self.program_parser();

    match program_parser.parse(&*filtered_tokens) {
      Ok(ast) => Ok(ast),
      Err(errors) => {
        let parse_errors: Vec<ParseError> = errors
          .into_iter()
          .map(|error| ParseError {
            kind: ParseErrorKind::UnexpectedToken,
            location: crate::lexer::SourceLocation::start(),
            context: Some(format!("Parse error: {:?}", error)),
            expected: vec![],
            found: None,
            suggestions: vec![],
          })
          .collect();

        Err(
          parse_errors
            .into_iter()
            .next()
            .unwrap_or_else(|| ParseError {
              kind: ParseErrorKind::UnexpectedToken,
              location: crate::lexer::SourceLocation::start(),
              context: Some("Parse error".to_string()),
              expected: vec![],
              found: None,
              suggestions: vec![],
            }),
        )
      }
    }
  }

  /// Parse a complete program
  fn program_parser(&self) -> impl ChumskyParser<Token, AstNode, Error = Simple<Token>> {
    // Try to parse as a single expression first, then fall back to multiple statements
    self
      .expression_parser()
      .then_ignore(chumsky::primitive::end())
      .map(|expr| {
        let span = expr.span();
        AstNode {
          statements: vec![Statement::Expression {
            expression: expr,
            span,
            type_annotation: None,
          }],
          span,
        }
      })
      .or(
        // If single expression fails, try multiple statements separated by semicolons
        self
          .statement_parser()
          .separated_by(self.token_kind(TokenKind::Semicolon))
          .collect::<Vec<Statement>>()
          .then_ignore(chumsky::primitive::end())
          .map(|statements: Vec<Statement>| {
            let span = if statements.is_empty() {
              Span::new(
                crate::lexer::SourceLocation::start(),
                crate::lexer::SourceLocation::start(),
              )
            } else {
              Span::new(
                statements.first().unwrap().span().start,
                statements.last().unwrap().span().end,
              )
            };

            AstNode { statements, span }
          }),
      )
  }

  /// Parse a statement
  fn statement_parser(&self) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> {
    self
      .let_statement_parser()
      .or(self.type_declaration_parser())
      .or(self.effect_declaration_parser())
      .or(self.expression_statement_parser())
  }

  /// Parse a let statement
  fn let_statement_parser(&self) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> {
    self
      .token_kind(TokenKind::Let)
      .ignore_then(self.binding_parser())
      .map(|binding| {
        let span = binding.span;
        Statement::Let {
          binding,
          span,
          type_annotation: None,
        }
      })
  }

  /// Parse a type declaration
  fn type_declaration_parser(&self) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> {
    let name = self.identifier_parser();

    // Optional type parameters: [T, U] or []
    let parameters = self
      .token_kind(TokenKind::LeftBracket)
      .ignore_then(
        self
          .identifier_parser()
          .separated_by(self.token_kind(TokenKind::Comma))
          .collect::<Vec<_>>(),
      )
      .then_ignore(self.token_kind(TokenKind::RightBracket))
      .or_not()
      .map(|params| params.unwrap_or_else(Vec::new));

    // Type variants: | Variant1 | Variant2(Type) |
    let variants = self.token_kind(TokenKind::BitwiseOr).ignore_then(
      self
        .type_variant_parser()
        .separated_by(self.token_kind(TokenKind::BitwiseOr))
        .collect::<Vec<_>>(),
    );

    name
      .then(parameters)
      .then(variants)
      .map(|((name, parameters), variants)| {
        let span = Span::new(
          name.span.start,
          variants.last().map(|v| v.span.end).unwrap_or(name.span.end),
        );

        Statement::Type {
          name,
          parameters,
          variants,
          span,
          type_annotation: None,
        }
      })
  }

  /// Parse an effect declaration
  fn effect_declaration_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> {
    let name = self.identifier_parser();
    let operations = self
      .token_kind(TokenKind::LeftBrace)
      .ignore_then(
        self
          .effect_operation_parser()
          .separated_by(self.token_kind(TokenKind::Semicolon))
          .collect::<Vec<_>>(),
      )
      .then_ignore(self.token_kind(TokenKind::RightBrace));

    name.then(operations).map(|(name, operations)| {
      let span = Span::new(
        name.span.start,
        operations
          .last()
          .map(|op| op.span.end)
          .unwrap_or(name.span.end),
      );

      Statement::Effect {
        name,
        operations,
        span,
        type_annotation: None,
      }
    })
  }

  /// Parse an expression statement
  fn expression_statement_parser(
    &self,
  ) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> {
    self.expression_parser().map(|expr| Statement::Expression {
      expression: expr.clone(),
      span: expr.span(),
      type_annotation: None,
    })
  }

  /// Parse a binding
  fn binding_parser(&self) -> impl ChumskyParser<Token, Binding, Error = Simple<Token>> {
    let pattern = self.pattern_parser();
    let type_annotation = self
      .token_kind(TokenKind::Colon)
      .ignore_then(self.type_expr_parser())
      .or_not();
    let expression = self
      .token_kind(TokenKind::Assign)
      .ignore_then(self.expression_parser());

    pattern.then(type_annotation).then(expression).map(
      |((pattern, type_annotation), expression)| {
        let span = Span::new(pattern.span().start, expression.span().end);

        Binding {
          pattern,
          type_annotation,
          expression,
          span,
        }
      },
    )
  }

  /// Parse an identifier
  fn identifier_parser(&self) -> impl ChumskyParser<Token, Identifier, Error = Simple<Token>> {
    self
      .token_kind(TokenKind::Identifier)
      .map(|token| Identifier {
        name: token.text.clone(),
        span: Span::new(token.start, token.end),
      })
  }

  /// Parse a type variant
  fn type_variant_parser(&self) -> impl ChumskyParser<Token, TypeVariant, Error = Simple<Token>> {
    let name = self.identifier_parser();
    let fields = self
      .token_kind(TokenKind::LeftParen)
      .ignore_then(
        self
          .type_expr_parser()
          .separated_by(self.token_kind(TokenKind::Comma)),
      )
      .then_ignore(self.token_kind(TokenKind::RightParen))
      .or_not()
      .map(|fields| fields.unwrap_or_else(Vec::new));

    name.then(fields).map(|(name, fields)| {
      let span = Span::new(
        name.span.start,
        fields.last().map(|f| f.span().end).unwrap_or(name.span.end),
      );

      TypeVariant { name, fields, span }
    })
  }

  /// Parse an effect operation
  fn effect_operation_parser(
    &self,
  ) -> impl ChumskyParser<Token, EffectOperation, Error = Simple<Token>> {
    let name = self.identifier_parser();
    let input_type = self
      .token_kind(TokenKind::Colon)
      .ignore_then(self.type_expr_parser());
    let output_type = self
      .token_kind(TokenKind::Arrow)
      .ignore_then(self.type_expr_parser());

    name
      .then(input_type)
      .then(output_type)
      .map(|((name, input_type), output_type)| {
        let span = Span::new(name.span.start, output_type.span().end);

        EffectOperation {
          name,
          input_type,
          output_type,
          span,
        }
      })
  }

  /// Parse an expression
  fn expression_parser(&self) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> {
    recursive(|expr| {
      // Primary expressions (identifiers, literals)
      let primary = self
        .identifier_parser()
        .map(|id| Expression::Variable {
          identifier: id.clone(),
          span: id.span,
          type_annotation: None,
        })
        .or(self.literal_parser());

      // Combine primary expressions first
      let atom = primary;

      // Function application
      let application = atom
        .then(
          self
            .token_kind(TokenKind::LeftParen)
            .ignore_then(
              expr
                .clone()
                .separated_by(self.token_kind(TokenKind::Comma))
                .collect::<Vec<Expression>>(),
            )
            .then_ignore(self.token_kind(TokenKind::RightParen))
            .or_not()
            .map(|args| args.unwrap_or_else(Vec::new)),
        )
        .map(|(function, args)| {
          let span = Span::new(
            function.span().start,
            args
              .last()
              .map(|arg| arg.span().end)
              .unwrap_or(function.span().end),
          );

          Expression::Application {
            function: Box::new(function),
            arguments: args,
            span,
            type_annotation: None,
          }
        });

      // Unary operations
      let unary = self
        .token_kind(TokenKind::Minus)
        .or(self.token_kind(TokenKind::Not))
        .then(expr.clone())
        .map(|(op_token, operand)| {
          let operator = match op_token.kind {
            TokenKind::Minus => UnaryOperator::Neg,
            TokenKind::Not => UnaryOperator::Not,
            _ => unreachable!(),
          };

          Expression::UnaryOp {
            operator,
            operand: Box::new(operand.clone()),
            span: Span::new(op_token.start, operand.span().end),
            type_annotation: None,
          }
        });

      // Binary operations
      let binary = self
        .identifier_parser()
        .map(|id| Expression::Variable {
          identifier: id.clone(),
          span: id.span,
          type_annotation: None,
        })
        .or(self.literal_parser())
        .then(choice((
          self.token_kind(TokenKind::Plus),
          self.token_kind(TokenKind::Minus),
          self.token_kind(TokenKind::Star),
          self.token_kind(TokenKind::Slash),
          self.token_kind(TokenKind::Equal),
          self.token_kind(TokenKind::NotEqual),
          self.token_kind(TokenKind::Less),
          self.token_kind(TokenKind::LessEqual),
          self.token_kind(TokenKind::Greater),
          self.token_kind(TokenKind::GreaterEqual),
          self.token_kind(TokenKind::And),
          self.token_kind(TokenKind::Or),
        )))
        .then(expr.clone())
        .map(|((left, op_token), right)| {
          let operator = match op_token.kind {
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
            _ => unreachable!(),
          };

          Expression::BinaryOp {
            operator,
            left: Box::new(left.clone()),
            right: Box::new(right.clone()),
            span: Span::new(left.span().start, right.span().end),
            type_annotation: None,
          }
        });

      // Return the final expression parser
      self
        .identifier_parser()
        .map(|id| Expression::Variable {
          identifier: id.clone(),
          span: id.span,
          type_annotation: None,
        })
        .or(self.literal_parser())
        .or(application)
        .or(unary)
        .or(binary)
    })
  }

  /// Parse a literal
  fn literal_parser(&self) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> {
    choice((
      self.token_kind(TokenKind::Integer).map(|token| {
        let value = token.text.parse::<i64>().unwrap_or(0);
        Expression::Literal {
          literal: Literal::Integer {
            value,
            span: Span::new(token.start, token.end),
          },
          span: Span::new(token.start, token.end),
          type_annotation: None,
        }
      }),
      self.token_kind(TokenKind::Float).map(|token| {
        let value = token.text.parse::<f64>().unwrap_or(0.0);
        Expression::Literal {
          literal: Literal::Float {
            value,
            span: Span::new(token.start, token.end),
          },
          span: Span::new(token.start, token.end),
          type_annotation: None,
        }
      }),
      self.token_kind(TokenKind::String).map(|token| {
        let value = token.text.clone();
        Expression::Literal {
          literal: Literal::String {
            value,
            span: Span::new(token.start, token.end),
          },
          span: Span::new(token.start, token.end),
          type_annotation: None,
        }
      }),
      self
        .token_kind(TokenKind::True)
        .map(|token| Expression::Literal {
          literal: Literal::Boolean {
            value: true,
            span: Span::new(token.start, token.end),
          },
          span: Span::new(token.start, token.end),
          type_annotation: None,
        }),
      self
        .token_kind(TokenKind::False)
        .map(|token| Expression::Literal {
          literal: Literal::Boolean {
            value: false,
            span: Span::new(token.start, token.end),
          },
          span: Span::new(token.start, token.end),
          type_annotation: None,
        }),
    ))
  }

  /// Parse a pattern
  fn pattern_parser(&self) -> impl ChumskyParser<Token, Pattern, Error = Simple<Token>> {
    recursive(|pattern| {
      self
        .identifier_parser()
        .map(|id| Pattern::Variable {
          identifier: id.clone(),
          span: id.span,
        })
        .or(
          self
            .token_kind(TokenKind::Underscore)
            .map(|token| Pattern::Wildcard {
              span: Span::new(token.start, token.end),
            }),
        )
        .or(
          self
            .identifier_parser()
            .then(
              self
                .token_kind(TokenKind::LeftParen)
                .ignore_then(
                  pattern
                    .clone()
                    .separated_by(self.token_kind(TokenKind::Comma))
                    .collect::<Vec<Pattern>>(),
                )
                .then_ignore(self.token_kind(TokenKind::RightParen))
                .or_not()
                .map(|args| args.unwrap_or_else(Vec::new)),
            )
            .map(|(name, args)| Pattern::Constructor {
              constructor: name.clone(),
              arguments: args,
              span: name.span,
            }),
        )
        .or(
          pattern
            .clone()
            .then(self.token_kind(TokenKind::As))
            .then(self.identifier_parser())
            .map(|((pattern, _), identifier)| Pattern::As {
              pattern: Box::new(pattern.clone()),
              identifier,
              span: pattern.span(),
            }),
        )
        .or(
          pattern
            .clone()
            .then(self.token_kind(TokenKind::BitwiseOr))
            .then(pattern.clone())
            .map(|((left, _), right)| Pattern::Or {
              left: Box::new(left.clone()),
              right: Box::new(right),
              span: left.span(),
            }),
        )
    })
  }

  /// Parse a match arm
  fn match_arm_parser(&self) -> impl ChumskyParser<Token, MatchArm, Error = Simple<Token>> {
    let pattern = self.pattern_parser();
    let guard = self
      .token_kind(TokenKind::Then)
      .ignore_then(self.expression_parser())
      .or_not();
    let expression = self
      .token_kind(TokenKind::FatArrow)
      .ignore_then(self.expression_parser());

    pattern
      .then(guard)
      .then(expression)
      .map(|((pattern, guard), expression)| {
        let span = Span::new(pattern.span().start, expression.span().end);

        MatchArm {
          pattern,
          guard: guard.map(|g| g),
          expression,
          span,
        }
      })
  }

  /// Parse a handler case
  fn handler_case_parser(&self) -> impl ChumskyParser<Token, HandlerCase, Error = Simple<Token>> {
    let operation = self.identifier_parser();
    let parameters = self.identifier_parser();
    let expression = self
      .token_kind(TokenKind::FatArrow)
      .ignore_then(self.expression_parser());

    operation
      .then(parameters)
      .then(expression)
      .map(|((operation, parameters), expression)| {
        let span = Span::new(operation.span.start, expression.span().end);

        HandlerCase {
          operation,
          pattern: Pattern::Variable {
            identifier: parameters.clone(),
            span: parameters.span,
          },
          expression,
          span,
        }
      })
  }

  /// Parse a type expression
  fn type_expr_parser(&self) -> impl ChumskyParser<Token, TypeExpr, Error = Simple<Token>> {
    recursive(|type_expr| {
      self
        .identifier_parser()
        .map(|name| TypeExpr::Variable {
          name: name.name,
          span: name.span,
        })
        .or(
          self
            .identifier_parser()
            .then(
              self
                .token_kind(TokenKind::LeftBracket)
                .ignore_then(
                  type_expr
                    .clone()
                    .separated_by(self.token_kind(TokenKind::Comma))
                    .collect::<Vec<TypeExpr>>(),
                )
                .then_ignore(self.token_kind(TokenKind::RightBracket))
                .or_not()
                .map(|args| args.unwrap_or_else(Vec::new)),
            )
            .map(|(name, args)| TypeExpr::Constructor {
              name: name.clone(),
              span: name.span,
            }),
        )
        .or(
          self
            .token_kind(TokenKind::LeftBracket)
            .ignore_then(type_expr.clone())
            .then_ignore(self.token_kind(TokenKind::RightBracket))
            .map(|element| TypeExpr::List {
              element_type: Box::new(element.clone()),
              span: element.span(),
            }),
        )
        .or(
          type_expr
            .clone()
            .then(self.token_kind(TokenKind::Arrow))
            .then(type_expr.clone())
            .map(|((input, _), output)| TypeExpr::Function {
              parameter: Box::new(input.clone()),
              return_type: Box::new(output),
              span: input.span(),
            }),
        )
    })
  }
}
