module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let re = Rex.Pcre.re

module Name = struct
  let string                    = "string"
  let block_comment             = "block_comment"
  let line_comment              = "line_comment"
  let single_quotes             = "single-quotes"
  let macro                     = "macro"
  let list_cons                 = "list-cons"
  let let_binding               = "let-binding"
  let lambda                    = "lambda"
  let type_definition           = "type-definition"
  let type_annotation           = "type-annotation"
  let control_keywords          = "control-keywords"
  let other_keywords            = "other-keywords"
  let numeric_literals          = "numeric-literals"
  let operators                 = "operators"
  let identifier_constructor    = "identifier-constructor"
  let identifier_lower          = "identifier-lower"
  let let_rec                   = "let-rec"
  let let_function              = "let-function"
  let let_constant              = "let-constant"
  let parameter_list            = "parameter-list"
  let identifier_parameter      = "identifier-parameter"
  let parenthesized_definition  = "parenthesized-definition"
  let identifier_variable_decl  = "identifier-variable-decl"
  let struct_type               = "struct-type"
  let sum_type                  = "sum-type"
  let type_alias                = "type-alias"
  let par_type                  = "par-type"
  let type_equals               = "type-equals"
end

let syntax_highlighting =
  let open Textmate in
  {
    syntax_name          = "mligo";
    alt_name              = "caml";
    scope_name           = "source.mligo";
    file_types           = [];
    folding_start_marker = None;
    folding_stop_marker  = None;
    language_features = {
      operators = [
        "not";
        "mod";
        "ediv";
        "+";
        "-";
        "*";
        "/";
        "==";
        "=/=";
        ">=";
        "<=";
        ">";
        "<";
        "#";
        "or";
        "and";
        "^"
      ];
      string_delimiters = [
        "\""
      ];
      comments = {
        line_comment = Some "//";
        block_comment = Some ("(*", "*)");
      };
      brackets = [
        ("{", "}");
        ("[", "]");
        ("(", ")")
      ];
      auto_closing_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
      ];
      surrounding_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
      ];
      syntax_table = [
        ("\n", "> b");
        ("/", ". 12b");
        ("*", ". 23");
        ("\\(", "()1n");
        ("\\)", ")(4n");
      ]
    };
    syntax_patterns = [
      Name.string;
      Name.single_quotes;
      Name.line_comment;
      Name.block_comment;
      Name.macro;
      Name.list_cons;
      Name.let_binding;
      Name.lambda;
      Name.type_definition;
      Name.type_annotation; 
      Name.control_keywords;
      Name.other_keywords;
      Name.numeric_literals;
      Name.operators;
      Name.identifier_constructor;
      Name.identifier_lower
    ];
    repository           = [
      Helpers.macro;
      {
        name = Name.let_binding;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\b(let)\\b", Some Function)];
          end_ = [("(\\=)", Some Operator)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.let_rec;
            Name.let_function;
            Name.let_constant;
          ]
        }
      };
      {
        name = Name.let_rec;
        kind = Match {
          match_ = re "\\b(rec)\\b";
          captures = [];
          match_name = Some StorageClass
        }
      };
      {
        name = Name.let_function;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\G\\s*([a-zA-Z_]\\w*)\\b(?=\\s*\\()", Some Function)];
          end_ = [("(?=\\=)", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.parameter_list;
            Name.type_annotation
          ]
        }
      };
      {
        name = Name.parameter_list;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\(", Some Operator)];
          end_ = [("\\)", Some Operator)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.identifier_parameter;
            Name.type_annotation
          ]
        }
      };
      {
        name = Name.parenthesized_definition;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\(", Some Operator)];
          end_ = [("\\)", Some Operator)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.identifier_variable_decl;
            Name.type_annotation
          ]
        }
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(:)\\s*", Some Type)];
          end_ = [("(?:[;|]|(?=[)=}])|$)", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.par_type;
            Name.type_equals;
          ]
        }
      };
      { name = Name.par_type;
        kind = Begin_end {
            meta_name = None;
            begin_ = [("\\(", None)];
            end_ = [("\\)", None)];
            patterns = [
              Name.line_comment;
              Name.block_comment;
              Name.par_type;
              Name.type_equals
            ]
          };
      };
      { name = Name.type_equals;
        kind = Match {
            match_ = re "([^=()|;}/]+)";
            match_name = None;
            captures = [
              (1, Type)
            ]
          }
      };
      {
        name = Name.let_constant;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\G", None)];
          end_ = [("(?=\\=)", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.type_annotation;
            Name.parenthesized_definition;
            Name.identifier_variable_decl
          ]
        }
      };
      {
        name = Name.lambda;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\b(fun)\\b", Some Statement)];
          end_ = [("(->)", Some Operator)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.parameter_list
          ]
        }
      };
      {
        name = Name.type_definition;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b(type)", Some Type);
            ("\\s+([a-zA-Z_]\\w*)\\b", Some Identifier)];
          end_ = [("(?=(?:\\blet\\b|\\btype\\b|^\\s*#\\w+))", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.struct_type;
            Name.sum_type;
            Name.type_alias
          ]
        }
      };
      {
        name = Name.struct_type;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\{", None)];
          end_ = [("\\}", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.identifier_variable_decl;
            Name.type_annotation
          ]
        }
      };
      {
        name = Name.sum_type;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b([A-Z]\\w*)", Some Label);
            ("\\s+(of)?", Some Statement);
          ];
          end_ = [("(\\||(?=\\blet\\b|\\btype\\b|^\\s*#\\w+))", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.par_type;
            Name.type_equals
          ]
        }
      };
      {
        name = Name.type_alias;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\G\\s*=\\s*(?=[(a-z])", None)];
          end_ = [("(?=\\blet\\b|\\btype\\b|^\\s*#\\w+)", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.par_type;
            Name.type_equals
          ]
        }
      }]
      @
      Helpers.string
      @
      [{
        name = Name.single_quotes;
        kind = Begin_end {
          meta_name = Some String;
          begin_ = [("\\'", None)];
          end_ = [("\\'", None)];
          patterns = []
        }
      }]
      @
      Helpers.ocaml_comment     
      @
      [{
        name = Name.list_cons;
        kind = Match {
          match_name = Some Operator;
          match_ = re "::";
          captures = []
        }
      };
      {
        name = Name.control_keywords;
        kind = Match {
          match_name = Some Conditional;
          match_ = re "\\b(match|with|if|then|else|assert|failwith|begin|end)\\b";
          captures = []
        }
      };
      {
        name = Name.other_keywords;
        kind = Match {
          match_name = Some Statement;
          match_ = re "\\b(in)\\b";
          captures = []
        }
      };
      Helpers.numeric_literals;
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = re "([-+*/])";
          captures = []
        }
      };
      {
        name = Name.identifier_lower;
        kind = Match {
          match_name = None;
          match_ = re "\\b([a-z_]\\w*)\\b";
          captures = [
            (1, Identifier)
          ]
        }
      };
      {
        name = Name.identifier_constructor;
        kind = Match {
          match_name = None;
          match_ = re "\\b([A-Z]\\w*)\\b";
          captures = [
            (1, Label)
          ]
        }
      };
      {
        name = Name.identifier_parameter;
        kind = Match {
          match_name = None;
          match_ = re "\\b([a-zA-Z_]\\w*)\\b";
          captures = [(1, Identifier)]          
        }
      };
      {
        name = Name.identifier_variable_decl;
        kind = Match {
          match_name = None;
          match_ = re "\\b([a-zA-Z_]\\w*)\\b";
          captures = [
            (1, Identifier)
          ]
        }
      }
    ]
  }