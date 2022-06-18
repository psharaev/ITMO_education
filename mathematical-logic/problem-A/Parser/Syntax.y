{
module Parser.Syntax (parse) where

import Parser.Lexer
import Logic.Expression
}

%name parse
%tokentype { Token }
%error { parseError }

%token VAR          { TOKEN_VARIABLE $$  }
%token OPENED_BRACE { TOKEN_OPENED_BRACE }
%token CLOSED_BRACE { TOKEN_CLOSED_BRACE }
%token IMPLICATION  { TOKEN_IMPLICATION  }
%token OR           { TOKEN_OR           }
%token AND          { TOKEN_AND          }
%token NOT          { TOKEN_NOT          }

%%

Expression : Or_Expression IMPLICATION Expression     { Binary Impl $1 $3 }
           | Or_Expression                            { $1 }

Or_Expression : Or_Expression OR And_Expression       { Binary Or $1 $3 }
              | And_Expression                        { $1 }

And_Expression : And_Expression AND Not_Expression    { Binary And $1 $3 }
               | Not_Expression                       { $1 }

Not_Expression : VAR                                  { Var $1 }
               | NOT Not_Expression                   { Unary Not $2 }
               | OPENED_BRACE Expression CLOSED_BRACE { $2 }

{

parseError :: [Token] -> a
parseError e = error $ "Parse error occured :((((("

}
